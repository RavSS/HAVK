-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking.adb                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Deallocation,
   HAVK_Kernel.Interrupts;

PACKAGE BODY HAVK_Kernel.Tasking
WITH
   Refined_State => (Tasking_State => (Tasks, Active_Task, Enabled, Countdown))
IS
   -- TODO: Does not go very fast.
   PROCEDURE Create
     (Task_Name      : IN string;
      Initial_Frames : IN number;
      Error_Status   : OUT error)
   WITH
      Refined_Global => (In_Out => (Tasks, Descriptors.TSS,
                                    Paging.Kernel_Page_Layout_State,
                                    SPARK.Heap.Dynamic_Memory,
                                    Memory.Frames.Frame_Allocator_State),
                         Input  => (Enabled, Active_Task,
                                    Memory.Kernel_Virtual_Base,
                                    Memory.Kernel_Isolated_Text_Base,
                                    Memory.Kernel_Isolated_Text_Size,
                                    Memory.Kernel_Isolated_Data_Base,
                                    Memory.Kernel_Isolated_Data_Size,
                                    Memory.Kernel_Isolated_BSS_Base,
                                    Memory.Kernel_Isolated_BSS_Size,
                                    UEFI.Bootloader_Arguments,
                                    UEFI.Memory_Map))
   IS
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object => task_control_block, name => access_task_control_block);

      Error_Check : error;
   BEGIN
      Task_Cleaner; -- Try to remove any dead tasks first.

      FOR
         Task_Index IN Tasks'range
      LOOP
         IF
            Tasks(Task_Index) = NULL
         THEN
            Tasks(Task_Index) := NEW task_control_block;
            Tasks(Task_Index).Name
              (Tasks(Task_Index).Name'first .. Task_Name'length) := Task_Name;
            Tasks(Task_Index).Initial_Frames := Initial_Frames;

            -- Get enough free frames to hold the task's code (i.e. all
            -- loadable segments of an executable ELF object).
            Memory.Frames.Allocate(Tasks(Task_Index).Physical_Base,
               Task_Index, Frame_Count => Initial_Frames);

            IF
               Tasks(Task_Index).Physical_Base =
                  Memory.Frames.Null_Frame_Address
            THEN
               Free(Tasks(Task_Index));
               Error_Status := memory_error;
               RETURN;
            END IF;

            -- Now we prepare the task using the default virtual entry address
            -- and let the subprograms handle a random value for the user stack
            -- (which is their responsibility).
            Prepare_Entry(Task_Index, Virtual_Entry, Error_Check);

            IF
               Error_Check /= no_error
            THEN
               Remove(Task_Index, Error_Check);

               IF
                  Error_Check /= no_error
               THEN
                  RAISE Panic
                  WITH
                     Source_Location & " - Failed to recover from bad task " &
                     "creation.";
                  PRAGMA Annotate(GNATprove, Intentional,
                     "exception might be raised",
                     "Avoid a non-removable task ""leak"" no matter what.");
               END IF;

               Error_Status := memory_error;
               RETURN;
            END IF;

            -- TODO: Move this elsewhere when performance can matter.
            IF -- If this is the first task, then set the TSS's ring 0 RSP.
               NOT Enabled AND THEN
               Descriptors.TSS.RSP_Ring_0 = 0
            THEN
               Descriptors.TSS.RSP_Ring_0 := Tasks(Task_Index).Kernel_Stack;
            END IF;

            -- Finally, map all the isolated sections that need to be present
            -- in the page layout for e.g. interrupt handlers and the system
            -- call entry. These do not need actual ring 3 access.
            -- This is a very cheap attempt at implementing what Linux calls
            -- KPTI or KAISER, just without KASLR.
            Paging.Map_Address_Range
              (Tasks(Task_Index).Virtual_Space,
               Memory.Kernel_Isolated_Text_Base,
               Memory.Kernel_Virtual_To_Physical
                 (Memory.Kernel_Isolated_Text_Base),
               Memory.Kernel_Isolated_Text_Size,
               Write_Access => false,
               No_Execution => false);

            Paging.Map_Address_Range
              (Tasks(Task_Index).Virtual_Space,
               Memory.Kernel_Isolated_Data_Base,
               Memory.Kernel_Virtual_To_Physical
                 (Memory.Kernel_Isolated_Data_Base),
               Memory.Kernel_Isolated_Data_Size,
               Write_Access => true,
               No_Execution => true);

            Paging.Map_Address_Range
              (Tasks(Task_Index).Virtual_Space,
               Memory.Kernel_Isolated_BSS_Base,
               Memory.Kernel_Virtual_To_Physical
                 (Memory.Kernel_Isolated_BSS_Base),
               Memory.Kernel_Isolated_BSS_Size,
               Write_Access => true,
               No_Execution => true);

            Error_Status := no_error;
            RETURN;
         END IF;
      END LOOP;

      Error_Status := attempt_error; -- Scheduler is full.
   END Create;

   PROCEDURE Prepare_Entry
     (Task_Index          : IN task_limit;
      Instruction_Address : IN Memory.canonical_address;
      Error_Status        : OUT error;
      Stack_Address       : IN address := address'last)
   IS
      New_Stack : Memory.page_address;
   BEGIN
      -- Get enough free frames to hold the task's kernel stack for the first
      -- thread. This is needed to make ISR's functional.
      Memory.Frames.Allocate(New_Stack, Task_Index,
         Frame_Count => Default_Kernel_Stack_Size / Paging.Page);

      IF -- We've run out of frames if true.
         New_Stack = Memory.Frames.Null_Frame_Address
      THEN
         Error_Status := memory_error;
         RETURN;
      END IF;

      -- The kernel needs to be able to access and write to the stack.
      Paging.Kernel_Map_Address_Range
        (New_Stack,
         New_Stack,
         Default_Kernel_Stack_Size,
         Write_Access => true);

      -- Now place the actual base/bottom of the kernel stack inside the
      -- task control block. The mask is for an extreme edge case.
      Tasks(Task_Index).Kernel_Stack := New_Stack +
         Memory.page_address(Default_Kernel_Stack_Size) AND 2**47 - 1;
      Tasks(Task_Index).Kernel_Stack_Size :=
         Default_Kernel_Stack_Size;

      -- TODO: Right now, the kernel stacks are identity-mapped to their
      -- physical location. I will change this later on.
      Tasks(Task_Index).Kernel_Stack_Physical :=
         Tasks(Task_Index).Kernel_Stack;

      -- Map the kernel stack. Must not have ring 3 access.
      Paging.Map_Address_Range
        (Tasks(Task_Index).Virtual_Space,
         Tasks(Task_Index).Kernel_Stack -
            address(Tasks(Task_Index).Kernel_Stack_Size),
         Tasks(Task_Index).Kernel_Stack_Physical -
            address(Tasks(Task_Index).Kernel_Stack_Size),
         Tasks(Task_Index).Kernel_Stack_Size,
         Write_Access => true);

      -- Put an interrupt frame on the new task's kernel stack, which will be
      -- used to enter it. The RSP value upon entry will be an invalid value by
      -- default, so the user must set up their own ring 3 stack (unless this
      -- is part of a system call, in which we will be helpful and set their
      -- chosen stack for them). If the interrupt frame will cause an
      -- exception, then it should be because of the RIP or RSP, not the
      -- segment indices.
      DECLARE
         -- We're simulating an interrupt here which is needed if we're
         -- switching into different privilege rings. See the below resource.
         -- READ: https://wiki.osdev.org/Getting_to_Ring_3#Entering_Ring_3
         Task_Kernel_Stack : Interrupts.interrupted_state
         WITH -- Not using the size attribute (`gnatprove`).
            Import  => true, -- This is overlayed from low to high.
            Address => Tasks(Task_Index).Kernel_Stack - 40;
      BEGIN
         PRAGMA Warnings(GNATprove, off, "unused assignment",
            Reason => "We're directly modifying memory here.");

         -- I've used a record for this, but pretend that this is an assembly
         -- routine instead so the interrupt frame logic makes more sense.

         -- First, we "push" the DS index depending on the task's privilege
         -- ring. This will always be (0x20 | 3).
         Task_Kernel_Stack.SS     := Descriptors.DS_Ring_3;

         -- Now we "push" the value of the stack pointer. Since the task's ring
         -- 3 stack is controlled by the task itself, we don't have to set
         -- anything here.
         Task_Kernel_Stack.RSP    := Stack_Address;

         -- "Push" a custom FLAGS value in the size of RFLAGS. Bit 1 (always
         -- set), bit 2 (even parity), and bit 9 (interrupt flag).
         Task_Kernel_Stack.RFLAGS := 2#0010_0000_0110#;

         -- Now we "push" the CS index like we did for the DS index.
         Task_Kernel_Stack.CS     := Descriptors.CS_Ring_3;

         -- Finally, we "push" the RIP. This will be our standard entry
         -- address.
         Task_Kernel_Stack.RIP    := Instruction_Address;

         -- Now that we're done with configuring the interrupt frame state,
         -- store the task's calculated kernel RSP in its state record, so it
         -- can `REX.W IRET` to the user's code. Like above with the address
         -- aspect, the size attribute is not used due to `gnatprove` needing
         -- help with the implementation dependent calculation.
         Tasks(Task_Index).State.RSP :=
            Intrinsics.general_register(Tasks(Task_Index).Kernel_Stack - 40);
      END;

      Error_Status := no_error;
   END Prepare_Entry;

   PROCEDURE Remove
     (Task_Index   : IN number;
      Error_Status : OUT error)
   IS
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object => task_control_block, name => access_task_control_block);
   BEGIN
      IF
         Task_Index NOT IN Tasks'range OR ELSE
         Tasks(Task_Index) = NULL
      THEN
         Error_Status := index_error;
         RETURN;
      ELSIF
         Task_Index = Active_Task
      THEN
         Error_Status := attempt_error;
         RETURN;
      END IF;

      -- First, deallocate the physical frames and deallocate the allocations
      -- made for the paging mechanisms.
      Memory.Frames.Deallocate_All_Owner_Frames(Task_Index);
      Paging.Deallocate_Mappings(Tasks(Task_Index).Virtual_Space);

      -- Remove the virtual address mappings from both the kernel and the task.
      Paging.Kernel_Map_Address_Range
        (Tasks(Task_Index).Kernel_Stack -
            address(Tasks(Task_Index).Kernel_Stack_Size),
         Tasks(Task_Index).Kernel_Stack_Physical -
            address(Tasks(Task_Index).Kernel_Stack_Size),
         Tasks(Task_Index).Kernel_Stack_Size,
         Present => false);

      Paging.Map_Address_Range
        (Tasks(Task_Index).Virtual_Space,
         Tasks(Task_Index).Kernel_Stack -
            address(Tasks(Task_Index).Kernel_Stack_Size),
         Tasks(Task_Index).Kernel_Stack_Physical -
            address(Tasks(Task_Index).Kernel_Stack_Size),
         Tasks(Task_Index).Kernel_Stack_Size,
         Present => false);

      -- Now free the accesses/pointers to the record.
      Free(Tasks(Task_Index));

      Error_Status := no_error;
   END Remove;

   PROCEDURE Kill_Active_Task
     (Kill_Code : IN number)
   WITH
      Refined_Post => Tasks(Active_Task) /= NULL
   IS
   BEGIN
      IF
         Tasks(Active_Task) = NULL
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Active task does not actually exist.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "This should be called more carefully.");
      END IF;

      Tasks(Active_Task).Alive := false;
      Tasks(Active_Task).Exit_Code := Kill_Code;
      Log("Task """ & Tasks(Active_Task).Name & """ was killed. Code: " &
         Image(Kill_Code) & '.', Tag => Tasking_Tag, Warn => Kill_Code /= 0);

      IF
        (FOR ALL Tasked OF Tasks => Tasked = NULL OR ELSE NOT Tasked.Alive)
      THEN
         RAISE Panic
         WITH
            Source_Location & " - There are zero active tasks remaining.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We cannot continue if all the user tasks failed.");
      END IF;
   END Kill_Active_Task;

   PROCEDURE Yield
   IS
   BEGIN
      Countdown := 0;
   END Yield;

   PROCEDURE Schedule
   WITH
      Refined_Global => (In_Out => (Tasks, Active_Task, Countdown,
                                    Descriptors.TSS),
                         Input  => Enabled)
   IS
   BEGIN
      IF
         NOT Enabled
      THEN
         RETURN;
      ELSIF
         Tasks(Active_Task) = NULL
      THEN
         RAISE Panic
         WITH
            Source_Location & " - The task scheduler has lost track of " &
            "the active task.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised",
            "Cannot happen without external corruption.");
      ELSE
         Round_Robin;
      END IF;
   END Schedule;

   PROCEDURE Round_Robin
   IS
   BEGIN
      IF -- A true state-of-the-art scheduler.
         Countdown = 0 OR ELSE
         NOT Tasks(Active_Task).Alive
      THEN
         Switch;
      ELSE
         Countdown := Countdown - 1;
      END IF;
   END Round_Robin;

   PROCEDURE Round_Robin_Cycle
   IS
      Old_Task : CONSTANT task_limit := Active_Task;
   BEGIN
      FOR -- Now cycle the task too, similar to how the thread was done.
         Task_Index IN Active_Task .. Tasks'last
      LOOP
         IF
            Tasks(Task_Index) /= NULL AND THEN
            Tasks(Task_Index).Alive
         THEN
            Active_Task := Task_Index;
            EXIT WHEN Task_Index /= Old_Task;
         END IF;
      END LOOP;

      IF -- If we're at the last living task, then go back to the start.
         Active_Task = Old_Task
      THEN
         FOR
            Task_Index IN Tasks'range
         LOOP
            IF
               Tasks(Task_Index) /= NULL AND THEN
               Tasks(Task_Index).Alive
            THEN
               Active_Task := Task_Index;
               EXIT WHEN true;
            END IF;
         END LOOP;
      END IF;

      IF -- Handle a case where all tasks are dead.
         Tasks(Active_Task) = NULL OR ELSE
         NOT Tasks(Active_Task).Alive
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Cannot switch tasks; no tasks remaining.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We should always have at least one remaining system task left.");
      END IF;

      -- Set the new kernel stack.
      Descriptors.TSS.RSP_Ring_0 := Tasks(Active_Task).Kernel_Stack;

      -- Reset the countdown.
      Countdown := Tasks(Active_Task).Max_Ticks;
   END Round_Robin_Cycle;

   PROCEDURE Task_Cleaner
   IS
      Error_Check : error;
   BEGIN
      FOR
         Task_Index IN Tasks'range
      LOOP
         IF
            Tasks(Task_Index) /= NULL
         THEN
            IF
               NOT Tasks(Task_Index).Alive
            THEN
               Remove(Task_Index, Error_Check);

               IF
                  Error_Check /= no_error
               THEN
                  RAISE Panic
                  WITH
                     Source_Location & " - Could not remove dead task " &
                     Image(Task_Index) & '.';
                  PRAGMA Annotate(GNATprove, Intentional,
                     "exception might be raised",
                     "We need to be able to remove all dead tasks for now.");
               END IF;
            END IF;
         END IF;
      END LOOP;
   END Task_Cleaner;

   PROCEDURE Map_Address_Range
     (Task_Index       : IN number;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size        : IN Paging.page_frame_variant := Paging.Page;
      Present          : IN boolean :=  true;
      Write_Access     : IN boolean := false;
      User_Access      : IN boolean := false;
      No_Execution     : IN boolean :=  true)
   IS
   BEGIN
      IF -- TODO: Return a proper error status.
         Task_Index NOT IN Tasks'range OR ELSE
         Tasks(Task_Index) = NULL
      THEN
         RETURN;
      END IF;

      Paging.Map_Address_Range
        (Layout           => Tasks(Task_Index).Virtual_Space,
         Virtual_Address  => Virtual_Address,
         Physical_Address => Physical_Address,
         Size             => Size,
         Page_Size        => Page_Size,
         Present          => Present,
         Write_Access     => Write_Access,
         User_Access      => User_Access,
         No_Execution     => No_Execution);
   END Map_Address_Range;

   PROCEDURE Get_Task_Index
     (Task_Name    : IN string;
      Task_Index   : OUT number;
      Error_Status : OUT error)
   WITH
      Refined_Global => (Input => Tasks),
      Refined_Post   => (IF Error_Status = no_error THEN
                            Task_Index IN task_limit'range AND THEN
                            Tasks(Task_Index) /= NULL
                         ELSIF Error_Status = attempt_error THEN
                            Task_Index = number'first)
   IS
   BEGIN
      FOR
         Index IN Tasks'range
      LOOP
         IF
            Tasks(Index) /= NULL AND THEN
            Tasks(Index).Name
              (Tasks(Index).Name'first .. Task_Name'length) = Task_Name
         THEN
            Task_Index   := Index;
            Error_Status := no_error;
            RETURN;
         END IF;
      END LOOP;

      Task_Index   := number'first;
      Error_Status := attempt_error;
   END Get_Task_Index;

   FUNCTION Get_Task_Status
     (Task_Index : IN number)
      RETURN task_status
   WITH
      Refined_Global => (Input => (Enabled, Tasks))
   IS
   BEGIN
      IF
         NOT Enabled                   OR ELSE
         Task_Index NOT IN Tasks'range OR ELSE
         Tasks(Task_Index) = NULL
      THEN
         RETURN (Index => 0, OTHERS => <>);
      END IF;

      RETURN
        (Index => Task_Index,
         Alive => Tasks(Task_Index).Alive,
         Name  => Tasks(Task_Index).Name);
   END Get_Task_Status;

   FUNCTION Get_Active_Task_Index
      RETURN number
   IS
     (Active_Task);

   FUNCTION Get_Active_Task_State
      RETURN Memory.canonical_address
   IS
     (Tasks(Active_Task).State'address)
   WITH
      SPARK_Mode => off; -- Address attribute used.

   FUNCTION Get_Active_Task_CR3
      RETURN Memory.page_address
   IS
      CR3_Value : CONSTANT address :=
         Paging.Get_Page_Map_Address(Tasks(Active_Task).Virtual_Space);
   BEGIN
      -- The address of the PML4 must be a physical address. I've
      -- identity-mapped the kernel's heap (no physical-virtual difference), so
      -- this assumption holds until that changes for whatever reason, as the
      -- task control blocks are allocated on the kernel's heap.
      PRAGMA Assume(CR3_Value <= address(128 * GiB),
         "The page map's base address must be a physical address.");

      RETURN CR3_Value;
   END Get_Active_Task_CR3;

END HAVK_Kernel.Tasking;
