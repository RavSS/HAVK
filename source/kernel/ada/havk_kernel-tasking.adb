-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking.adb                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Tasking
WITH
   Refined_State => (Tasking_State => (Tasks, Active_Task, Active_Task_Context,
                                       Enabled, Countdown))
IS
   PROCEDURE Create
     (Task_Name      : IN string;
      Initial_Frames : IN number;
      Error_Status   : OUT error)
   WITH
      Refined_Global => (In_Out => (Tasks, Active_Task, Active_Task_Context,
                                    Paging.Kernel_Page_Layout_State,
                                    SPARK.Heap.Dynamic_Memory,
                                    Memory.Frames.Frame_Allocator_State),
                         Input  => (Enabled,Memory.Kernel_Virtual_Base,
                                    Memory.Kernel_Isolated_Text_Base,
                                    Memory.Kernel_Isolated_Text_Size,
                                    Memory.Kernel_Isolated_Data_Base,
                                    Memory.Kernel_Isolated_Data_Size,
                                    Memory.Kernel_Isolated_BSS_Base,
                                    Memory.Kernel_Isolated_BSS_Size,
                                    UEFI.Bootloader_Arguments,
                                    UEFI.Memory_Map))
   IS
      Error_Check : error;
   BEGIN
      Task_Cleaner; -- Try to remove any dead tasks first.

      FOR
         Task_Index IN Tasks'range
      LOOP
         IF
            NOT Tasks(Task_Index).Present
         THEN
            Tasks(Task_Index).Present := true;
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
               Tasks(Task_Index).Present := false;
               Tasks(Task_Index).Name := (OTHERS => NUL);
               Tasks(Task_Index).Initial_Frames := 0;
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

            IF -- If this is the first task, then set the active task context.
               NOT Enabled AND THEN
               Active_Task_Context = 0
            THEN -- Also configure the pointer to the active task's context.
               Active_Task := Task_Index;
               Active_Task_Context := Get_Task_State_Address(Active_Task);
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
      New_Stack          : Memory.page_address;
      Virtual_Space_Base : address;
   BEGIN
      -- Get enough free frames to hold the task's kernel stack for the first
      -- thread. This is needed to make ISR's functional.
      Memory.Frames.Allocate(New_Stack, Task_Index,
         Frame_Count => Default_Kernel_Stack_Size / Paging.page_size'enum_rep);

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

      -- The the page map (PML4) objects are inside the tasking records (no
      -- accesses/pointers), which means it will have an address that is in the
      -- higher-half. Must be converted to a physical and canonical address.
      Virtual_Space_Base := Memory.Kernel_Virtual_To_Physical
        (Paging.Get_Page_Map_Address(Tasks(Task_Index).Virtual_Space));

      IF -- This shouldn't be true, but it's checked just in case.
         number(Virtual_Space_Base) > 2**47 - 1 OR ELSE
         number(Virtual_Space_Base) /= Memory.Align
           (number(Virtual_Space_Base), Paging.page_size'enum_rep)
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Static page layout is not placed on a " &
            "4-kibibyte boundary.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised", "It will always be page-aligned.");
      END IF;

      -- The context switcher needs these values set in the task's register
      -- state so it can (re)construct an interrupt frame and enter it.
      Tasks(Task_Index).State :=
        (CR3    => Virtual_Space_Base,
         -- Set a custom FLAGS value in the size of RFLAGS. Bit 1 (always set),
         -- bit 2 (even parity), and bit 9 (interrupt flag).
         RFLAGS => 2#0010_0000_0110#,
         RIP    => Instruction_Address,
         RSP    => Intrinsics.general_register(Stack_Address),
         OTHERS => <>); -- Ring 3 CS and SS/DS are covered by the defaults.

      Error_Status := no_error;
   END Prepare_Entry;

   PROCEDURE Remove
     (Task_Index   : IN number;
      Error_Status : OUT error)
   IS
   BEGIN
      IF
         Task_Index NOT IN Tasks'range OR ELSE
         NOT Tasks(Task_Index).Present
      THEN
         Error_Status := index_error;
         RETURN;
      ELSIF
         Task_Index = Active_Task OR ELSE
         Tasks(Task_Index).Alive
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

      -- Now mark it as not present by reinitialising the record object. A
      -- default assignment is not possible due to the page layout type being
      -- a limited type (unassignable).
      Tasks(Task_Index).Present           := false;
      Tasks(Task_Index).Name              := (OTHERS => NUL);
      Tasks(Task_Index).Initial_Frames    := 0;
      Tasks(Task_Index).Physical_Base     := 0;
      Tasks(Task_Index).State             := (OTHERS => <>);
      Tasks(Task_Index).Kernel_Stack      := 0;
      Tasks(Task_Index).Kernel_Stack_Size := 0;
      Tasks(Task_Index).Exit_Code         := 0;
      Tasks(Task_Index).Max_Ticks         := 10;
      Tasks(Task_Index).Heap_End          := 0;

      Error_Status := no_error;
   END Remove;

   PROCEDURE Kill_Active_Task
     (Kill_Code : IN number)
   WITH
      Refined_Post => Tasks(Active_Task).Present
   IS
   BEGIN
      IF
         NOT Tasks(Active_Task).Present
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
        (FOR ALL Tasked OF Tasks =>
            NOT Tasked.Present OR ELSE NOT Tasked.Alive)
      THEN
         RAISE Panic
         WITH
            Source_Location & " - There are zero active tasks remaining.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We cannot continue if all the user tasks failed.");
      END IF;
   END Kill_Active_Task;

   PROCEDURE Yield -- TODO: Make this actually do a context switch.
   IS
   BEGIN
      Countdown := 0;
   END Yield;

   PROCEDURE Schedule
   WITH
      Refined_Global => (In_Out => (Active_Task, Active_Task_Context,
                                    Countdown),
                         Input  => (Tasks, Enabled))
   IS
   BEGIN
      IF
         NOT Enabled
      THEN
         RETURN;
      ELSIF
         NOT Tasks(Active_Task).Present
      THEN
         RAISE Panic
         WITH
            Source_Location & " - The task scheduler has lost track of " &
            "the active task.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised",
            "Cannot happen without external corruption.");
      ELSIF -- A true state-of-the-art scheduler.
         Countdown = 0 OR ELSE
         NOT Tasks(Active_Task).Alive
      THEN
         Round_Robin;
      ELSE
         Countdown := Countdown - 1;
      END IF;
   END Schedule;

   PROCEDURE Round_Robin
   IS
      Old_Task : CONSTANT task_limit := Active_Task;
   BEGIN
      FOR -- Now cycle the task too, similar to how the thread was done.
         Task_Index IN Active_Task .. Tasks'last
      LOOP
         IF
            Tasks(Task_Index).Present AND THEN
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
               Tasks(Task_Index).Present AND THEN
               Tasks(Task_Index).Alive
            THEN
               Active_Task := Task_Index;
               EXIT WHEN true;
            END IF;
         END LOOP;
      END IF;

      IF -- Handle a case where all tasks are dead.
         NOT Tasks(Active_Task).Present OR ELSE
         NOT Tasks(Active_Task).Alive
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Cannot switch tasks; no tasks remaining.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We should always have at least one remaining system task left.");
      END IF;

      -- Set the active task context pointer to the task's current context
      -- state record. Works around SPARK's rules to make assembly code work.
      Active_Task_Context := Get_Task_State_Address(Active_Task);

      -- Reset the countdown.
      Countdown := Tasks(Active_Task).Max_Ticks;
   END Round_Robin;

   PROCEDURE Task_Cleaner
   IS
      Error_Check : error;
   BEGIN
      FOR
         Task_Index IN Tasks'range
      LOOP
         IF
            Tasks(Task_Index).Present AND THEN
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
      END LOOP;
   END Task_Cleaner;

   PROCEDURE Map_Address_Range
     (Task_Index       : IN number;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size_Type   : IN Paging.page_frame_variant := Paging.page_size;
      Present          : IN boolean := true;
      Write_Access     : IN boolean := false;
      User_Access      : IN boolean := false;
      No_Execution     : IN boolean := true)
   IS
   BEGIN
      IF -- TODO: Return a proper error status.
         Task_Index NOT IN Tasks'range OR ELSE
         NOT Tasks(Task_Index).Present
      THEN
         RETURN;
      END IF;

      Paging.Map_Address_Range
        (Layout           => Tasks(Task_Index).Virtual_Space,
         Virtual_Address  => Virtual_Address,
         Physical_Address => Physical_Address,
         Size             => Size,
         Page_Size_Type   => Page_Size_Type,
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
                            Tasks(Task_Index).Present
                         ELSIF Error_Status = attempt_error THEN
                            Task_Index = number'first)
   IS
   BEGIN
      FOR
         Index IN Tasks'range
      LOOP
         IF
            Tasks(Index).Present AND THEN
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
         NOT Tasks(Task_Index).Present
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

   FUNCTION Get_Task_State_Address
     (Task_Index : IN task_limit)
      RETURN Memory.canonical_address
   IS
     (Tasks(Task_Index).State'address)
   WITH
      SPARK_Mode => off; -- Address attribute used.

END HAVK_Kernel.Tasking;