-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking.adb                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Tasking
WITH
   Refined_State => (Tasking_State => (Tasks, Active_Task, Enabled))
IS
   -- TODO: Does not replace dead/finished tasks or catch them.
   -- TODO: Does not go very fast or very safely.
   PROCEDURE Create
     (Task_Name  : IN string;
      Code_Size  : IN number;
      Heap_Size  : IN number;
      Stack_Size : IN number := 16 * KiB)
   WITH
      Refined_Global => (In_Out => (Tasks, Descriptors.TSS,
                                    Paging.Kernel_Paging_Layout,
                                    Memory.Manager.Frame_Allocator_State),
                         Input  => (Memory.Memory_State, Enabled,
                                    Memory.Kernel_Virtual_Base,
                                    Memory.Kernel_Isolated_Text_Base,
                                    Memory.Kernel_Isolated_Text_Size,
                                    Memory.Kernel_Isolated_Data_Base,
                                    Memory.Kernel_Isolated_Data_Size,
                                    Memory.Kernel_Isolated_BSS_Base,
                                    Memory.Kernel_Isolated_BSS_Size))
   IS
      TYPE pointer IS ACCESS void; -- Need a generic pointer type for now.

      PROCEDURE Allocate IS NEW Memory.Manager.Allocator
        (generic_data    => void,
         generic_pointer => pointer);

      PROCEDURE Allocate IS NEW Memory.Manager.Allocator
        (generic_data    => Memory.x86_stack,
         generic_pointer => Memory.access_x86_stack);

      FUNCTION To_Address
        (Entry_Access : IN pointer)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;

      -- TODO: This crashes `gnatprove` with the following message:
      -- "unregistered entity havk_kernel__paging__page_layout"
      -- I've moved it to a non-analysed procedure for now.
      -- Tasks(Task_Index).Virtual_Space := NEW Paging.page_layout;
      PROCEDURE Create_Virtual_Space
        (Current_Page_Layout : IN OUT Paging.access_page_layout)
      WITH
         Post => Current_Page_Layout /= NULL;

      PROCEDURE Create_Virtual_Space
        (Current_Page_Layout : IN OUT Paging.access_page_layout)
      WITH
         SPARK_Mode => off -- This crashes `gnatprove` for GNAT CE 2019.
      IS
      BEGIN
         Current_Page_Layout := NEW Paging.page_layout;
      END Create_Virtual_Space;

      Total_Size     : CONSTANT number := Code_Size + (Stack_Size + Heap_Size);
      Error_Check    : error;
      New_Stack      : Memory.access_x86_stack;
      Physical_Entry : pointer;
      Padded_Name    : string(1 .. 64) := (OTHERS => character'val(0));
   BEGIN
      Padded_Name(Task_Name'range) := Task_Name;
      Intrinsics.Disable_Interrupts;

      FOR
         Task_Index IN Tasks'range
      LOOP
         IF
            Tasks(Task_Index) = NULL
         THEN
            Tasks(Task_Index) := NEW task_control_block'
              (Name           => Padded_Name,
               Physical_Space => Memory.Manager.New_Space,
               State          => (SS => Descriptors.DS_Ring_3, OTHERS => 0),
               OTHERS         => <>);

            -- Create a memory space for the task.
            Memory.Manager.Initialise_Space(Tasks(Task_Index).Physical_Space,
               Total_Size, Error_Check);

            IF
               Error_Check = no_error
            THEN
               Paging.Kernel_Paging_Layout.Map_Address_Range
                 (Memory.Manager.Get_Base(Tasks(Task_Index).Physical_Space),
                  Memory.Manager.Get_Base(Tasks(Task_Index).Physical_Space),
                  number(Memory.Manager.Get_Base
                    (Tasks(Task_Index).Physical_Space)) + Total_Size,
                  Write_Access => true);
            ELSE
               RETURN;
            END IF;

            -- All (ring 3) tasks need a kernel stack for ISR functionality.
            Tasks(Task_Index).Kernel_Stack :=
               Memory.Allocate_System_Stack(Default_Kernel_Stack_Size);
            Tasks(Task_Index).Kernel_Stack_Size := Default_Kernel_Stack_Size;

            -- The kernel needs to be able to access and write to it.
            Paging.Kernel_Paging_Layout.Map_Address_Range
              (Tasks(Task_Index).Kernel_Stack -
                  address(Default_Kernel_Stack_Size),
               Tasks(Task_Index).Kernel_Stack -
                  address(Default_Kernel_Stack_Size),
               Default_Kernel_Stack_Size,
               Write_Access => true);

            -- Adjust the task stack now that we have a memory space for it.
            -- Allocate it first. It's allocated on a physical page boundary.
            Allocate(Tasks(Task_Index).Physical_Space, New_Stack, Stack_Size,
               Alignment => Paging.Page);

            -- Modify the stack from the allocated block's end due to how x86
            -- stacks progress in usage and then prepare it.
            Prepare_Task_Stack(Task_Index, Memory.To_Address(New_Stack) +
               address(Stack_Size));
            Tasks(Task_Index).Stack_Size := Stack_Size;

            -- TODO: Move this elsewhere when performance can matter.
            IF -- If this is the first task, then set the TSS's ring 0 RSP.
               NOT Enabled AND THEN
               Descriptors.TSS.RSP_Ring_0 = 0
            THEN
               Descriptors.TSS.RSP_Ring_0 := Tasks(Task_Index).Kernel_Stack;
            END IF;

            -- Prepare to create a special region of memory that is executable.
            -- We can modify the NX properties of inner regions later on, but
            -- for now, pretend that it's all a static region with no data
            -- section and does everything on the predefined task stack.
            Allocate(Tasks(Task_Index).Physical_Space, Physical_Entry,
               Code_Size, Alignment => Paging.Page);

            IF
               To_Address(Physical_Entry) =
                  Memory.Manager.Null_Allocation_Address
            THEN
               RETURN; -- TODO: Add better error handling.
            ELSE
               Tasks(Task_Index).Physical_Entry := To_Address(Physical_Entry);
            END IF;

            -- Each task needs its own virtual memory layout.
            Create_Virtual_Space(Tasks(Task_Index).Virtual_Space);

            -- Map the given code size itself. User executable code.
            Tasks(Task_Index).Virtual_Space.Map_Address_Range
              (Virtual_Entry,
               Tasks(Task_Index).Physical_Entry,
               Code_Size,
               User_Access  => true,
               No_Execution => false);

            -- Map its stack by making the physical stack space match the
            -- virtual stack space. Non-executable stack, of course.
            Tasks(Task_Index).Virtual_Space.Map_Address_Range
              (address(Memory.Align(number(Virtual_Stack_Base) -
                  Tasks(Task_Index).Stack_Size, Paging.Page,
                  Round_Up => true)),
               address(Memory.Align
                 (number(Tasks(Task_Index).Physical_Stack_Base) -
                  Tasks(Task_Index).Stack_Size, Paging.Page,
                  Round_Up => true)),
               Tasks(Task_Index).Stack_Size,
               Write_Access => true,
               User_Access  => true);

            -- Map the kernel stack. Must not have ring 3 access.
            Tasks(Task_Index).Virtual_Space.Map_Address_Range
              (address(Memory.Align(number(Tasks(Task_Index).Kernel_Stack) -
                  Tasks(Task_Index).Kernel_Stack_Size, Paging.Page,
                  Round_Up => true)),
               address(Memory.Align(number(Tasks(Task_Index).Kernel_Stack) -
                  Tasks(Task_Index).Kernel_Stack_Size, Paging.Page,
                  Round_Up => true)),
               Tasks(Task_Index).Kernel_Stack_Size,
               Write_Access => true);

            -- Finally, map all the isolated sections that need to be present
            -- in the page layout for e.g. interrupt handlers and the system
            -- call entry. These do not need actual ring 3 access.
            -- This is a very cheap attempt at implementing what Linux calls
            -- KPTI or KAISER, just without KASLR.
            Tasks(Task_Index).Virtual_Space.Map_Address_Range
              (Memory.Kernel_Isolated_Text_Base,
               Memory.Kernel_Virtual_To_Physical
                 (Memory.Kernel_Isolated_Text_Base),
               Memory.Kernel_Isolated_Text_Size,
               Write_Access => false,
               No_Execution => false);

            Tasks(Task_Index).Virtual_Space.Map_Address_Range
              (Memory.Kernel_Isolated_Data_Base,
               Memory.Kernel_Virtual_To_Physical
                 (Memory.Kernel_Isolated_Data_Base),
               Memory.Kernel_Isolated_Data_Size,
               Write_Access => true,
               No_Execution => true);

            Tasks(Task_Index).Virtual_Space.Map_Address_Range
              (Memory.Kernel_Isolated_BSS_Base,
               Memory.Kernel_Virtual_To_Physical
                 (Memory.Kernel_Isolated_BSS_Base),
               Memory.Kernel_Isolated_BSS_Size,
               Write_Access => true,
               No_Execution => true);

            EXIT WHEN true;
         END IF;
      END LOOP;

      Intrinsics.Enable_Interrupts;
   END Create;

   PROCEDURE Prepare_Task_Stack
     (Task_Index : IN task_limit;
      Stack_Base : IN address)
   IS
      -- We're simulating an interrupt here which is needed if we're switching
      -- into different privilege rings. See the below resource.
      -- READ: https://wiki.osdev.org/Getting_to_Ring_3#Entering_Ring_3

      -- I'm not using the "interrupted_state" record type found in
      -- "HAVK_Kernel.Interrupts", just so we have more freedom in placing
      -- things on the stack here. We're storing 8-byte addresses on it. First
      -- index represents the last/newest value placed on the stack.
      Task_Stack : quad_words(1 .. 5)
      WITH
         Import  => true, -- This is overlayed from low to high address-wise.
         Address => Stack_Base - ((address'size / 8) * 5);
   BEGIN
      PRAGMA Warnings(GNATprove, off, "unused assignment",
         Reason => "It will be used later during a context switch.");

      -- First, we push the DS index depending on the task's privilege ring.
      Task_Stack(5) := Descriptors.DS_Ring_3;

      -- Now we push the value of the stack pointer that the task will begin
      -- with, which is the standard virtual address that we define elsewhere.
      Task_Stack(4) := number(Virtual_Stack_Base);

      -- Push a custom FLAGS value in the size of RFLAGS.
      -- Bit 1 (always set), bit 2 (even parity), and bit 9 (interrupt flag).
      Task_Stack(3) := 2#0010_0000_0110#;

      -- Now we "push" the CS index like we did for the DS index.
      Task_Stack(2) := Descriptors.CS_Ring_3;

      -- Finally, we push the RIP. This will be our standard entry address.
      Task_Stack(1) := number(Virtual_Entry);

      -- Now store the virtual stack pointer with the appropriate subtraction
      -- so we're ready to `REX.W IRET` with it in the RSP register.
      Tasks(Task_Index).Stack := -- 8-byte addresses and values.
         (Virtual_Stack_Base - (Task_Stack'length * 8)) + 1;
      Tasks(Task_Index).Physical_Stack_Base := Stack_Base;
   END Prepare_Task_Stack;

   PROCEDURE Schedule
   WITH
      Refined_Global => (In_Out => (Tasks, Active_Task, Descriptors.TSS),
                         Input  => (Enabled, APIC.Timer.Ticks))
   IS
   BEGIN
      IF
         Enabled
      THEN
         IF
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
      END IF;
   END Schedule;

   PROCEDURE Round_Robin
     (Yield : IN boolean := false)
   IS
   BEGIN
      IF -- A true state-of-the-art scheduler.
         Yield OR ELSE APIC.Timer.Ticks MOD Tasks(Active_Task).Max_Ticks = 0
      THEN
         Switch(Tasks(Active_Task).State);
      END IF;
   END Round_Robin;

   PROCEDURE Store_Task
     (Task_Stack : IN address)
   IS
   BEGIN
      Tasks(Active_Task).Stack := Task_Stack; -- Store the task's stack.

      IF -- Increment the task counter to the next task or loop back around.
         Active_Task + 1 IN Tasks'range AND THEN
         Tasks(Active_Task + 1) /= NULL
      THEN
         Active_Task := Active_Task + 1;
      ELSE
         Active_Task := Tasks'first;

         WHILE -- In case the first task is null, but others somewhere aren't.
            Tasks(Active_Task) = NULL
         LOOP
            Active_Task := (IF Active_Task + 1 IN Tasks'range THEN
               Active_Task + 1 ELSE Tasks'first);
         END LOOP;
      END IF;

      -- Set the new kernel stack.
      Descriptors.TSS.RSP_Ring_0 := Tasks(Active_Task).Kernel_Stack;
   END Store_Task;

   FUNCTION Get_Task_Physical_Entry
     (Task_Name : IN string)
      RETURN address
   WITH
      Refined_Global => (Input => Tasks)
   IS
   BEGIN
      FOR
         Tasked OF Tasks
      LOOP
         IF
            Tasked /= NULL AND THEN Tasked.Name(Task_Name'range) = Task_Name
         THEN
            RETURN Tasked.Physical_Entry;
         END IF;
      END LOOP;

      RETURN address'first;
   END Get_Task_Physical_Entry;

   FUNCTION Get_Active_Task_Name
      RETURN string
   IS
   (
      IF
         Enabled AND THEN Tasks(Active_Task) /= NULL
      THEN
         Tasks(Active_Task).Name
      ELSE
        (1 .. 64 => character'val(0))
   )
   WITH
      Refined_Global => (Input => (Enabled, Tasks, Active_Task));

   FUNCTION Get_Task_Stack
      RETURN address
   IS
     (Tasks(Active_Task).Stack);

   FUNCTION Get_Task_State
      RETURN address
   IS
     (Tasks(Active_Task).State'address)
   WITH
      SPARK_Mode => off; -- Address attribute used.

   FUNCTION Get_Task_CR3
      RETURN address
   IS
     (Tasks(Active_Task).Virtual_Space.ALL.L4'address)
   WITH
      SPARK_Mode => off; -- Address attribute used.

END HAVK_Kernel.Tasking;
