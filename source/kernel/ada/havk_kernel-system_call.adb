-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Memory,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.System_Call
IS
   PROCEDURE Null_Operation_Call
     (RSI : IN register;  -- Argument 1.
      RDX : IN register;  -- Argument 2.
      R8  : IN register;  -- Argument 3.
      R9  : IN register;  -- Argument 4.
      R10 : IN register;  -- Argument 5.
      RIP : IN register;  -- Call address.
      RAX : OUT register) -- An error status returned by us.
   IS
      Active_Thread_Index : CONSTANT number := Tasking.Get_Active_Thread_Index;
   BEGIN
      Log("Task """ & Tasking.Get_Active_Task_Name &
         """ (thread " & Image(Active_Thread_Index) &
         ") called the null operation. " &
         "Argument 1: 0x" & Image(address(RSI)) & " - " &
         "Argument 2: 0x" & Image(address(RDX)) & " - " &
         "Argument 3: 0x" & Image(address(R8)) & " - " &
         "Argument 4: 0x" & Image(address(R9)) & " - " &
         "Argument 5: 0x" & Image(address(R10)) & " - " &
         "Call address: 0x" & Image(address(RIP)) & '.',
         Tag => System_Call_Tag);
      RAX := no_error'enum_rep;
   END Null_Operation_Call;

   PROCEDURE Exit_Thread_Operation_Call
     (RSI : IN register) -- Exit code returned by the task's thread.
   IS
   BEGIN
      Tasking.Kill_Active_Thread(number(RSI));
   END Exit_Thread_Operation_Call;

   PROCEDURE Create_Thread_Operation_Call
     (RSI : IN register;  -- The entry address. Must be canonical.
      RDX : IN register;  -- The stack address. Must be canonical.
      RAX : OUT register) -- An error status returned by us.
   IS
      Active_Task : CONSTANT number := Tasking.Get_Active_Task_Index;
      Error_Check : error;
   BEGIN
      Log("Creating new thread for task " & Image(Active_Task) & '.',
         Tag => System_Call_Tag);

      IF
         RSI <= register(natural'last) AND THEN
         RDX <= register(natural'last)
      THEN
         Tasking.Create_Thread(Active_Task, Memory.canonical_address(RSI),
            Error_Check, Thread_Stack => Memory.canonical_address(RDX),
            Living => true);
         RAX := Error_Check'enum_rep;
      ELSE
         RAX := memory_error'enum_rep;
      END IF;
   END Create_Thread_Operation_Call;

   PROCEDURE Framebuffer_Access_Operation_Call
     (RSI : OUT register; -- Returned framebuffer base address.
      RDX : OUT register; -- Returned byte size of the framebuffer.
      R8  : OUT register; -- 63:32 = Width. 31:0 = Height.
      R9  : OUT register; -- The amount of pixels in a scanline.
      R10 : OUT register; -- The pixel format.
      RAX : OUT register) -- An error status returned by us.
   IS
      Active_Task : CONSTANT number := Tasking.Get_Active_Task_Index;
   BEGIN
      Tasking.Map_Address_Range
        (Active_Task,
         Memory.user_framebuffer_address'first,
         UEFI.Bootloader_Arguments.Framebuffer_Address,
         UEFI.Bootloader_Arguments.Framebuffer_Size,
         Write_Access => true,
         User_Access  => true);

      RSI := register(Memory.user_framebuffer_address'first);
      RDX := register(UEFI.Bootloader_Arguments.Framebuffer_Size);
      R8  :=
         Shift_Left(register(UEFI.Bootloader_Arguments.Horizontal_Resolution),
            32) OR register(UEFI.Bootloader_Arguments.Vertical_Resolution);
      R9  := register(UEFI.Bootloader_Arguments.Pixels_Per_Scanline);
      R10 := UEFI.Bootloader_Arguments.Pixel_Format'enum_rep;
      RAX := no_error'enum_rep;
   END Framebuffer_Access_Operation_Call;

END HAVK_Kernel.System_Call;
