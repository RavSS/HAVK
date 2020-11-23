-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Tasking,
   HAVK_Kernel.Tasking.ELF,
   HAVK_Kernel.Tasking.IPC,
   HAVK_Kernel.Tasking.Buffer,
   HAVK_Kernel.Tasking.Memory;

PACKAGE BODY HAVK_Kernel.System_Call
IS
   PROCEDURE Null_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : IN Intrinsics.general_register;
      Argument_3   : IN Intrinsics.general_register;
      Argument_4   : IN Intrinsics.general_register;
      Argument_5   : IN Intrinsics.general_register;
      Call_Address : IN Memory.canonical_address;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task_Index  : CONSTANT number := Tasking.Get_Active_Task_Index;
      Active_Task_Status : CONSTANT Tasking.task_status :=
         Tasking.Get_Task_Status(Active_Task_Index);
   BEGIN
      Log("Task """ & Active_Task_Status.Name &
         """ called the null operation. " &
         "Argument 1: 0x" & Image(address(Argument_1)) & " - " &
         "Argument 2: 0x" & Image(address(Argument_2)) & " - " &
         "Argument 3: 0x" & Image(address(Argument_3)) & " - " &
         "Argument 4: 0x" & Image(address(Argument_4)) & " - " &
         "Argument 5: 0x" & Image(address(Argument_5)) & " - " &
         "Call address: 0x" & Image(Call_Address) & '.',
         Tag => System_Call_Tag);
      Error_Status := no_error'enum_rep;
   END Null_Operation_Call;

   PROCEDURE Exit_Task_Operation_Call
     (Argument_1 : IN Intrinsics.general_register)
   IS
   BEGIN
      Tasking.Kill_Active_Task(number(Argument_1));
   END Exit_Task_Operation_Call;

   PROCEDURE Receive_Message_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.general_register;
      Argument_3   : OUT Intrinsics.general_register;
      Argument_4   : OUT Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task  : CONSTANT number := Tasking.Get_Active_Task_Index;
      Message_Data : Tasking.IPC.message_data;
      Error_Check  : error;
   BEGIN
      Tasking.IPC.Receive_Message
        (number(Argument_1),
         Active_Task,
         number(Argument_2),
         number(Argument_3),
         Message_Data,
         Error_Check);
      Argument_4 := Message_Data.XMM;
      Error_Status := Error_Check'enum_rep;
   END Receive_Message_Operation_Call;

   PROCEDURE Send_Message_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : IN Intrinsics.general_register;
      Argument_3   : IN Intrinsics.general_register;
      Argument_4   : IN Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task  : CONSTANT number := Tasking.Get_Active_Task_Index;
      Message_Data : CONSTANT Tasking.IPC.message_data := (XMM => Argument_4);
      Error_Check  : error;
   BEGIN
      Tasking.IPC.Send_Message
        (Active_Task,
         number(Argument_1),
         number(Argument_2),
         number(Argument_3),
         Message_Data,
         Error_Check);
      Error_Status := Error_Check'enum_rep;
   END Send_Message_Operation_Call;

   PROCEDURE Identify_Task_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register)
   IS
      TYPE padded_task_status IS RECORD
         Data   : Tasking.task_status;
         Zeroed : bytes(1 .. 176);
      END RECORD
      WITH
         Size        => 2048,
         Object_Size => 2048;

      FUNCTION To_XMM IS NEW Ada.Unchecked_Conversion
        (source => padded_task_status, target => Intrinsics.XMM_registers);
      PRAGMA Annotate(GNATprove, False_Positive, "type with constraints *",
         "The task status's size fits inside all of the Argument_ registers.");

      Active_Task_Index : CONSTANT number := Tasking.Get_Active_Task_Index;
      Status            : CONSTANT Tasking.task_status :=
         Tasking.Get_Task_Status
           (IF Argument_1 = 0 THEN Active_Task_Index ELSE number(Argument_1));
   BEGIN
      Argument_2 := To_XMM((Data => Status, Zeroed => (OTHERS => 0)));
      Error_Status := (IF Status.Index = 0 THEN
         index_error'enum_rep ELSE no_error'enum_rep);
   END Identify_Task_Operation_Call;

   PROCEDURE Load_ELF_Operation_Call
     (Argument_1   : IN Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task_Index : CONSTANT number := Tasking.Get_Active_Task_Index;
      ELF_Address       : address;
      ELF_Size          : number;
      Error_Check       : error;
   BEGIN
      Tasking.Buffer.Buffer_Base(Active_Task_Index, ELF_Address, ELF_Size,
         Error_Check);

      IF
         Error_Check /= no_error
      THEN
         Error_Status := Error_Check'enum_rep;
         RETURN;
      END IF;

      Tasking.ELF.Load(ELF_Address, ELF_Size,
         string(To_Characters(Argument_1)(Tasking.task_name_string'range)),
         Error_Check);
      Error_Status := Error_Check'enum_rep;
   END Load_ELF_Operation_Call;

   PROCEDURE Heap_Increase_Operation_Call
     (Argument_1   : OUT Intrinsics.general_register;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task_Index : CONSTANT number := Tasking.Get_Active_Task_Index;
      Error_Check       : error;
   BEGIN
      Tasking.Memory.Increase(Active_Task_Index, address(Argument_1),
         Error_Check);
      Error_Status := Error_Check'enum_rep;
   END Heap_Increase_Operation_Call;

   PROCEDURE Yield_Operation_Call
     (Error_Status : OUT Intrinsics.general_register)
   IS
   BEGIN
      Tasking.Yield;
      Error_Status := no_error'enum_rep;
   END Yield_Operation_Call;

   PROCEDURE Log_Operation_Call
     (Argument_1   : IN Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task_Index  : CONSTANT number := Tasking.Get_Active_Task_Index;
      Active_Task_Status : CONSTANT Tasking.task_status :=
         Tasking.Get_Task_Status(Active_Task_Index);
   BEGIN
      Log("Log from task """ & Active_Task_Status.Name & """: || " &
         string(To_Characters(Argument_1)) & " ||", Tag => System_Call_Tag);
      Error_Status := no_error'enum_rep;
   END Log_Operation_Call;

   PROCEDURE IRQ_Statistics_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.general_register;
      Error_Status : OUT Intrinsics.general_register)
   IS
   BEGIN
      IF
         number(Argument_1) IN Interrupts.Counters'range
      THEN
         Argument_2 := Intrinsics.general_register
           (Interrupts.Counters(number(Argument_1)));
         Error_Status := no_error'enum_rep;
      ELSE
         Argument_2 := Intrinsics.general_register'first;
         Error_Status := index_error'enum_rep;
      END IF;
   END IRQ_Statistics_Operation_Call;

   PROCEDURE Buffer_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : IN Intrinsics.general_register;
      Argument_3   : IN OUT Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Task_Index  : CONSTANT number := Tasking.Get_Active_Task_Index;
      Error_Check : error;
   BEGIN
      CASE
         Argument_1
      IS
         WHEN 1 => -- Create.
            Tasking.Buffer.Create(Task_Index, number(Argument_2),
               Error_Check);
         WHEN 2 => -- Read.
            Tasking.Buffer.Read(Task_Index, number(Argument_2), Argument_3,
               Error_Check);
         WHEN 3 => -- Write.
            Tasking.Buffer.Write(Task_Index, number(Argument_2), Argument_3,
               Error_Check);
         WHEN 4 => -- Delete.
            Tasking.Buffer.Delete(Task_Index);
         WHEN OTHERS =>
            Error_Check := index_error;
      END CASE;

      Error_Status := Error_Check'enum_rep;
   END Buffer_Operation_Call;

   PROCEDURE Framebuffer_Access_Operation_Call
     (Argument_1   : OUT Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.general_register;
      Argument_3   : OUT Intrinsics.general_register;
      Argument_4   : OUT Intrinsics.general_register;
      Argument_5   : OUT Intrinsics.general_register;
      Error_Status : OUT Intrinsics.general_register)
   IS
      Active_Task : CONSTANT number := Tasking.Get_Active_Task_Index;
   BEGIN
      Tasking.Map_Address_Range
        (Active_Task,
         Tasking.Memory.user_framebuffer_address'first,
         UEFI.Bootloader_Arguments.Framebuffer_Address,
         UEFI.Bootloader_Arguments.Framebuffer_Size,
         Write_Access => true,
         User_Access  => true);

      Argument_1 := Intrinsics.general_register
        (Tasking.Memory.user_framebuffer_address'first);

      Argument_2 := Intrinsics.general_register
        (UEFI.Bootloader_Arguments.Framebuffer_Size);

      Argument_3  := Intrinsics.Shift_Left(Intrinsics.general_register
        (UEFI.Bootloader_Arguments.Horizontal_Resolution), 32) OR
         Intrinsics.general_register
           (UEFI.Bootloader_Arguments.Vertical_Resolution);

      Argument_4  := Intrinsics.general_register
        (UEFI.Bootloader_Arguments.Pixels_Per_Scanline);

      Argument_5 := UEFI.Bootloader_Arguments.Pixel_Format'enum_rep;

      Error_Status := no_error'enum_rep;
   END Framebuffer_Access_Operation_Call;

END HAVK_Kernel.System_Call;
