-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_phase_ii.adb                                      --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE
   HAVK_Kernel,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

-- This phase is for setting up the HAVK operating system, as tasking is now
-- functional if this is reached successfully.
PROCEDURE HAVK_Phase_II
IS
   Display : CONSTANT view := Get_Display(UEFI.Get_Arguments);

   -- Just to show something for now.
   Goodbye : textbox(Display.Screen_Width / 14 + 2, 1);
BEGIN
   Log("Entered Phase II successfully.", nominal);
   Log("End of HAVK's Phase II reached.", warning);

   -- Development is on-going from here.

   Goodbye.Start_Position := Display.Calculate_Pixel
     (Display.Screen_Width / 8, Display.Screen_Height / 2);
   Goodbye.Print("WORK-IN-PROGRESS FROM HERE!", Centre => true);
   Display.Draw_Fill(0, Display.Framebuffer_Elements, 0);
   Goodbye.Draw_On(Display);

   LOOP
      Intrinsics.Spinlock_Pause;
   END LOOP;
END HAVK_Phase_II;
