WITH
   HAVK_Kernel.Paging,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

PACKAGE HAVK_Kernel.Initialise IS
   -- Prepares the descriptor tables, which is necessary for interrupts.
   PROCEDURE Descriptor_Tables;

   -- Identity maps the kernel's address space. Takes in an access pointer
   -- to the main framebuffer, as that's our only real important MMIO region
   -- for now (I think).
   PROCEDURE Default_Page_Layout(
      Display  : IN view);

   -- Draws a grid to the screen as an initial test.
   PROCEDURE Grid_Test(
      Display  : IN OUT view;
      Colour   : IN pixel);

   -- Prints the magic number to the textbox.
   PROCEDURE See_Magic(
      Terminal : IN OUT textbox);

   -- Prints the nearly all of the current font's characters.
   PROCEDURE Font_Test(
      Terminal : IN OUT textbox);

   -- Outputs the PS/2 scancodes to the textbox.
   PROCEDURE PS2_Scancode_Test(
      Terminal : IN OUT textbox);

   -- Outputs the character from the input handler to the textbox.
   PROCEDURE Input_Key_Test(
      Terminal : IN OUT textbox);

   -- Prints the amount of seconds passed (since call) to a textbox endlessly.
   PROCEDURE Seconds_Count(
      Terminal : IN OUT textbox);

   -- Initialises the PS/2 controller for keyboard input purposes (as of now).
   PROCEDURE PS2_Input;

   -- The default paging layout.
   Kernel_Paging_Layout : Paging.page_layout_huge;

   -- Bootloader magic number.
   Magic  : CONSTANT num
   WITH
      Import     => true,
      Convention => C,
      Link_Name  => "bootloader.magic";
END HAVK_Kernel.Initialise;