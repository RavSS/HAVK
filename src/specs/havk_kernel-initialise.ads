WITH
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Paging,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

PACKAGE HAVK_Kernel.Initialise
IS
   -- Prepares the descriptor tables, which is necessary for interrupts.
   PROCEDURE Descriptor_Tables;

   -- Identity maps the kernel's address space. Takes in an argument
   -- for the bootloader arguments structure passed to the kernel.
   PROCEDURE Default_Page_Layout(
      Bootloader : IN UEFI.arguments;
      Map        : IN UEFI.memory_map);

   -- Draws a grid to the screen as an initial test.
   PROCEDURE Grid_Test(
      Display    : IN OUT view;
      Colour     : IN pixel);

   -- Prints the magic number to the textbox.
   PROCEDURE See_Magic(
      Terminal   : IN OUT textbox);

   -- Prints the nearly all of the current font's characters.
   PROCEDURE Font_Test(
      Terminal   : IN OUT textbox);

   -- Outputs the PS/2 scancodes to the textbox. Does not return as of now.
   PROCEDURE PS2_Scancode_Test(
      Terminal   : IN OUT textbox;
      Display    : IN view);

   -- Outputs the character from the input handler to the textbox.
   PROCEDURE Input_Key_Test(
      Terminal   : IN OUT textbox;
      Display    : IN view);

   -- Prints the amount of seconds passed (since call) to a textbox endlessly.
   PROCEDURE Seconds_Count(
      Terminal   : IN OUT textbox;
      Display    : IN view)
   WITH
      No_Return     => true,
      Inline_Always => true;

   -- Initialises the PS/2 controller for keyboard input purposes (as of now).
   PROCEDURE PS2_Input;

   -- Retrieves the date and time in ISO 8601 format of when the current
   -- running version of the kernel was compiled and built.
   FUNCTION HAVK_Build_Datetime
   RETURN string;

   -- Initialises any debug utilities.
   PROCEDURE Debugger;

   -- The default paging layout.
   Kernel_Paging_Layout : Paging.page_layout;
PRIVATE
   -- Bootloader magic number.
   Magic : CONSTANT num
   WITH
      Import     => true,
      Convention => C,
      Link_Name  => "bootloader.magic";
END HAVK_Kernel.Initialise;
