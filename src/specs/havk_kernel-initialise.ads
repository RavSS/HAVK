WITH
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
   PROCEDURE Default_Page_Layout;

   -- Draws a grid to the screen as an initial test.
   PROCEDURE Grid_Test(
      Display    : IN view;
      Colour     : IN pixel);

   -- Prints the magic number to the textbox.
   PROCEDURE See_Magic(
      Terminal   : IN OUT textbox);

   -- Prints the nearly all of the current font's characters.
   PROCEDURE Font_Test(
      Terminal   : IN OUT textbox);

   -- Waits for a key to be inputted that is different from the old key
   -- specified as an argument.
   PROCEDURE Wait_For_New_Key(
      Terminal : IN OUT textbox;
      Display  : IN view;
      Old_Key  : IN character;
      New_Key  : IN character;
      Message  : IN string);

   -- Outputs the character from the input handler to the textbox.
   PROCEDURE Input_Key_Test(
      Terminal   : IN OUT textbox;
      Display    : IN view);

   -- Prints the amount of seconds passed (since call) to a textbox endlessly.
   PROCEDURE Seconds_Count(
      Terminal   : IN OUT textbox;
      Display    : IN view);

   -- Show arbitrary information about the memory map and its descriptors.
   PROCEDURE Memory_Map_Info(
      Terminal : IN OUT textbox);

   -- Initialises the PS/2 controller for keyboard input purposes (as of now).
   PROCEDURE PS2_Input;

   -- Retrieves the date and time in ISO 8601 format of when the current
   -- running version of the kernel was compiled and built.
   FUNCTION HAVK_Build_Datetime
   RETURN string
   WITH
      Post => HAVK_Build_Datetime'result'first =  1 AND THEN
              HAVK_Build_Datetime'result'last  = 19; -- "YYYY-MM-DDTHH:MM:SS"

   -- Initialises any debug utilities.
   PROCEDURE Debugger;

   -- The default paging layout.
   Kernel_Paging_Layout : Paging.page_layout;
PRIVATE
   -- Bootloader magic number. This is here in-case any other procedures etc.
   -- need to access it outside of `See_Magic()`.
   Magic : CONSTANT num
   WITH
      Import     => true,
      Convention => C,
      Link_Name  => "bootloader.magic";
END HAVK_Kernel.Initialise;
