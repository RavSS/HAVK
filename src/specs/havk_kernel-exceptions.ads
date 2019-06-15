WITH
   System;
USE
   System;

PACKAGE HAVK_Kernel.Exceptions
IS
   PROCEDURE Last_Chance_Handler(
      Source_Location : System.Address;
      Line : integer)
   WITH
      Export        =>  True,
      Convention    =>  C,
      External_Name => "__gnat_last_chance_handler";

   PROCEDURE Stack_Smash_Handler
   WITH
      Export        =>  True,
      Convention    =>  C,
      External_Name => "__stack_chk_fail";
END HAVK_Kernel.Exceptions;
