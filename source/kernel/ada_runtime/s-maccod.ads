------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . M A C H I N E _ C O D E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides machine code support, both for intrinsic machine
--  operations, and also for machine code statements. See GNAT documentation
--  for full details.

PACKAGE System.Machine_Code IS
   PRAGMA No_Elaboration_Code_All;
   PRAGMA Pure;

   --  All identifiers in this unit are implementation defined

   PRAGMA Implementation_Defined;

   TYPE ASM_input_operand IS PRIVATE;
   TYPE ASM_output_operand IS PRIVATE;
   --  These types are never used directly, they are declared only so that
   --  the calls to Asm are type correct according to Ada semantic rules.

   No_Input_Operands  : CONSTANT ASM_input_operand;
   No_Output_Operands : CONSTANT ASM_output_operand;

   TYPE ASM_input_operand_list IS
      ARRAY (integer RANGE <>) OF ASM_input_operand;

   TYPE ASM_output_operand_list IS
      ARRAY (integer RANGE <>) OF ASM_output_operand;

   TYPE Asm_Insn IS PRIVATE;
   --  This type is not used directly. It is declared only so that the
   --  aggregates used in code statements are type correct by Ada rules.

   PROCEDURE ASM
      (Template : string;
       Outputs  : ASM_output_operand_list;
       Inputs   : ASM_input_operand_list;
       Clobber  : string  := "";
       Volatile : boolean := false);

   PROCEDURE ASM
      (Template : string;
       Outputs  : ASM_output_operand := No_Output_Operands;
       Inputs   : ASM_input_operand_list;
       Clobber  : string             := "";
       Volatile : boolean            := false);

   PROCEDURE ASM
      (Template : string;
       Outputs  : ASM_output_operand_list;
       Inputs   : ASM_input_operand := No_Input_Operands;
       Clobber  : string            := "";
       Volatile : boolean           := false);

   PROCEDURE ASM
      (Template : string;
       Outputs  : ASM_output_operand := No_Output_Operands;
       Inputs   : ASM_input_operand  := No_Input_Operands;
       Clobber  : string             := "";
       Volatile : boolean            := false);

   FUNCTION ASM
      (Template : string;
       Outputs  : ASM_output_operand_list;
       Inputs   : ASM_input_operand_list;
       Clobber  : string  := "";
       Volatile : boolean := false)
      RETURN asm_insn;

   FUNCTION ASM
      (Template : string;
       Outputs  : ASM_output_operand := No_Output_Operands;
       Inputs   : ASM_input_operand_list;
       Clobber  : string             := "";
       Volatile : boolean            := false)
      RETURN asm_insn;

   FUNCTION ASM
      (Template : string;
       Outputs  : ASM_output_operand_list;
       Inputs   : ASM_input_operand := No_Input_Operands;
       Clobber  : string            := "";
       Volatile : boolean           := false)
      RETURN asm_insn;

   FUNCTION ASM
      (Template : string;
       Outputs  : ASM_output_operand := No_Output_Operands;
       Inputs   : ASM_input_operand  := No_Input_Operands;
       Clobber  : string             := "";
       Volatile : boolean            := false)
      RETURN asm_insn;

   PRAGMA Import (Intrinsic, ASM);

PRIVATE

   TYPE ASM_input_operand IS NEW integer;
   TYPE ASM_output_operand IS NEW integer;
   TYPE ASM_insn IS NEW integer;
   --  All three of these types are dummy types, to meet the requirements of
   --  type consistency. No values of these types are ever referenced.

   No_Input_Operands  : CONSTANT ASM_input_operand  := 0;
   No_Output_Operands : CONSTANT ASM_output_operand := 0;

END System.Machine_Code;
