------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              A D A . U N C H E C K E D _ C O N V E R S I O N             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

GENERIC
   TYPE Source (<>) IS LIMITED PRIVATE;
   TYPE Target (<>) IS LIMITED PRIVATE;

FUNCTION Ada.Unchecked_Conversion
   (S : source)
   RETURN target;

PRAGMA No_Elaboration_Code_All (Ada.Unchecked_Conversion);
PRAGMA Pure (Ada.Unchecked_Conversion);
PRAGMA Import (Intrinsic, Ada.Unchecked_Conversion);
