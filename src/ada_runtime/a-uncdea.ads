------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           A D A . U N C H E C K E D _ D E A L L O C A T I O N            --
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
   TYPE Object (<>) IS LIMITED PRIVATE;
   TYPE Name IS ACCESS object;

PROCEDURE Ada.Unchecked_Deallocation
   (X : IN OUT name);
PRAGMA Preelaborate (Unchecked_Deallocation);

PRAGMA Import (Intrinsic, Ada.Unchecked_Deallocation);
