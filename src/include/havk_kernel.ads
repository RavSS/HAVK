PACKAGE HAVK_Kernel IS
	TYPE byte IS MOD 16#FF# + 1;
	TYPE bytes IS ARRAY(natural RANGE <>) OF byte;
END HAVK_Kernel;
