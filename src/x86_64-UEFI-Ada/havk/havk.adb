with System.Machine_Code; use System.Machine_Code;
with Terminal; use Terminal;

procedure Havk is
begin
	Asm("MOV RAX, 0x5241564A4F54;" &
		"MOV RBX, 0xDEADC0DEDEADC0DE;",
		Clobber  => "rax, rbx",
		Volatile => True);

	-- Never exit this loop. Should probably be in the assembly entry.
	loop
		Asm("CLI; HLT;", Volatile => True);
	end loop;
end Havk;
