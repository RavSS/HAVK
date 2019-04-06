with System.Machine_Code; use System.Machine_Code;
with Terminal; use Terminal;

procedure HAVK is
begin
	Asm("hlt", Volatile => True);
end HAVK;
