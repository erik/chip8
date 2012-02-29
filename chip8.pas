program chip8;

{$mode objfpc}{$H+}

uses
  Classes, VM, Screen;
var
   ROM     : Chip8ROM;
   Chip8VM : TChip8VM;
   FP      : File of Byte;
   B       : Byte;
   I, Size : Integer;

begin

   if ParamCount < 1 then
   begin
      Writeln ('Need to specify a ROM file.');
      halt;
   end;

   Assign (FP, ParamStr (1));
   Reset (FP);

   Size := FileSize (FP);

   if Size > VM.ROMSize then Size := VM.ROMSize;

   B := 0;

   for I := Low (ROM) to Size - 1 do
   begin
      Read (FP , B);
      ROM [I] := B;
   end;

   for I := I to High (ROM) do ROM[I] := 0;

   Close (FP);

   Chip8VM := TChip8VM.Create (ROM);

   try
      Chip8VM.RunLoop;
   except
      on Screen.QuitException do {nothing};
   end;

   Chip8VM.Free;
end.

