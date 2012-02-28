program chip8;

{$mode objfpc}{$H+}

uses
  Classes, VM, Screen;
var
   ROM : Chip8ROM;
   Chip8VM : TChip8VM;

   I : Integer;
begin
   WriteLn ('CHIP8 Interpreter.');

   for I := 0 to VM.MemorySize do
      ROM [I] := 0;

   Chip8VM := TChip8VM.Create (ROM);
   Chip8VM.RunLoop;
end.

