unit vm;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils,
   Screen, SDL;

const
   MemorySize = $0FFF;
   StackSize  = $F;
   ClockTick  = 1000 div 60;

   { default character palatte }
   Font : array [0 .. $4F] of Byte
      = ($F0, $90, $90, $90, $F0,
         $20, $60, $20, $20, $70,
         $F0, $10, $F0, $80, $F0,
         $F0, $10, $F0, $10 ,$F0,
         $90, $90, $F0, $10, $10,
         $F0, $80, $F0, $10, $F0,
         $F0, $80, $F0, $90, $F0,
         $F0, $10, $20, $40, $40,
         $F0, $90, $F0, $90 ,$F0,
         $F0, $90, $F0, $10, $F0,
         $F0, $90, $F0, $90, $90,
         $E0, $90, $E0, $90, $E0,
         $F0, $80, $80, $80, $F0,
         $E0, $90, $90, $90, $E0,
         $F0, $80, $F0, $80, $F0,
         $F0, $80, $F0, $80, $80);

type Chip8ROM = array [0 .. MemorySize] of Byte;

type TChip8VM = class
private
   { General purpose 8-bit registers }
   Registers              : array [0 .. $F] of Byte;

   { 16 bit register }
   RegisterI              : Word;

   { Program memory }
   Memory                 : Chip8ROM;
   PC                     : Word;

   DelayTimer, SoundTimer : Byte;

   Stack                  : array [0 .. StackSize] of Word;
   SP                     : Byte;

   Screen                 : TScreen;
   ScreenArr              : ScreenArray;
public
   constructor Create (Prog : Chip8ROM);
   destructor Destroy; override;

   procedure Evaluate;
   procedure ClearScreen;
   procedure RunLoop;
end;

implementation

constructor TChip8VM.Create (Prog : Chip8ROM);
var
   I : Integer;
begin
   Randomize;

   RegisterI := 0;
   DelayTimer := 0;
   SoundTimer := 0;
   PC := 0;
   SP := 0;

   for I := 0 to $F do
   begin
      Registers [I] := 0;
   end;

   { TODO: is there a memcpy-esque function for pascal? }
   for I := 0 to MemorySize do
   begin
      Memory [I] := Prog [I];
   end;

   for I := 0 to StackSize do
   begin
      Stack [I] := 0;
   end;

   ClearScreen;

   Screen := TScreen.Create (128, 64);
end;

destructor TChip8VM.Destroy;
begin
   Screen.Destroy;
   inherited Destroy;
end;

procedure TChip8VM.Evaluate;
var
   Instruction      : Word;
   OP               : Byte;
   X, Y, RX, RY, KK : Byte;
   NNN              : Word;

   KeyState         : KeyboardState;

   { general purpose }
   I : Integer;
begin
   Instruction := (Memory [PC] shl 8) or Memory [PC + 1];
   PC := PC + 2;

   OP := (Instruction and $F000) shr 12;
   X  := (Instruction and $0F00) shr  8;
   Y  := (Instruction and $00F0) shr  4;

   KK := Instruction and $00FF;
   NNN:= Instruction and $0FFF;

   RX := Registers [X];
   RY := Registers [Y];

   KeyState := Screen.KeyState;

   if DelayTimer > 0 then DelayTimer := DelayTimer - 1;
   if SoundTimer > 0 then SoundTimer := SoundTimer - 1;

   if DelayTimer <= 0 then DelayTimer := 0;
   if SoundTimer <= 0 then
   begin
      SoundTimer := 0;
      { TODO: Generate beep. }
   end;

   case OP of
      0: {SYS, CLS, RET}
         case KK of
            $E0: ClearScreen;
            $EE: begin
                    PC := Stack [SP];
                    SP := SP - 1;
                 end;
         else    {SYS, unused};
         end;
      1: {JP}    PC := NNN;
      2: {CALL}
         begin
            SP := SP + 1;
            Stack [SP] := PC;
            PC := NNN;
         end;
      3: {SE}    if RX =  KK then PC := PC + 2;
      4: {SNE}   if RX <> KK then PC := PC + 2;
      5: {SE}    if RX =  RY then PC := PC + 2;
      6: {LD}    Registers [X] := KK;
      7: {ADD}   Registers [X] := RX + KK;
      8: {LD, OR, AND, XOR, SUB, SHR, SUBN, SHL}
         case KK of
            0: Registers [X] := RY;
            1: Registers [X] := RX or  RY;
            2: Registers [X] := RX and RY;
            3: Registers [X] := RX xor RY;
            4: begin
                  I := RX + RY;
                  if I > 255 then Registers [$F] := 1;
                  Registers [X] := I and $FF;
               end;
            5: begin
                  if RX > RY then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := RX - RY;
               end;
            6: begin
                  if RX and $1 = 1 then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := RX div 2;
               end;
            7: begin
                  if RY > RX then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := RY - RX;
               end;
            $E: begin
                   if RX and $1000 = $1 then Registers [$F] := 1
                   else Registers [$F] := 0;
                   Registers [X] := RX * 2;
                end;
         end;
      9: {SNE}   if RX <> RY then PC := PC + 2;
      $A: {LD}   RegisterI := NNN;
      $B: {JP}   PC := Registers [0] + NNN;
      $C: {RND}  Registers [X] := Random ($100) and KK;
      $D: {DRW} {TODO};
      $E: {SKP, SKNP}
         case KK of
            $9E: if KeyState [RX and $F] = 1  then PC := PC + 2;
            $A1: if KeyState [RX and $F] <> 1 then PC := PC + 2;
         end;
      $F: {LD, ADD}
         case KK of
            $07: Registers [X] := DelayTimer;
            $0A: Registers [X] := Screen.WaitKey;
            $15: DelayTimer := RX;
            $18: SoundTimer := RX;
            $1E: RegisterI  := RegisterI + RX;
            $29: {TODO};
            $33: {TODO};
            $55:
               for I := 0 to $F do
                  Memory [RegisterI + I] := Registers [I];
            $65:
               for I := 0 to $F do
                  Registers [I] := Memory [RegisterI + I];
         end;
   end;
end;

procedure TChip8VM.ClearScreen;
var
   I : Integer;
begin
   for I := 0 to ScreenSize do
   begin
      ScreenArr [I] := 0;
   end;
end;

procedure TChip8VM.RunLoop;
begin
   while True do
   begin
      Evaluate;
      Screen.DisplayScreen (ScreenArr);
      SDL_Delay (ClockTick);
   end;
end;

end. // unit vm
