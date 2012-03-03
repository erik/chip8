unit vm;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils,
   Screen, SDLScreen, SDL;

const
   MemorySize  = $0FFF;
   ROMSize     = MemorySize - $200;
   StackSize   = $10;
   ClockTick   = 1000 div 60;
   ScreenScale = 15;

   { default character palatte }
   Font : array [0 .. $4F] of Byte
      = ($F0, $90, $90, $90, $F0,  // 0
         $20, $60, $20, $20, $70,  // 1
         $F0, $10, $F0, $80, $F0,  // 2
         $F0, $10, $F0, $10 ,$F0,  // 3
         $90, $90, $F0, $10, $10,  // 4
         $F0, $80, $F0, $10, $F0,  // 5
         $F0, $80, $F0, $90, $F0,  // 6
         $F0, $10, $20, $40, $40,  // 7
         $F0, $90, $F0, $90 ,$F0,  // 8
         $F0, $90, $F0, $10, $F0,  // 9
         $F0, $90, $F0, $90, $90,  // A
         $E0, $90, $E0, $90, $E0,  // B
         $F0, $80, $80, $80, $F0,  // C
         $E0, $90, $90, $90, $E0,  // D
         $F0, $80, $F0, $80, $F0,  // E
         $F0, $80, $F0, $80, $80); // F

   SChip8Font : array [0 .. $9F] of Byte
      = ($7E, $FF, $C3, $C3, $C3, $C3, $C3, $C3, $FF, $7E,  // 0
         $38, $F8, $F8, $38, $38, $38, $38, $38, $FF, $FF,  // 1
         $3C, $FF, $E7, $07, $0E, $1C, $38, $70, $FF, $FF,  // 2
         $3C, $FF, $E7, $07, $7E, $7E, $0F, $E7, $FF, $3C,  // 3
         $1E, $3E, $E6, $C6, $FF, $7F, $0E, $0E, $0E, $0E,  // 4
         $FF, $FF, $E0, $E0, $78, $3C, $0F, $E7, $FF, $7E,  // 5
         $0F, $3C, $78, $F0, $FE, $FF, $E7, $E7, $FF, $7E,  // 6
         $FF, $FF, $E7, $0E, $1C, $38, $38, $38, $38, $38,  // 7
         $3C, $E7, $C3, $E7, $3C, $3C, $E7, $C3, $E7, $3C,  // 8
         $7E, $FF, $E7, $E7, $FF, $7F, $0F, $1E, $1E, $78,  // 9
         $3C, $66, $E7, $E7, $FF, $FF, $E7, $E7, $E7, $E7,  // A
         $FE, $E3, $E3, $E3, $FE, $FE, $E3, $E3, $E3, $FE,  // B
         $7E, $E7, $E0, $E0, $E0, $E0, $E0, $E0, $E7, $7E,  // C
         $FC, $E6, $E3, $E3, $E3, $E3, $E3, $E3, $E6, $FC,  // D
         $FF, $FF, $E1, $E0, $FE, $FE, $E0, $E1, $FF, $FF,  // E
         $FF, $FF, $E1, $E0, $FE, $FE, $E0, $E0, $E0, $E0); // F

type
   InstructionExitException    = class (Exception);
   IllegalInstructionException = class (Exception);


type Chip8ROM = array [0 .. ROMSize] of Byte;

type TChip8VM = class
private
   { General purpose 8-bit registers }
   Registers              : array [0 .. $F] of Byte;

   { 16 bit register }
   RegisterI              : 0 .. $FFF;

   { Program memory }
   Memory                 : array [0 .. MemorySize] of Byte;
   PC                     : 0 .. MemorySize;

   SChip8Flags            : array [0 .. 7] of Byte;

   DelayTimer, SoundTimer : Byte;

   Stack                  : array [1 .. StackSize] of Word;
   SP                     : 1 .. StackSize;

   Screen                 : TScreen;
public
   constructor Create (Prog : Chip8ROM);
   destructor Destroy; override;

   procedure Evaluate;
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
   PC := $200;
   SP := 1;

   for I := Low (Font) to High (Font) do
      Memory [I] := Font [I];

   for I := Low (SChip8Font) to High (SChip8Font) do
      Memory [I + $50] := SChip8Font [I];

   for I := Low (Prog) to High (Prog) do
      Memory [I + $200] := Prog [I];

   Screen := TSDLScreen.Create (Chip8Width * 10, Chip8Height * 10);
   Screen.ClearScreen;
end;

destructor TChip8VM.Destroy;
begin
   Screen.Free;
   inherited Destroy;
end;

procedure TChip8VM.Evaluate;
var
   Instruction   : Word;
   OP, X, Y      : 0 .. 15;
   RX, RY, KK, N : Byte;
   NNN           : Word;

   { general purpose }
   I, J  : Integer;
   B     : Boolean;
begin
   Instruction := (Memory [PC] shl 8) or Memory [PC + 1];
   PC          := PC + 2;

   I := 0; J := 0;

   OP  := (Instruction and $F000) shr 12;
   X   := (Instruction and $0F00) shr  8;
   Y   := (Instruction and $00F0) shr  4;

   KK  := Instruction and $00FF;
   NNN := Instruction and $0FFF;
   N   := Instruction and $000F;

   RX  := Registers [X];
   RY  := Registers [Y];


  { writeln
      (Format ('PC: %.4x INS: %.4x X: %.2x Y: %.2X RX: %.2x RY: %.2x I: %.3x',
               [PC, Instruction, X, Y, RX, RY, RegisterI]));}


   if DelayTimer > 0 then DelayTimer := DelayTimer - 1;
   if SoundTimer > 0 then SoundTimer := SoundTimer - 1;

   case OP of
      0: case KK of
            $E0: Screen.ClearScreen;                             { CLS }
            $EE: begin                                           { RET }
                    SP := SP - 1;
                    PC := Stack [SP];
                 end;
            $FB:  Screen.ScrollRight;                            { SCR }
            $FC:  Screen.ScrollLeft;                             { SCL }
            $FD:  raise InstructionExitException.Create          { EXIT }
                     ('EXIT called');
            $FE:  Screen.SetLow;                                 { LOW  }
            $FF:  Screen.SetHigh;                                { HIGH }
         else if Y = $C then Screen.ScrollDown (N);              { SCD }
         end;

      1: PC := NNN;                                              { JP NNN }
      2: begin                                                   { CALL NNN}
            Stack [SP] := PC;
            SP := SP + 1;
            PC := NNN;
         end;

      3: if RX =  KK then PC := PC + 2;                          { SE Vx, KK }
      4: if RX <> KK then PC := PC + 2;                          { SNE Vx, KK }
      5: if RX =  RY then PC := PC + 2;                          { SE Vx, VY }

      6: Registers [X] := KK;                                    { LD Vx, KK }

      7: Registers [X] := (RX + KK) and $FF;                     { ADD Vx, KK }

      8: case KK of
            0: Registers [X] := RY;                              { LD Vx, Vy }

            1: begin                                             { OR Vx, Vy }
                  Registers [$F] := 0;
                  Registers [X] := RX or RY;
               end;
            2: begin                                             { AND Vx, Vy }
                  Registers [$F] := 0;
                  Registers [X] := RX and RY;
               end;
            3: begin                                             { XOR Vx, Vy }
                  Registers [$F] := 0;
                  Registers [X] := RX xor RY;
               end;

            4: begin                                             { ADD Vx, Vy }
                  I := RX + RY;
                  if I > $FF then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := I and $FF;
               end;
            5: begin                                             { SUB Vx, Vy }
                  if RX >= RY then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := (RX - RY) and $FF;
               end;

            6: begin                                             { SHR Vx }
                  if RX and $1 = 1 then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := RX div 2;
               end;

            7: begin                                             { SUBN Vx, Vy }
                  if RY >= RX then Registers [$F] := 1
                  else Registers [$F] := 0;
                  Registers [X] := (RY - RX) and $FF;
               end;

            $E: begin                                            { SHL Vx }
                   if RX and ($1 shl 7) = $1 then
                      Registers [$F] := 1
                   else
                      Registers [$F] := 0;
                   Registers [X] := (RX shl 1) and $FF;
                end;
         end;

      9:  if RX <> RY then PC := PC + 2;                         { SNE Vx, Vy }

      $A: RegisterI := NNN;                                      { LD I, NNN }

      $B: PC := (Registers [0] + NNN) and $FFF;                  { JP V0, NNN }

      $C: Registers [X] := Random ($FFF) and KK;                 { RND Vx, KK }

      $D:
         if N = 0 then                                           { DRW0 Vx, Vy }
         begin
            Registers [$F] := 0;
            // XXX: This is completely broken
            for I := 0 to 31 do
               for J := 0 to 7 do
                  if (Memory [RegisterI + I] and (1 shl (7 - J))) <> 0 then
                  begin
                     B := Screen.GetPixel (RX + J, RY + I);

                     if B = True then
                        Registers [$F] := 1;

                     Screen.SetPixel (RX + J, RY + I, B xor True);
                  end;
         end
         else                                                    { DRW Vx, Vy }
         begin
            Registers [$F] := 0;
            // Y coord
            for I := 0 to N - 1 do
               // X coord
               for J := 0 to 7 do
                  // if current bit of sprite is set
                  if (Memory [RegisterI + I] and (1 shl (7 - J))) <> 0 then
                  begin
                     B := Screen.GetPixel (RX + J, RY + I);

                     if B = True then
                        Registers [$F] := 1;

                     Screen.SetPixel (RX + J, RY + I, B xor True);
                  end;
         end;

      $E: case KK of
             $9E: if Screen.Keys [RX] = True  then PC := PC + 2; { SKP Vx }
             $A1: if Screen.Keys [RX] <> True then PC := PC + 2; { SKNP Vx }
          end;

      $F: case KK of
             $07: Registers [X] := DelayTimer;                   { LD Vx, DT }

             $0A: Registers [X] := Screen.GetKey;                { LD Vx, K }

             $15: DelayTimer := RX;                              { LD DT, Vx }
             $18: SoundTimer := RX;                              { LD ST, Vx }

             $1E: begin                                          { ADD I, Vx }
                     I := RegisterI + RX;
                     Registers [$F] := 0;
                     if I > $FFF then Registers [$F] := 1;
                     RegisterI := I and $FFF;
                  end;

             $29: RegisterI := 5 * RX;                           { LD F, Vx }
             $30: RegisterI := $50 + 10 * RX;                    { LD HF, Vx }
             $33: begin                                          { LD B, Vx }
                     Memory [RegisterI]     := RX div 100;
                     Memory [RegisterI + 1] := (RX div 10) mod 10;
                     Memory [RegisterI + 2] := (RX mod 100) mod 10;
                  end;
             $55: begin                                          { LD [I], Vx }
                     for I := 0 to X do
                        Memory [RegisterI + I] := Registers [I];
                  end;
             $65: begin                                          { LD Vx, [I] }
                     for I := 0 to X do
                        Registers [I] := Memory [RegisterI + I];
                  end;
             $75: begin                                          { LD R, Vx }
                     if X > 7 then X := 7;
                     for I := 0 to X do
                        SChip8Flags [I] := Registers [I];
                  end;
             $85: begin                                          { LD Vx, R }
                     if X > 7 then X := 7;
                     for I := 0 to X do
                        Registers [I] := SChip8Flags [I];
                  end;
          end;
   end;
end;

procedure TChip8VM.RunLoop;
begin

   while True do
   begin
      if not Screen.Paused then Evaluate;
      Screen.Display;
      Screen.Update;
      SDL_Delay (ClockTick);
   end;
end;

end. // unit vm
