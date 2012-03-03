unit screen;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils;

const
   Chip8Width  = 64;   Chip8Height = 32;
   SChip8Width = 138; SChip8Height = 74;

type
   KeyboardState   = array [0 .. $F] of Boolean;
   ScreenArrayType = array of array of Boolean;
   Nibble          = 0 .. 15;
   ScreenMode      = (Extended, Normal);

type
   KeyPress = Record
      Key     : 0 .. 15;
      KeyType : (Press, Release, None);
   end;

type
   QuitException = class (Exception);
   SDLException  = class (Exception);
   ConsoleException  = class (Exception);

type TScreen = class
protected
   Screen   : ScreenArrayType;
   KeyState : KeyboardState;
   Mode     : ScreenMode;
   IsPaused : Boolean;

   function PollKey : KeyPress; virtual abstract;
   function WaitKey : KeyPress; virtual abstract;

   function CreateNewScreen : ScreenArrayType;
public
   constructor Create; virtual;
   destructor Destroy; override;

   property Keys   : KeyboardState read KeyState;
   property Paused : Boolean read IsPaused;

   procedure SetPixel (X, Y : Integer;
                       B    : Boolean);
   function  GetPixel (X,Y  : Integer) : Boolean;

   procedure ClearScreen;
   procedure Update;

   procedure Display; virtual abstract;

   function GetKey : Byte;

   function ScreenWidth  : Integer;
   function ScreenHeight : Integer;
   function ShiftWidth   : Integer;

   procedure ScrollDown (Lines : Nibble);
   procedure ScrollLeft;
   procedure ScrollRight;

   procedure SetLow;  virtual;
   procedure SetHigh; virtual;
end;

implementation

constructor TScreen.Create;
var
   I, J : Integer;
begin
   IsPaused := False;

   SetLength(Screen, Chip8Width, Chip8Height);
   Mode := Normal;

   for I := 0 to High (KeyState) do
      KeyState [I] := False;

   for I := 0 to ScreenWidth do
      for J := 0 to ScreenHeight do
         SetPixel (I, J, False);
end;

destructor TScreen.Destroy;
begin
   inherited Destroy;
end;

function TScreen.GetPixel (X, Y : Integer) : Boolean;
begin
   X := X and ScreenWidth;
   Y := Y and ScreenHeight;
   Result := Screen [X][Y];
end;

procedure TScreen.SetPixel (X, Y : Integer; B : Boolean);
begin
   if X > ScreenWidth  then X := ScreenWidth;
   if Y > ScreenHeight then Y := ScreenHeight;

   Screen [X][Y] := B;
end;

procedure TScreen.ClearScreen;
var
   X, Y : Integer;
begin
   for X := 0 to ScreenWidth do
      for Y := 0 to ScreenHeight do
         SetPixel (X, Y, False);
end;

function TScreen.GetKey : Byte;
var
   Key : KeyPress;
begin
   repeat
      Key := WaitKey;
      if Key.KeyType = Press  then
         KeyState [Key.Key] := True
      else if Key.KeyType = Release then
         KeyState [Key.Key] := False;
   until Key.KeyType = Press;

   Result := Key.Key;
end;

procedure TScreen.Update;
var
   Key : KeyPress;
begin
   repeat
      Key := PollKey;
      if Key.KeyType = Press  then
         KeyState [Key.Key] := True
      else if Key.KeyType = Release then
         KeyState [Key.Key] := False;
   until Key.KeyType = None;
end;

procedure TScreen.ScrollDown (Lines : Nibble);
var
   X, Y  : Integer;
   NewScreen : array of array of Boolean;
begin
   NewScreen := CreateNewScreen;

   for X := 0 to ScreenWidth do
      for Y := 0 to ScreenHeight do
         if Y < Lines then NewScreen [X][Y] := False
         else NewScreen [X][Y] := Screen [X][Y - Lines];
end;

procedure TScreen.ScrollLeft;
var
   X, Y   : Integer;
   NewScreen : array of array of Boolean;
begin
   NewScreen := CreateNewScreen;

   for X := 0 to ScreenWidth do
      for Y := 0 to ScreenHeight do
         if X >= ScreenWidth - ShiftWidth then NewScreen [X][Y] := False
         else NewScreen [X][Y] := Screen [X + ShiftWidth][Y];
   Screen := NewScreen;
end;

procedure TScreen.ScrollRight;
var
   X, Y   : Integer;
   NewScreen : array of array of Boolean;
begin
   NewScreen := CreateNewScreen;

   for X := 0 to ScreenWidth do
      for Y := 0 to ScreenHeight do
         if X < ShiftWidth then NewScreen [X][Y] := False
         else NewScreen [X][Y] := Screen [X - ShiftWidth][Y];
   Screen := NewScreen;
end;


procedure TScreen.SetLow;
begin
   writeln ('Switching to normal mode.');

   Mode := Normal;
   Screen := CreateNewScreen;
   ClearScreen;
end;

procedure TScreen.SetHigh;
begin
   writeln ('Switching to extended mode.');

   Mode := Extended;
   Screen := CreateNewScreen;
   ClearScreen;
end;

function TScreen.ScreenWidth  : Integer;
begin
   Result := High (Screen);
end;

function TScreen.ScreenHeight : Integer;
begin
   Result := High (Screen [0]);
end;

function TScreen.ShiftWidth : Integer;
begin
   case Mode of
      Extended: Result := 4;
      Normal  : Result := 2;
   end;
end;

function TScreen.CreateNewScreen : ScreenArrayType;
var
   Arr : ScreenArrayType;
begin
   case Mode of
      Extended: SetLength (Arr, SChip8Width, SChip8Height);
      Normal:   SetLength (Arr, Chip8Width,  Chip8Height);
   end;

   Result := Arr;
end;

end. // unit screen
