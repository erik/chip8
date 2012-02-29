unit screen;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils;

const
   ScreenWidth = 64;
   ScreenHeight = 32;
   ScreenSize = ScreenWidth * ScreenHeight;

type
   KeyboardState = array [0 .. $F] of Boolean;
   ScreenArray = array [0 .. ScreenWidth, 0 .. ScreenHeight] of Boolean;

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
   Screen   : ScreenArray;
   KeyState : KeyboardState;

   function PollKey : KeyPress; virtual abstract;
   function WaitKey : KeyPress; virtual abstract;
public
   constructor Create; virtual;
   destructor Destroy; override;

   property Keys : KeyboardState read KeyState;

   procedure SetPixel (X, Y : Integer; B : Boolean);
   function  GetPixel (X,Y   : Integer) : Boolean;

   procedure ClearScreen;
   procedure Update;

   procedure Display; virtual abstract;

   function GetKey : Byte;
end;

implementation

constructor TScreen.Create;
var
   I, J : Integer;
begin
   for I := 0 to High (KeyState) do
      KeyState [I] := False;

   for I := 0 to ScreenWidth do
      for J := 0 to ScreenHeight do
         Screen [I][J] := False;
end;

destructor TScreen.Destroy;
begin
   inherited Destroy;
end;

function TScreen.GetPixel (X, Y : Integer) : Boolean;
begin
   Result := Screen [X][Y];
end;

procedure TScreen.SetPixel (X, Y : Integer; B : Boolean);
begin
   Screen [X][Y] := B;
end;

procedure TScreen.ClearScreen;
var
   I, J : Integer;
begin
   for I := 0 to ScreenWidth - 1 do
      for J := 0 to ScreenHeight - 1 do
         SetPixel (I, J, False);
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
      if Key.KeyType = Press then writeln ('keypress => ', Key.key);

      if Key.KeyType = Press  then
         KeyState [Key.Key] := True
      else if Key.KeyType = Release then
         KeyState [Key.Key] := False;
   until Key.KeyType = None;
end;

end. // unit screen
