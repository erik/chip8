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
   KeyboardState = array [0 .. $F] of 0..1;
   ScreenArray = array [0 .. ScreenWidth, 0 .. ScreenHeight] of 0..1;

type
   QuitException = class (Exception);
   SDLException  = class (Exception);
   ConsoleException  = class (Exception);

type TScreen = class
protected
   Screen : ScreenArray;
public
   KeyState : KeyboardState;

   constructor Create; virtual;
   destructor Destroy; override;

   procedure SetPixel (X,Y,Val : Integer);
   function GetPixel (X, Y : Integer) : Byte;

   procedure Display; virtual abstract;
   procedure UpdateKeyState; virtual abstract;

   function WaitKey : Byte; virtual abstract;
end;

implementation

constructor TScreen.Create;
var
   I, J : Integer;
begin
   for I := 0 to High (KeyState) do
      KeyState [I] := 0;

   for I := 0 to ScreenWidth do
      for J := 0 to ScreenHeight do
         Screen [I][J] := 0;
end;

destructor TScreen.Destroy;
begin
   inherited Destroy;
end;

function TScreen.GetPixel (X, Y : Integer):Byte;
begin
   Result := Screen [X][Y];
end;

procedure TScreen.SetPixel (X, Y, Val : Integer);
begin
   Screen [X][Y] := Val;
end;

end. // unit screen
