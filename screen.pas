{ interface to SDL screen }

unit screen;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, SDL, SDL_gfx;

const
   ScreenWidth = 64;
   ScreenHeight = 32;
   ScreenSize = ScreenWidth * ScreenHeight;

   { One bit array for screen, to be converted to SDL screen }
type ScreenArray = array [0 .. ScreenWidth, 0 .. ScreenHeight] of 0..1;

type KeyboardState = array [0 .. $F] of 0..1;

type QuitException = class (Exception);

type TScreen = class
private
   Scale         : Integer;
   DisplayScreen : pSDL_Surface;
   Screen        : ScreenArray;

   function HandleEvent (event : pSDL_Event):Integer;
public
   KeyState : KeyboardState;

   constructor Create (s : Integer);
   destructor Destroy; override;

   procedure Display;
   procedure SetPixel (X,Y,Val : Integer);
   procedure UpdateKeyState;
   function WaitKey:Byte;
   function GetPixel (X, Y : Integer):Byte;
end;

implementation

constructor TScreen.Create (S : Integer);
var
   I, J : Integer;
begin
   SDL_Init (SDL_INIT_VIDEO);

   Scale := S;

   DisplayScreen := SDL_SetVideoMode (ScreenWidth * Scale, ScreenHeight * Scale, 32, SDL_SWSURFACE);

   if DisplayScreen = nil then
   begin
      writeln ('SDL Init failed!');
      halt;
   end;

   for I := 0 to High (KeyState) do
      KeyState [I] := 0;

   for I := 0 to ScreenWidth do
      for J := 0 to ScreenHeight do
         Screen [I][J] := 0;

end;

destructor TScreen.Destroy;
begin
   SDL_FreeSurface (DisplayScreen);
   SDL_Quit;

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

procedure TScreen.Display;
var
   I, J : Integer;
   SX, SY : Integer;
   P : LongWord;
begin
   for I := 0 to ScreenWidth - 1 do
      for J := 0 to ScreenHeight - 1 do
      begin
         if Screen [I][J] <> 0 then P := $00FF00FF else P := $222222FF;
         SX := I * Scale;
         SY := J * Scale;

         BoxColor (DisplayScreen, SX, SY, SX + Scale, SY + Scale, P);
      end;
   SDL_Flip (DisplayScreen);
end;

function TScreen.HandleEvent (event : pSDL_Event):Integer;
begin
   case event^.type_ of
      SDL_KEYDOWN:
         begin
            { 27 = Escape }
            if event^.key.keysym.sym = 27 then
               raise QuitException.Create ('Quit pressed.');
            // TODO
            Result := 0;
         end;
      SDL_KEYUP:
         begin
            // TODO
            Result := 1;
         end;

      { TODO: cleaner exit }
      SDL_QUITEV: halt;
   end; // case event

   Result := -1;
end;

procedure TScreen.UpdateKeyState;
var
   Event : pSDL_Event;
begin
   new (Event);

   while SDL_PollEvent(event) = 1 do
      HandleEvent (event);

   dispose (Event);
end;

// Blocking, waits for a key press
function TScreen.WaitKey:Byte;
var
   Event : pSDL_Event;
   I : Integer;
begin
   new (Event);

   write ('waiting for key...');

   // continuous loop because a non-keypress event may be generated.
   while True do
   begin
      SDL_WaitEvent (Event);
      I := HandleEvent (Event);
      if I <> -1 then break;
   end; // while true

   dispose (Event);

   writeln ('done!');
   Result := I;
end;

end. // unit screen
