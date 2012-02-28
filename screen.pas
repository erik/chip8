{ interface to SDL screen }

unit screen;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, SDL;

const
   ScreenSize = 64 * 32;

   { One bit array for screen, to be converted to SDL screen }
type ScreenArray = array [0 .. ScreenSize] of 0..1;

type KeyboardState = array [0 .. $F] of 0..1;

type TScreen = class
private
   width, height : Integer;
   screen     : pSDL_Surface;
public
   KeyState : KeyboardState;

   constructor Create (w, h : Integer);
   destructor Destroy; override;

   procedure DisplayScreen (arr : ScreenArray);
   function WaitKey:Byte;
end;

implementation

constructor TScreen.Create (w, h : Integer);
begin
   SDL_Init (SDL_INIT_VIDEO);

   width := w;
   height := h;

   screen := SDL_SetVideoMode(width, height, 32, SDL_SWSURFACE);
   if screen = nil then
   begin
      writeln ('SDL Init failed!');
      halt;
   end;

end;

destructor TScreen.Destroy;
begin
   SDL_Quit;

   inherited Destroy;
end;

procedure TScreen.DisplayScreen (arr : ScreenArray);
begin
   SDL_Flip (screen);
end;

function TScreen.WaitKey:Byte;
begin
   Result := 0;
end;

end. // unit screen
