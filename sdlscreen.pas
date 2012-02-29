unit sdlscreen;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, SDL, SDL_gfx, Screen;

type TSDLScreen = class (TScreen)
private
   Scale         : Integer;
   DisplayScreen : pSDL_Surface;
   function HandleEvent (event : pSDL_Event):Integer;
public
   constructor Create (S : Integer); reintroduce;
   destructor Destroy; override;

   procedure Display; override;
   procedure UpdateKeyState; override;
   function  WaitKey : Byte; override;
end;

implementation

constructor TSDLScreen.Create (S : Integer);
begin
   inherited Create;

   SDL_Init (SDL_INIT_VIDEO);

   Scale := S;

   DisplayScreen := SDL_SetVideoMode (ScreenWidth * Scale,
                                      ScreenHeight * Scale,
                                      32, SDL_SWSURFACE);

   if DisplayScreen = nil then
      raise SDLException.Create ('SDL Init failed!');
end;

destructor TSDLScreen.Destroy;
begin
   inherited Destroy;

   SDL_FreeSurface (DisplayScreen);
   SDL_Quit;
end;

procedure TSDLScreen.Display;
var
   I, J : Integer;
   SX, SY : Integer;
   P : LongWord;
begin
   for I := 0 to ScreenWidth - 1 do
      for J := 0 to ScreenHeight - 1 do
      begin
         if Self.Screen [I][J] <> 0 then P := $00FF00FF else P := $222222FF;
         SX := I * Scale;
         SY := J * Scale;

         BoxColor (DisplayScreen, SX, SY, SX + Scale, SY + Scale, P);
      end;
   SDL_Flip (DisplayScreen);
end;


function TSDLScreen.HandleEvent (event : pSDL_Event) : Integer;
begin
   case event^.type_ of
      SDL_KEYDOWN:
         begin
            { 27 = Escape }
            if event^.key.keysym.sym = 27 then
               raise QuitException.Create ('Quit');
            // TODO
            Result := 0;
         end;

      SDL_KEYUP:
         begin
            // TODO
            Result := 1;
         end;

      SDL_QUITEV: raise QuitException.Create ('Quit');
   end; // case event

   Result := -1;
end;

procedure TSDLScreen.UpdateKeyState;
var
   Event : pSDL_Event;
begin
   new (Event);

   while SDL_PollEvent(event) = 1 do
      HandleEvent (event);

   dispose (Event);
end;

// Blocking, waits for a key press
function TSDLScreen.WaitKey : Byte;
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

end.
