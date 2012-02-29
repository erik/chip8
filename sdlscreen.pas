unit sdlscreen;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, SDL, SDL_gfx, Screen;

type TSDLScreen = class (TScreen)
private
   Scale         : Integer;
   DisplayScreen : pSDL_Surface;
   function HandleEvent (event : pSDL_Event) : KeyPress;

protected
   function PollKey : Screen.KeyPress; override;
   function WaitKey : Screen.KeyPress; override;
public
   constructor Create (S : Integer); reintroduce;
   destructor Destroy; override;

   procedure Display; override;
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
   I, J   : Integer;
   SX, SY : Integer;
   P      : LongWord;
begin
   for I := 0 to ScreenWidth - 1 do
      for J := 0 to ScreenHeight - 1 do
      begin
         if Self.Screen [I][J] then P := $00FF00FF else P := $222222FF;
         SX := I * Scale;
         SY := J * Scale;

         BoxColor (DisplayScreen, SX, SY, SX + Scale, SY + Scale, P);
      end;
   SDL_Flip (DisplayScreen);
end;


function TSDLScreen.HandleEvent (Event : pSDL_Event) : KeyPress;
begin
   Result.KeyType := None;

   case Event^.Type_ of
      SDL_KEYDOWN: Result.KeyType := Press;
      SDL_KEYUP:   Result.KeyType := Release;
      SDL_QUITEV:  raise QuitException.Create ('Quit');
   end;

   if Result.KeyType <> None then
      with Result do
         case Event^.Key.Keysym.Sym of
            SDLK_Escape : raise QuitException.Create ('Quit');
            SDLK_KP0    : Key := 0;
            SDLK_KP1    : Key := 1;
            SDLK_KP2    : Key := 2;
            SDLK_KP3    : Key := 3;
            SDLK_KP4    : Key := 4;
            SDLK_KP5    : Key := 5;
            SDLK_KP6    : Key := 6;
            SDLK_KP7    : Key := 7;
            SDLK_KP8    : Key := 8;
            SDLK_KP9    : Key := 9;
            SDLK_KP_PERIOD   : Key := $A;
            SDLK_KP_ENTER    : Key := $B;
            SDLK_KP_PLUS     : Key := $C;
            SDLK_KP_MINUS    : Key := $D;
            SDLK_KP_MULTIPLY : Key := $E;
            SDLK_KP_DIVIDE   : Key := $F;
         else KeyType := None;
         end;
end;

{$GOTO ON}
function TSDLScreen.PollKey : KeyPress;
var
   Event : pSDL_Event;
   label Retry;
begin
   new (Event);

   Result.KeyType := None;

   Retry :
      if SDL_PollEvent(Event) = 1 then
         if (Event^.Type_ <> SDL_KEYDOWN) and
               (Event^.Type_ <> SDL_KEYUP) and
               (Event^.Type_ <> SDL_QUITEV) then
            goto Retry
         else Result := HandleEvent (Event);

   dispose (Event);
end;

function TSDLScreen.WaitKey : KeyPress;
var
   Event : pSDL_Event;
begin
   new (Event);
   repeat
      SDL_WaitEvent (Event);
      Result := HandleEvent (Event);
   until Result.KeyType <> None;

   dispose (Event);
end;

end.
