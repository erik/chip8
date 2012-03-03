unit sdlscreen;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, SDL, SDL_gfx, Screen;

type TSDLScreen = class (TScreen)
private
   Width, Height  : Integer;
   ScaleW, ScaleH : Real;
   DisplayScreen  : pSDL_Surface;
   function HandleEvent (event : pSDL_Event) : KeyPress;
   procedure HandleResize (Event : pSDL_Event);

protected
   function PollKey : Screen.KeyPress; override;
   function WaitKey : Screen.KeyPress; override;
public
   constructor Create (W, H : Integer); reintroduce;
   destructor Destroy; override;

   procedure Display; override;
   procedure SetLow;  override;
   procedure SetHigh; override;
end;

implementation

constructor TSDLScreen.Create (W, H : Integer);
begin
   SDL_Init (SDL_INIT_VIDEO);

   ScaleW := W / Chip8Width;
   ScaleH := H / Chip8Height;

   DisplayScreen := SDL_SetVideoMode (W, H, 32, SDL_SWSURFACE);

   if DisplayScreen = nil then
      raise SDLException.Create ('SDL Init failed!');

   inherited Create;
end;

destructor TSDLScreen.Destroy;
begin
   inherited Destroy;

   SDL_FreeSurface (DisplayScreen);
   SDL_Quit;
end;

procedure TSDLScreen.Display;
var
   X, Y   : Integer;
   SX, SY : LongWord;
   P      : LongWord;
begin
   for X := 0 to Self.ScreenWidth do
      for Y := 0 to Self.ScreenHeight do
      begin
         if Self.Screen [X][Y] then P := $FFFFFFFF else P := $70;
         SX := Round (X * ScaleW);
         SY := Round (Y * ScaleH);

         BoxColor (DisplayScreen, SX, SY, Round (SX + ScaleW), Round (SY + ScaleH), P);
      end;

   SDL_Flip (DisplayScreen);
end;


function TSDLScreen.HandleEvent (Event : pSDL_Event) : KeyPress;
begin
   Result.KeyType := None;

   case Event^.Type_ of
      SDL_KEYDOWN: Result.KeyType := Press;
      SDL_KEYUP:   Result.KeyType := Release;
      SDL_VIDEORESIZE: HandleResize (Event);
      SDL_QUITEV:  raise QuitException.Create ('Quit');
   end;

   if Result.KeyType <> None then
      with Result do
         case Event^.Key.Keysym.Sym of
            SDLK_Escape      : raise QuitException.Create ('Quit');
            SDLK_p           : if Event^.Type_ = SDL_KEYDOWN then IsPaused := not IsPaused;
            SDLK_KP0         : Key := 0;
            SDLK_KP1         : Key := 1;
            SDLK_KP2         : Key := 2;
            SDLK_KP3         : Key := 3;
            SDLK_KP4         : Key := 4;
            SDLK_KP5         : Key := 5;
            SDLK_KP6         : Key := 6;
            SDLK_KP7         : Key := 7;
            SDLK_KP8         : Key := 8;
            SDLK_KP9         : Key := 9;
            SDLK_KP_PERIOD   : Key := $A;
            SDLK_KP_ENTER    : Key := $B;
            SDLK_KP_PLUS     : Key := $C;
            SDLK_KP_MINUS    : Key := $D;
            SDLK_KP_MULTIPLY : Key := $E;
            SDLK_KP_DIVIDE   : Key := $F;
         else KeyType := None;
         end;
end;

procedure TSDLScreen.HandleResize (Event : pSDL_Event);
begin
   DisplayScreen := SDL_SetVideoMode (Event^.Resize.W,
                                      Event^.Resize.H, 32,
                                      SDL_SWSURFACE or SDL_RESIZABLE);

   Width  := Event^.Resize.W;
   Height := Event^.Resize.H;

   if DisplayScreen = nil then raise SDLException.Create ('Resize failed.');

   case Mode of
      Extended: begin
                   ScaleW := Width  / SChip8Width;
                   ScaleH := Height / SChip8Height;
                end;
      Normal:   begin
                   ScaleW := Width  / Chip8Width;
                   ScaleH := Height / Chip8Height;
                end;
   end;

   Display;
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
{$GOTO OFF}

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

procedure TSDLScreen.SetHigh;
begin
   inherited SetHigh;

   ScaleW := Width  / SChip8Width;
   ScaleH := Height / SChip8Height;
end;

procedure TSDLScreen.SetLow;
begin
   inherited SetLow;

   ScaleW := Width  / Chip8Width;
   ScaleH := Height / Chip8Height;
end;

end.
