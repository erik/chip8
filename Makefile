FLAGS=
DFLAGS= -g -Cr -Co -Ct

all:
	fpc $(FLAGS) chip8.pas

debug:
	fpc $(DFLAGS) chip8.pas

clean:
	rm -f *.o *.ppu chip8
