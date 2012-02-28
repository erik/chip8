all:
	fpc -g chip8.pas

clean:
	rm -f *.o *.ppu chip8
