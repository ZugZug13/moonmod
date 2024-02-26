default: all

all:
	sjasmplus --nologo --msg=war moonmod.asm
	sjasmplus --nologo --msg=war -DFAST_CPU=1 moonmod.asm

clean:
	-rm -f ../bin/moonmod.com
	-rm -f ../bin/moonmodt.com
