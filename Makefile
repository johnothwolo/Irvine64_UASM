CC=link.exe
CFLAGS= /NOLOGO /SUBSYSTEM:CONSOLE /ENTRY:main /LIBPATH:"C:\Irvine" Irvine32.lib Kernel32.Lib User32.Lib 
WINE=wine

LDIR=lib
BDIR=bin

CURRDIR=$(pwd)
SRCS=$(wildcard *.asm)
BINS=$(addprefix $(BDIR)/,$(SRCS:.asm=))

all: $(BINS)

$(BDIR)/%.exe: $(BDIR)/%.obj#$(BDIR)/%: $(BDIR)/%.obj
	cd $(BDIR)
	$(WINE) lib/$(CC) $(CFLAGS) $< /OUT:$@

$(BDIR)/%.obj : $(BDIR)/%.asm
	$(WINE) ml -Fo $@ -nologo -c -coff -Zi $<

$(BDIR)/%.asm: %.asm
	mkdir -p $(BDIR)
	cp $< $@

clean:
	rm -Rf $(BDIR)

.PHONY: all clean
