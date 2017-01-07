DIRS=util,x86
LIBS=nums
main.native:
	ocamlbuild -Is $(DIRS) -lib $(LIBS) main.native

main.byte:
	ocamlbuild -Is $(DIRS) -lib $(LIBS) main.byte

all: main.native

.PHONY: clean
clean:
	ocamlbuild -clean
