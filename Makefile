DIRS=util,x86

main.native:
	ocamlbuild -Is $(DIRS) main.native

main.byte:
	ocamlbuild -Is $(DIRS) main.byte

all: main.native

.PHONY: clean
clean:
	ocamlbuild -clean
