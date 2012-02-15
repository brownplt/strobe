.PHONY = debug prof opt clean
OCAMLBUILD = ocamlbuild -build-dir ../obj -quiet  -cflags -annot -menhir "menhir --stdlib \"C:\cygwin\usr\local\share\menhir\""

$(shell mkdir -p ../obj)

all: 
	$(OCAMLBUILD) main.d.byte
	ln -sf obj/main.d.byte ../run

prof:
	$(OCAMLBUILD) main.p.native
	ln -sf obj/main.p.native ../run

opt:
	$(OCAMLBUILD) main.native
	ln -sf obj/main.native ../run

clean:
	rm -rf ../run ../obj
