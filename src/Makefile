.PHONY = clean builddir debug

OCAMLBUILD = ocamlbuild -build-dir ../build -quiet

all: debug

clean:
	rm -rf ../build

builddir:
	@mkdir ../build 2> /dev/null || true

debug: builddir
	$(OCAMLBUILD) main.d.byte

doc: builddir
	$(OCAMLBUILD) ../build/doc.docdir/index.html
