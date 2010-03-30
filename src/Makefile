.PHONY = clean builddir debug

OCAMLBUILD = ocamlbuild -build-dir ../build -quiet

all: debug

clean:
	rm -rf ../build

builddir:
	@mkdir ../build 2> /dev/null || true

debug: builddir
	$(OCAMLBUILD) main.d.byte

prof: builddir
	$(OCAMLBUILD) main.p.native

cfa: builddir
	$(OCAMLBUILD) cfa.d.byte

doc: builddir
	$(OCAMLBUILD) doc.docdir/index.html
