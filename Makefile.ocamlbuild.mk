# $Id: ba0654eaa99646693fb7a4ccb41fa7b22f3e7e44 $

# Configuration
BUILD_FLAGS = -use-ocamlfind -use-menhir
DOC_FLAGS   = -charset UTF-8, -colorize-code, -sort

%.native %.byte %.cmo %.inferred.mli: FORCE
	ocamlbuild $(BUILD_FLAGS) $@

doc.odocl: FORCE
	for m in lib/*.mli test/lib/*.mli; do \
	    t=$${m##*lib/} && echo $${t%%.mli}; \
	done > $@

doc: FORCE doc.odocl
	ocamlbuild $(BUILD_FLAGS) -docflags '$(DOC_FLAGS)' \
	    doc.docdir/index.html > doc.log
	mv doc.docdir doc

clean: FORCE
	ocamlbuild -quiet -clean
	rm -f doc.odocl doc.log
