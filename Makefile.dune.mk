# $Id: 225cc366680273c96be2da96bbbc8c524701c707 $

$(PROGRAMS:%=%.byte): FORCE opam
	dune build bin/$(@:%.byte=%.bc)
	ln -sf _build/default/bin/$(@:%.byte=%.bc) $@

$(PROGRAMS:%=%.native): FORCE opam
	dune build bin/$(@:%.native=%.exe)
	ln -sf _build/default/bin/$(@:%.native=%.exe) $@

runtests.byte: FORCE opam
	dune build test/bin/$(@:%.byte=%.bc)
	ln -sf _build/default/test/bin/$(@:%.byte=%.bc) $@

%.cmo: FORCE opam
	dune build %{cmo:lib/$(@:%.cmo=%)}

%.opam:
	touch $@

doc: FORCE opam
	dune build @doc 2> doc.log
	ln -sf _build/default/_doc/_html $@

opam: spa.opam spatest.opam

clean: FORCE
	dune clean
	rm -f $(PROGRAMS:%=%.byte) $(PROGRAMS:%=%.native) runtests.byte \
	    doc doc.log spa.opam spatest.opam
