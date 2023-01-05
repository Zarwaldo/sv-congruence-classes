# $Id: c4068b8bf14abf03a5f5b671cc18b8076897a47a $

# Build system (either `ocamlbuild' or `dune')
BUILDSYSTEM = dune

# Main programs
PROGRAMS = sai

# Default target
all: byte

# Program targets
byte: FORCE $(PROGRAMS:%=%.byte)
native: FORCE $(PROGRAMS:%=%.native)

# Test targets
test: FORCE runtests.byte
	CAMLRUNPARAM="b" ./runtests.byte

# To select the test suite(s) to run (e.g., make test-domsign)
test-%: FORCE runtests.byte
	CAMLRUNPARAM="b" ./runtests.byte $*

FORCE:

include Makefile.requirements.mk
include Makefile.$(BUILDSYSTEM).mk
