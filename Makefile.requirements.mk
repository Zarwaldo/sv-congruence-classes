# $Id: f9b9cd0217e24742fe4744ac03e5459863acb16a $

# Error message for missing build requirements
REQERR = See README for build requirements

# Check for menhir
ifneq ($(shell menhir --version | sed -e 's/,.*//'), menhir)
  $(error Check for menhir failed.  $(REQERR))
endif

# Check for OCaml compiler >= 4.07 (deprecate Num)
req = 4.07
ifneq ($(shell (v=$$(ocamlc -version) ; echo $$v ; echo $(req)) | sort -V | head -n 1), $(req))
  $(error Check for ocamlc version >= $(req) failed.  $(REQERR))
endif

# Check for Zarith >= 1.10 (function divisible)
req = 1.10
ifneq ($(shell (v=$$(ocamlfind query -format "%v" zarith) ; echo $$v ; echo $(req)) | sort -V | head -n 1), $(req))
  $(error Check for zarith version >= $(req) failed.  $(REQERR))
endif

ifeq ($(BUILDSYSTEM), ocamlbuild)
  # Check for ocamlbuild
  ifneq ($(shell ocamlbuild -version | sed -e 's/ .*//'), ocamlbuild)
    $(error Check for ocamlbuild failed.  $(REQERR))
  endif
else ifeq ($(BUILDSYSTEM), dune)
  # Check for dune >= 2.9
  req = 2.9
  ifneq ($(shell (v=$$(dune --version) ; echo $$v ; echo $(req)) | sort -V | head -n 1), $(req))
    $(error Check for dune version >= $(req) failed.  $(REQERR))
  endif
  # Check for odoc >= 2.0
  req = 2.0
  ifneq ($(shell (v=$$(odoc --version) ; echo $$v ; echo $(req)) | sort -V | head -n 1), $(req))
    $(error Check for odoc version >= $(req) failed.  $(REQERR))
  endif
else
  $(error Invalid value for variable BUILDSYSTEM: $(BUILDSYSTEM))
endif
