#
#
# wget http://ctp.di.fct.unl.pt/LEAFS/OFLAT-more/OFLAT.makefile
#
# Learn-OCaml+OFLAT installer 
#
# This file is a quick hack that probably will be superseded by a better solution.
#
# How to use this makefile? Via the command line interface, firstly you create one empty directory
# (for exemple named "learn-ocaml") and place this makefile inside. Move into that directory
# using "cd".
#
# Now, the following three commands can be issued:
#
#	make -f OFLAT.makefile install
#	  - deploy a Learn-OCaml instance (takes 15 minutes of time and takes 2GB on the disk)
#	  - patch the code with support for external tools
#	  - create and initial repository containing some OFLAT examples
#	  - install the OFLAT xtool
#	  - everything becomes ready to use
#
#	make -f OFLAT.makefile serve
#	  - run the Learn-OCaml web server 
#
#	make -f OFLAT.makefile run	(a 2nd opened terminal is required)
#	  - open Learn-OCaml in a tab on the web browser
#

LEARN = learn-ocaml
SRC = $(LEARN)/src/app
XTOOLS = $(LEARN)/xtools
STATIC = www/static
REP = my-learn-ocaml-repository

OS = $(shell uname -s)
ifeq ($(OS),Linux)
	BROWSER = "x-www-browser"
else
	BROWSER = "open"
endif

install: check_opam $(LEARN) $(LEARN)/_opam $(SRC)/learnocaml_bridge.ml \
			compile repository $(XTOOLS)/OFLAT 

.PHONY: serve
serve: repository
	cd $(LEARN) && eval $$(opam env) && cd .. && learn-ocaml serve

.PHONY: serve2
serve2: repository
	cd $(LEARN) && eval $$(opam env) && cd .. && learn-ocaml serve >> SERVER.OUT 2>&1 &

.PHONY: pid
pid:
	@pidof learn-ocaml-server

.PHONY: kill
kill:
	@kill -9 $$(pidof learn-ocaml-server)

.PHONY: run
run:
	$(BROWSER) http://localhost:8080

.PHONY: version
version:
	@echo "opam" $(shell opam --version)
	@cd $(LEARN) && eval $$(opam env) && ocaml --version

.PHONY: check_opam
check_opam:
ifeq (, $(shell opam --version 2> /dev/null))
	@echo "*** ERROR: opam is missing!!!"
	exit 1
endif

$(LEARN):
	@echo "\n**************************************"
	@echo "*** DEPLOYING A LEARN-OCAML INSTANCE"
	git clone --branch release-v0.13.2 https://github.com/ocaml-sf/learn-ocaml.git

$(LEARN)/_opam:
	@echo "\n**************************************************************"
	@echo "*** INSTALLING THE DEPENDENCIES (THIS WILL TAKE A LONG TIME...)"
	cd $(LEARN) && eval $$(opam env) \
			&& opam switch create . --deps-only --locked --yes

$(SRC)/learnocaml_bridge.ml: $(LEARN)/_opam
	@echo "\n**************************************"
	@echo "*** PATCHING THE SOURCE CODE"
	mv $(SRC)/dune $(SRC)/dune-ori
	mv $(SRC)/learnocaml_exercise_main.ml $(SRC)/learnocaml_exercise_main-ori.ml
	rm -f $(SRC)/learnocaml_bridge.ml $(SRC)/learnocaml_bridge_oflat.ml 
	wget -P $(SRC) https://raw.githubusercontent.com/git-amd/learn-ocaml/master/src/app/learnocaml_bridge.ml
	wget -P $(SRC) https://raw.githubusercontent.com/git-amd/learn-ocaml/master/src/app/learnocaml_bridge_oflat.ml
	wget -P $(SRC) https://raw.githubusercontent.com/git-amd/learn-ocaml/master/src/app/learnocaml_exercise_main.ml
	wget -P $(SRC) https://raw.githubusercontent.com/git-amd/learn-ocaml/master/src/app/dune

.PHONY: compile
compile: $(LEARN)/_opam $(SRC)/learnocaml_bridge.ml
	@echo "\n**************************************"
	@echo "*** COMPILING"
	cd $(LEARN) && eval $$(opam env) \
			&& make && make opaminstall 2>&1 | grep -v "Deleting\|Installing"

$(REP):
	@echo "\n**************************************"
	@echo "*** CREATING THE INITIAL REPOSITORY"
	cp -fr $(LEARN)/demo-repository $(REP)
	rm -rf $(REP)/exercises
	wget -q -nd -r -P $(REP) http://ctp.di.fct.unl.pt/LEAFS/OFLAT-more/exercises.tgz
	cd $(REP) ; tar zxf exercises.tgz ; rm exercises.tgz

.PHONY: repository
repository: $(REP)
	@echo "\n**************************************"
	@echo "*** BUILDING THE REPOSITORY"
	cd $(LEARN) && eval $$(opam env) && cd .. \
			&& learn-ocaml build -v --repo $(REP)

$(XTOOLS)/OFLAT:
	@echo "\n**************************************"
	@echo "*** INSTALLING THE OFLAT XTOOL"
	wget -q -nd -r -P $(XTOOLS)/OFLAT http://ctp.di.fct.unl.pt/LEAFS/OFLAT/
	ln -sf ../../$(XTOOLS) $(STATIC)

.PHONY: update
update:
	@echo "\n**************************************"
	@echo "*** REINSTALLING THE OFLAT XTOOL"
	rm -rf $(XTOOLS)/OFLAT
	wget -q -nd -r -P $(XTOOLS)/OFLAT http://ctp.di.fct.unl.pt/LEAFS/OFLAT/
	ln -sf ../../$(XTOOLS) $(STATIC)
	wget -q http://ctp.di.fct.unl.pt/LEAFS/OFLAT-more/OFLAT.makefile -O OFLAT.makefile
	cp OFLAT.makefile Makefile
	wget -q http://ctp.di.fct.unl.pt/LEAFS/OFLAT-more/exercises.tgz -O exercises.tgz
	rm -rf exercices ; tar zxf exercises.tgz ; rm -f exercises.tgz

.PHONY: clean
clean:
	rm -rf www $(REP) $(XTOOLS)/OFLAT
#	rm -f $(SRC)/learnocaml_bridge.ml $(SRC)/learnocaml_bridge_oflat.ml
#	mv $(SRC)/dune-ori $(SRC)/dune
#	mv $(SRC)/learnocaml_exercise_main-ori.ml $(SRC)/learnocaml_exercise_main.ml



# http://localhost:8080/learn-ocaml/static/xtools/OFLAT/index.html?learn-ocaml
# http://193.136.122.94/learn-ocaml/static/xtools/OFLAT/index.html?learn-ocaml
# http://193.136.122.94/learn-ocaml/

