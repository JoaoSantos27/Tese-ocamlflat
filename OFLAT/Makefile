APP = OFLAT
VERSION = 3.0
COMP = ocamlc
SRC_DIR = src
APP_DIR = $(APP)
ifneq ("$(wildcard /run/user/1000)","")
	RAM_DIR = /run/user/1000
	OBJ_DIR = $(RAM_DIR)/RAMDISK/obj
else
	OBJ_DIR = obj
endif
FLAGS = -w -8 -g
PACKAGES = js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-lwt,js_of_ocaml-tyxml
OCAMLFLAT = OCamlFlat
OCAMLFLAT_BASE = ../$(OCAMLFLAT)
OCAMLFLAT_LIB = $(OCAMLFLAT_BASE)/lib/$(OCAMLFLAT).ml
NAMES = OCamlFlat Lang JS ViewUtil Listeners Cytoscape				\
		AutomatonView												\
		FiniteAutomatonView RegularExpressionView TransducerView	\
		StateVariables												\
		ContextFreeGrammarBasicView ContextFreeGrammarLL1View		\
		ContextFreeGrammarLRView ContextFreeGrammarView				\
		PushdownAutomatonView TuringMachineView	CompositionView		\
		GrammarView													\
		HtmlPageClient												\
		Controller AutomatonController								\
		FiniteAutomatonController RegularExpressionController		\
		TransducerController ContextFreeGrammarController PushdownAutomatonController	\
		TuringMachineController ExerciseController					\
		GrammarController 											\
		CompositionController ControllerListeners Calling
define OBJS_CMO
	$(addsuffix .cmo, $(foreach file, $(NAMES), $(file)))
endef
define SRCS_ML
	$(addsuffix .ml, $(foreach file, $(NAMES), $(file)))
endef
define OBJS
	$(addprefix $(OBJ_DIR)/, $(OBJS_CMO))
endef
define SRCS
	$(addprefix $(SRC_DIR)/, $(SRCS_ML))
endef

OS = $(shell uname -s)
ifeq ($(OS),Linux)
    ifneq ($(shell command -v brave-browser),)
        OPEN_BROWSER = "brave-browser"
	else ifneq ($(shell command -v chromium),)
		OPEN_BROWSER = "chromium"
	else ifneq ($(shell command -v chrome),)
		OPEN_BROWSER = "chrome"
	else ifneq ($(shell command -v firefox),)
		OPEN_BROWSER = "firefox"
	else ifneq ($(shell command -v epiphany),)
		OPEN_BROWSER = "epiphany"
	else
		OPEN_BROWSER = "x-www-browser"
	endif
else
	OPEN_BROWSER = "open"
endif

$(APP_DIR)/$(APP).js: $(OBJ_DIR)/$(APP).byte
	@echo "BUILDING" $@
	@js_of_ocaml --pretty --no-inline --debug-info --source-map $(APP_DIR)/GraphLibrary.js $< -o $@

$(OBJ_DIR)/$(APP).byte: $(OBJS)
	@echo "LINKING" $(APP).byte
	@cd $(OBJ_DIR) && ocamlfind $(COMP) $(FLAGS) -package $(PACKAGES) -linkpkg -o $(APP).byte $(OBJS_CMO)

$(OBJS): $(SRCS)
	@echo "COMPILING" $(SRC_DIR)/\*.ml
ifdef RAM_DIR	
	@cp -a $(SRC_DIR)/*.ml $(OBJ_DIR)
	@cd $(OBJ_DIR) && ocamlfind $(COMP) $(FLAGS) -I $(OBJ_DIR) -package $(PACKAGES) -c $(SRCS_ML)
else	
	@ocamlfind $(COMP) $(FLAGS) -I $(SRC_DIR) -package $(PACKAGES) -c $(SRCS) || rm -f $(SRC_DIR)/*.cm[io]
	@mv $(SRC_DIR)/*.cm[io] $(OBJ_DIR)
endif

#$(OBJ_DIR)/%.cmo $(OBJ_DIR)/%.cmi: src/%.ml
#	@echo "COMPILING" $<
#	@ocamlfind $(COMP) $(FLAGS) -I $(OBJ_DIR) -package $(PACKAGES) -c $<
#	@mv $(SRC_DIR)/*.cm[io] $(OBJ_DIR)

$(OCAMLFLAT_LIB):
	@echo "BUILDLING" $@
	@cd $(OCAMLFLAT_BASE) && $(MAKE)

$(SRC_DIR)/$(OCAMLFLAT).ml: $(OCAMLFLAT_LIB)
	@echo "INSTALLING" $@
	@cp -a $(OCAMLFLAT_LIB) $@

$(OBJS): | $(OBJ_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

.PHONY: run
run: $(APP_DIR)/$(APP).js
	@$(OPEN_BROWSER) $(APP_DIR)/index.html > /dev/null 
	@echo "OPENED $(APP) in the browser"

.PHONY: runl
runl: $(APP_DIR)/$(APP).js
	@$(OPEN_BROWSER) "file://""`pwd`""/$(APP_DIR)/index.html?logging" > /dev/null &
	@echo "OPENED $(APP) in the browser (using active logging)"

.PHONY: runl2
runl2: $(APP_DIR)/$(APP).js
	@$(OPEN_BROWSER) "file://""`pwd`""/$(APP_DIR)/index.html?logging2" > /dev/null &
	@echo "OPENED $(APP) in the browser (using active logging)"

.PHONY: edit
edit:
	@cd $(SRC_DIR) ; geany ../$(APP).geany &

.PHONY: editreset
editreset:
	cp $(OCAMLFLAT_BASE)/geany.template $(APP).geany
	sed -i -e "s/PROJECT_NAME/$(APP)/" $(APP).geany
	@echo PLEASE, activate the plugins "File Browser" and "Split Window"; sleep 3
	$(MAKE) edit

.PHONY: clean
clean:
	rm -rf $(APP_DIR)/$(APP).* $(OBJ_DIR) DIST

.PHONY: cleanfull
cleanfull: clean
	rm -f $(SRC_DIR)/$(OCAMLFLAT).ml

.PHONY: dist
dist: clean
	rm -rf DIST; mkdir -p DIST/$(APP_DIR)
	cp -a LICENCE Makefile README.md $(APP_DIR).geany src $(APP_DIR).makefile $(APP_DIR) DIST/$(APP_DIR)
	cd DIST; tar cpvz $(APP_DIR) > $(APP_DIR)-$$(date +%y%m%d).tgz
	@cd DIST; rm -rf $(APP_DIR); find "$${PWD}"/*

.PHONY: ram
ram:
ifdef RAM_DIR	
	@echo $(RAM_DIR)
else
	@echo "UNAVAILABLE"
endif

.PHONY: update
update: $(APP_DIR)/$(APP).js
	mkdir ~/www/leafs/$(APP) && cp -a $(APP)/* ~/www/leafs/$(APP)

.PHONY: git0
git0: clean
	git fetch
	git status

.PHONY: git1
git1: clean
	git fetch
	git diff

.PHONY: git2
git2: clean
	git fetch
	git add src/*.ml $(APP)/* Makefile
	git status

.PHONY: git3
git3: clean
	git fetch
	git commit
	git status

.PHONY: git4
git4:
	git push origin master

.PHONY: gitp
gitp:
	git pull

