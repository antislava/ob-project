FAST_TAGS_VER := $(shell fast-tags --version 2>/dev/null)

GIT_CACHE   = /r-cache/git

DIR = .
NIX_DEPS    = $(DIR)/nix-deps
NIX_DIR     = $(DIR)/nix

NIXPKGS_SRC = /github.com/NixOS/nixpkgs
NIXPKGS_JSN = $(NIX_DIR)/nixpkgs.git.json
# NIXPKGS_NIX = $(NIX_DIR)/nixpkgs.nix

OBELISK_SRC = /github.com/obsidiansystems/obelisk
OBELISK_JSN = $(NIX_DIR)/obelisk.git.json
# OBELISK_NIX = $(NIX_DIR)/obelisk.nix

REFLEX_SRC = /github.com/reflex-frp/reflex-platform
REFLEX_JSN = $(NIX_DIR)/reflex-platform.git.json

HDEPS       = $(DIR)/.haskdeps
HDEPS_ALL   = $(HDEPS)/all
HDEPS_GHC   = $(HDEPS)/ghc
HDEPS_GHCJS = $(HDEPS)/ghcjs
HDEPS_CORE  = $(HDEPS)/core
# TARGETS = "ps: [ ]"


# GHC shell post-initialisation commands
INIT_GHC   = "ghc-pkg list | head -1 | xargs | xargs -I {} ln -sf -T {} $(HDEPS)/package.conf.d.ghc; ls -1 $(HDEPS)/package.conf.d.ghc | sort > $(HDEPS)/package.conf.d.ghc.txt; ghc-pkg list --simple-output | tr ' ' '\n' | sort > $(HDEPS)/ghc-all.txt"
INIT_GHCJS = "ghcjs-pkg list | head -1 | xargs | xargs -I {} ln -sf {} $(HDEPS)/package.conf.d.ghcjs; ls -1 $(HDEPS)/package.conf.d.ghcjs | sort > $(HDEPS)/package.conf.d.ghcjs.txt; ghcjs-pkg list --simple-output | tr ' ' '\n' | sort > $(HDEPS)/ghcjs-all.txt"

INIT_GHC-RET   = $(INIT_GHC)"; return"
INIT_GHCJS-RET = $(INIT_GHC)"; return"


# DIR STRUCTURE INITIALISATION (ON MAKE PARSE)
# https://stackoverflow.com/questions/1950926/create-directories-using-make-file
DIRS = $(NIX_DIR) $(NIX_DEPS) $(HDEPS) $(HDEPS_ALL) $(HDEPS_CORE)

$(info $(shell mkdir -p $(DIRS)))

# https://stackoverflow.com/a/1951111
dir_guard = @mkdir -p $(@D) # currently not used but potentially useful


# DEFAULT make action
.PHONY : default
default:
	@echo "No default action - use specific make flags instead. (Directories initialised.)"


# NIX

git-init :
	git init
	git submodule add https://github.com/antislava/nix-utils

# Import expressions for key nix packages (based on git json files)
# make nix/obelisk.nix -B
# make nix/reflex-platform.nix -B
# make nix/nixpkgs.nix -B

$(NIX_DIR)/%.nix : $(NIX_DIR)/%.git.json
	echo -e "with builtins.fromJSON (builtins.readFile ./$(<F));\nbuiltins.fetchGit { inherit url rev; }" > $@

# make -B nix/obelisk.git.json to force update
$(NIXPKGS_JSN) :
	# Switch between the original nixpkgs at github or a local mirror:
	nix-prefetch-git https:/$(NIXPKGS_SRC) > $(NIXPKGS)
	# cd $(GIT_CACHE)$(NIXPKGS_SRC) && git fetch
	# nix-prefetch-git $(GIT_CACHE)$(NIXPKGS_SRC) > $(NIXPKGS_JSN)

# make -B nix/obelisk.git.json to force update
$(REFLEX_JSN) :
	# Switch between the original reflex-pltf at github or a local mirror:
	# nix-prefetch-git https:/$(REFLEX_SRC) > $(REFLEX_JSN)
	cd $(GIT_CACHE)$(REFLEX_SRC) && git fetch
	nix-prefetch-git $(GIT_CACHE)$(REFLEX_SRC) > $(REFLEX_JSN)

# make -B nix/obelisk.git.json to force update
$(OBELISK_JSN) :
	# Switch between the original obelisk at github or a local mirror:
	# nix-prefetch-git https:/$(OBELISK_SRC) > $(OBELISK_JSN)
	cd $(GIT_CACHE)$(OBELISK_SRC) && git fetch
	nix-prefetch-git $(GIT_CACHE)$(OBELISK_SRC) > $(OBELISK_JSN)
	# nix-prefetch-git --rev refs/heads/develop https:/$(OBELISK_SRC) > $(OBELISK_JSN)
	# nix-prefetch-git --rev refs/heads/is-allow-location-services-android https:/$(OBELISK_SRC) > $(OBELISK_JSN)


# IMPORT EXPRESSIONS FOR DEPENDENT (HASKELL) PACKAGES

# Example (autocompletion doesn't work unfortunately):
# make ./nix-deps/groundhog-ghcjs.nix
$(NIX_DEPS)/%.nix : $(NIX_DEPS)/%.sh
	sh $< > $@

# %.nix : %.sh
# 	sh $< > $@


# OBELISK (and other tools) SHELL

.PHONY : ob-install-global
ob-install-global : $(OBELISK) $(OBELISK_NIX)
	nix-env -f "<nixpkgs>" -i -E "f: (import (import ./nix/obelisk.nix) { }).command"

.PHONY : ob-install-local
ob-install-local : $(OBELISK) $(OBELISK_NIX)
	nix-build -E "(import (import ./nix/obelisk.nix) { }).command" -o ./.ob

.PHONY : shell-tools
shell-tools : $(OBELISK) $(OBELISK_NIX)
	# nix-shell -p "(import (import ./nix/obelisk.nix) { }).command" "haskellPackages.ghcWithPackages (ps: [ ps.fast-tags ])"
	nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.fast-tags ])"

ob-init :
	ob init

ob-upgrade-global-and-local :
	make nix/obelisk.nix -B
	make ob-install-global
	mkdir tmp && cd tmp && ob init && cp -a .obelisk .. && cd .. # && rm -rf tmp

# Small patches to files created by ob-init. Switching to obelisk.nix in nix folder (for greater transparency and control + compatibility with the installed ob executable)
ob-init-patch :
	# TODO: switch to git diff/apply
	patch default.nix -i default.nix.patch

ob-repl :
	# rm .ghc.environment.x86_64-linux-8.4.3
	rm -f .ghc.environment.*
	ob repl

ob-run :
	rm -f .ghc.environment.*
	ob run


# NIX_SHELL

# Not used but keeping it for the future
# assert-ghc-shell := $(shell if [ -z $(NIX_GHC) ]; then echo "GHC is not installed. Enter nix-shell script (e.g. make shell)"; exit 1; fi;)

shells-init :
	nix-shell -A shells.ghc   --run $(INIT_GHC)
	nix-shell -A shells.ghcjs --run $(INIT_GHCJS)

.PHONY : shell-ghc
# shell-ghc : nix-shell-check
shell-ghc :
ifndef NIX_GHC
	# @touch nix-shell-check
	nix-shell -A shells.ghc --command $(INIT_GHC-RET)
else
	$(error Already in GHC shell!)
endif

.PHONY : shell-ghcjs
# shell-ghc : nix-shell-check
shell-ghcjs :
ifndef NIX_GHCJS
	# @touch nix-shell-check
	nix-shell -A shells.ghcjs --command $(INIT_GHCJS-RET)
else
	$(error Already in GHCJS shell!)
endif

# # .PHONY : nix-shell-check
# # nix-shell-check : project.nix $(PKG-NIX) nix/* nix-deps/*
# nix-shell-check : project.nix nix/* nix-deps/*
# 	@echo "Some nix shell dependencies changed!"


# TAGS GENERATION

.FORCE:

# hasktags seems to have problems because of lazy IO. Switched to fast-tags
tags : .FORCE haskdeps $(HDEPS)/core
ifdef FAST_TAGS_VER
	mkdir -p $(HDEPS)/all
	mkdir -p $(HDEPS)/ghc
	mkdir -p $(HDEPS)/ghcjs
	cp -d $(HDEPS)/ghc/*   $(HDEPS)/all
	cp -d $(HDEPS)/ghcjs/* $(HDEPS)/all
	fast-tags -RL . $(HDEPS)/all -o tags
else
	$(error fast-tags not installed! Are you in the right shell?)
endif

# NOTE:
# Currently generating two directories for ghc and ghcjs, which greatly overlap, resulting in many redundancies in tags file!
haskdeps :
	nix-build nix/sources.nix -A sources -o $(HDEPS)/ghc --argstr compiler "ghc" --arg targets "ps: [ ps.common ps.frontend ps.backend ]"
	nix-build nix/sources.nix -A sources -o $(HDEPS)/ghcjs --argstr compiler "ghcjs" --arg targets "ps: [ ps.common ps.frontend ]"
# make doesn't like <(...) too much...
	ls -1 $(HDEPS)/ghc   > $(HDEPS)/ghc.txt
	ls -1 $(HDEPS)/ghcjs > $(HDEPS)/ghcjs.txt
	comm -2 -3 $(HDEPS)/ghc-all.txt   $(HDEPS)/ghc.txt   > $(HDEPS)/ghc-core.txt
	comm -2 -3 $(HDEPS)/ghcjs-all.txt $(HDEPS)/ghcjs.txt > $(HDEPS)/ghcjs-core.txt

$(HDEPS)/core :
	rm -rf $(HDEPS)/core
	mkdir  $(HDEPS)/core
	cat $(HDEPS)/ghc-core.txt $(HDEPS)/ghcjs-core.txt | sort | uniq | xargs -I P sh -c 'cabal get P -d /cabal-cache; ln -s /cabal-cache/P $(HDEPS)/core'

# $(HDEPS)/ghc-core :
# ifdef NIX_GHC
# 	rm -rf $(HDEPS)/ghc-core
# 	mkdir  $(HDEPS)/ghc-core
# 	cat $(HDEPS)/ghc-core.txt | xargs -I P sh -c 'cabal get P -d /cabal-cache; ln -s /cabal-cache/P $(HDEPS)/ghc-core'
# else
# 	$(error Not in GHC shell!)
# endif

# $(HDEPS)/ghcjs-core :
# ifdef NIX_GHCJS
# 	rm -rf $(HDEPS)/ghcjs-core
# 	mkdir  $(HDEPS)/ghcjs-core
# 	cat $(HDEPS)/ghcjs-core.txt | xargs -I P sh -c 'cabal get P -d /cabal-cache; ln -s /cabal-cache/P $(HDEPS)/ghcjs-core'
# else
# 	$(error Not in GHCJS shell!)
# endif


# CLEANING

.PHONY: clean-all
clean-all : clean-tmp clean-tags clean-build

.PHONY: clean-build
clean-build :
	# cabal clean
	cabal new-clean
	rm -r dist

.PHONY: clean-tags
clean-tags :
	rm -f  tags
	rm -rf $(HDEPS)

.PHONY: clean-tmp
clean-tmp :
	rm -f  .ghc.environment.*

