PACKAGE      = package
VERSION      = ` date "+%Y.%m%d%" `
RELEASE_DIR  = $(CURDIR)
RELEASE_FILE = $(PACKAGE)-$(VERSION)
ROOT         = $(RELEASE_DIR)/debian/xfunky/usr
FILES       := $(shell find -type f ! -name ".DS_Store")
CABAL        = $(HOME)/.cabal/bin/cabal


all:

clean:
	@ rm -RIf $(RELEASE_DIR)/dist

.PHONY: install
install:
	echo "Installing XFunky..."

	echo "Updating cabal package list..."
	$(CABAL) update       || return 1
	$(CABAL) sandbox init || return 1

	## comment out the lines bellow if not using xmobar
	#$(CABAL) install-deps xmobar || return 1
	#$(CABAL) install X11-xft hinotify || return 1
	#$(CABAL) --prefix=$(ROOT) install xmobar -fwith_xft -fwith_inotify --force-reinstalls || return 1

	$(CABAL) install --dependencies-only || return 1
	$(CABAL) --datadir=$(ROOT) --datasubdir="" --prefix=$(ROOT)  install || return 1
