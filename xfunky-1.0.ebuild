# Copyright 1999-2013 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: /var/cvsroot/gentoo-x86/dev-haskell/hdbc-postgresql/hdbc-postgresql-2.3.2.2.ebuild,v 1.1 2013/11/15 17:46:56 slyfox Exp $

EAPI=0

# project is hosted on github.com, so git-2 is needed (git is deprecated)
inherit git-2 haskell-cabal

DESCRIPTION="Tiling windows manager (XMonad) for KDE4"
HOMEPAGE=""

EGIT_REPO_URI="https://github.com/lostbean/xfunky.git"
SRC_URI=""

MY_P="xfunky"
LICENSE="BSD"
SLOT="0"
KEYWORDS="~amd64 ~x86"

RDEPEND="
	x11-libs/libX11
"
DEPEND="${RDEPEND}
	>=dev-lang/ghc-6.12.1
	>=x11-wm/xmonad-0.10
    >=x11-wm/xmonad-contrib-0.10
    dev-haskell/containers
	dev-haskell/process
	>=dev-haskell/utf8-string-0.3
	>=dev0haskell/dbus-0.10
	>=dev-haskell/cabal-1.8.0.2"

CABAL_EXTRA_CONFIGURE_FLAGS=" --datadir=/usr --datasubdir="

src_install() {
        cabal_src_install
}
