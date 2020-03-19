# Maintainer: Edgar Gomes <talktoedgar(a|t)gmail(.)com>

pkgname=xfunky
pkgver=1.0
pkgrel=1
pkgdesc='Customized tiling manager for X with KDE4 integration.'
arch=('any')
url=''
license=('GPL2')
depends=('pkg-config' 'libxft' 'libx11' 'libxinerama' 'libxrandr' 'libxml2' )
makedepends=('ghc' 'cabal-install')
backup=()
options=()
install=
source=()
noextract=()
md5sums=()
conflicts=()
replaces=()

package () {
  cd "${startdir}"
  cabal install --datadir="${pkgdir}/usr" --datasubdir="" --prefix="${pkgdir}/usr"
  cabal install --datadir="${pkgdir}/usr" --datasubdir="" --prefix="${pkgdir}/usr" xmobar -fwith_xft
  chmod +x "${pkgdir}/usr/bin/kwintoxmd"
}

build () {
  cd "${startdir}"
  cabal sandbox init
  cabal install --only-dependencies
  cabal build
}
