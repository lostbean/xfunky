{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
   name ="xfunky-1.0.0";
   src = ./xmonad;
   buildInputs = [
      glpk pcre
      pkg-config
      dbus
      libdbusmenu
      xorg.libX11
      xorg.libXrandr
      xorg.libXinerama
      xorg.libXScrnSaver
      xorg.libXext
      xorg.libXft
      haskellPackages.alex
      haskellPackages.happy
   ];
   inherit ghc;
}