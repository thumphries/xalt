{ mkDerivation, base, config-value, containers, directory, filepath
, gi-gtk, gi-gtk-hs, gtk3, microlens, mtl, optparse-applicative
, pretty-show, stdenv, taffybar, text, transformers, X11, xmonad
, xmonad-contrib, lib
}:
mkDerivation {
  pname = "xalternative";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base config-value containers directory filepath microlens taffybar
    text transformers X11 xmonad xmonad-contrib
  ];
  executableHaskellDepends = [
    base gi-gtk gi-gtk-hs gtk3 mtl optparse-applicative pretty-show
    taffybar text transformers
  ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = lib.licenses.bsd3;
}
