{ mkDerivation, async, base, chronos, clock, config-value
, containers, directory, gi-gtk, gi-gtk-hs, gtk3, microlens, mtl
, optparse-applicative, stdenv, taffybar, text, torsor
, transformers, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xalternative";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base config-value containers directory microlens taffybar text
    transformers X11 xmonad xmonad-contrib
  ];
  executableHaskellDepends = [
    async base chronos clock gi-gtk gi-gtk-hs gtk3 mtl
    optparse-applicative taffybar text torsor transformers
  ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = stdenv.lib.licenses.bsd3;
}
