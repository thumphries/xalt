{ mkDerivation, async, base, chronos, clock, config-value
, containers, dbus, directory, gi-gtk, gi-gtk-hs, gtk3, microlens
, mtl, optparse-applicative, stdenv, stm, taffybar, text
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
    async base chronos clock dbus gi-gtk gi-gtk-hs gtk3 mtl
    optparse-applicative stm taffybar text transformers
  ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = stdenv.lib.licenses.bsd3;
}
