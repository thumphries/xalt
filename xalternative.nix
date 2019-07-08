{ mkDerivation, async, base, bytestring, chronos, clock
, config-value, containers, dbus, directory, gi-gtk, gi-gtk-hs
, gtk3, microlens, mtl, optparse-applicative, serialise, stdenv
, stm, taffybar, text, transformers, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xalternative";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring chronos clock config-value containers dbus
    directory gi-gtk gi-gtk-hs gtk3 microlens serialise stm taffybar
    text transformers X11 xmonad xmonad-contrib
  ];
  executableHaskellDepends = [
    async base chronos dbus gi-gtk gi-gtk-hs gtk3 mtl
    optparse-applicative stm taffybar text transformers
  ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = stdenv.lib.licenses.bsd3;
}
