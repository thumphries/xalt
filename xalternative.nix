{ mkDerivation, base, config-value, containers, directory, gtk3
, microlens, optparse-applicative, stdenv, taffybar, text
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
    base gtk3 optparse-applicative taffybar
  ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = stdenv.lib.licenses.bsd3;
}
