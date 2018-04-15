{ mkDerivation, base, config-value, containers, lens-family-core
, optparse-applicative, stdenv, text, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xalternative";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base config-value containers lens-family-core text X11 xmonad
    xmonad-contrib
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = stdenv.lib.licenses.bsd3;
}
