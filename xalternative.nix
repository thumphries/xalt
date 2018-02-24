{ mkDerivation, base, stdenv, xmonad, xmonad-contrib }:
mkDerivation {
  pname = "xalternative";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base xmonad xmonad-contrib ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/thumphries/xalternative";
  description = "XMonad+";
  license = stdenv.lib.licenses.bsd3;
}