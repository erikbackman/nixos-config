{ mkDerivation, lib, base, xmonad, xmonad-contrib, X11, cpphs, dbus, ... }:
mkDerivation {
  pname = "ebn-xmonad-x86_64-linux";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmonad xmonad-contrib X11 cpphs dbus ];
  license = lib.licenses.bsd3;
}
