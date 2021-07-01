lib:

with lib;
with lib.filesystem;
with builtins;
let
  hasDefaultNix = dir: pathExists (dir + "/default.nix");

  findModulesRec = dir:
    let
      files = filter (hasSuffix ".nix") (listFilesRecursive dir);
    in
      /* Some modules will be spread across multiple nix files
         and will therefore have a default.nix,
         in that case we import default.nix
         otherwise we import the file
      */
      map (f: let path = dirOf f;
              in if hasDefaultNix path then path + "/default.nix" else f) files;
in {
  modules = {
    inherit findModulesRec;
  };
}
