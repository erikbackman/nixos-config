lib:

with lib;
with lib.filesystem;
with builtins;
{
  modules = {
    listModulesRec = dir: filter (hasSuffix "default.nix") (listFilesRecursive dir);
  };
}
