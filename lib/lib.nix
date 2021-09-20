lib:

with lib;
with lib.filesystem;
with builtins;
{
  listModulesRec = dir: filter (hasSuffix "default.nix") (listFilesRecursive dir);
}
