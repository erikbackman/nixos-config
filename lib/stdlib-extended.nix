lib:
let myLib = import ./.;
in lib.extend (self: super: { ebn = myLib { lib = super; }; })
