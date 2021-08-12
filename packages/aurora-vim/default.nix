pkgs:

pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "aurora-vim";
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "erikbackman";
      repo = "aurora.vim";
      rev = "84f0d8d3f2a0e21f3e1618fd747d1aea6bbf2840";
      sha256 = "0qxv6wq34di7b88yl2982v9f42ijcb86gy320ivmp0ysabdc8gkd";
    };
    meta.homepage = "https://github.com/erikbackman/aurora.vim";
}
