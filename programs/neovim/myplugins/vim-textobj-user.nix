{ pkgs, ... }:
{
  plugins = [
    # Plug 'kana/vim-textobj-user' | Plug 'reedes/vim-textobj-sentence'
    (pluginBuilder rec {
      name = "vim-textobj-user";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/kana/vim-textobj-user;
    })
  ];
  extraConfig = [];
}
