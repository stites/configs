{ pkgs, ... }:
{
  plugins = [
    (pluginBuilder rec {
      name = "vim-textobj-sentence";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/reedes/vim-textobj-sentence;
    })
  ];
  extraConfig = [];
}
