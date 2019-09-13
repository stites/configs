{ pkgs, ... }:
{
  plugins = [
    # highlight what you yank!
    (pluginBuilder rec {
      name = "vim-highlightedyank";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/machakann/vim-highlightedyank;
    })
  ];
  extraConfig = [];
}
