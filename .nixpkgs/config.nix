{ pkgs }:
let
  vimrc = import ./vimrc.nix { };
in
with pkgs ; rec {
  allowUnfree = true;
  packageOverrides = pkgs : with pkgs; rec {
    my_vim = vim_configurable.customize {
      name = "my-vim";

      vimrcConfig.customRC = vimrc.config;

      vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;
      vimrcConfig.vam.pluginDictionaries = [
        { names = [
          "Syntastic"
          "Tagbar"
          "fzf-vim"
        ]; }
      ];
    };
    all = pkgs.buildEnv {
      name = "all";
      paths = [
        ctags
        my_vim
        steam
        unetbootin
        vkeybd
        youtube-dl
        wolfebin
      ];
    };
  };
}
