{ lib, pkgs, ... }:
let
  validPluginFiles = (pkgs.callPackage ../plugin/functions.nix {}).validPluginFiles;
  compileAll = (pkgs.callPackage ../plugin/compile.nix {}).compileAll;

  tmux-plugins = [
    ./vim-tmux-navigator.nix
    ./tslime.nix
  ];

  txt-plugins = [
    ./goyo-vim.nix
  ];

  layout-plugins = [
    ./vim-airline.nix
    ./vim-airline-theme.nix
    ./fzf-vim.nix
  ];

  plugins = [
    ./vim-polyglot.nix
    ./vim-multiple-cursors.nix
    ./themes/wombat256-vim.nix
    ./vim-gitgutter.nix
  ] ++ tmux-plugins
    ++ txt-plugins
    ++ layout-plugins
    ;
in

assert validPluginFiles plugins;
let
  ps = compileAll plugins;
in {
  plugins = map (p: p.pkg) ps;
  extraConfig = lib.strings.concatStringsSep "\n" (map (p: p.extraConfig) ps);
}

