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
    ./vim-textobj-sentence.nix
  ];

  layout-plugins = [
    ./vim-airline.nix
    ./vim-airline-theme.nix
    ./fzf-vim.nix
    ./vim-gitgutter.nix
  ];

  coc-plugins = let usetabnine = true; in [
    ./coc
    ./coc/highlight.nix
    ./coc/yank.nix
    ./coc/json.nix
    ./coc/snippets.nix
    ./coc/lists.nix
    ./coc/prettier.nix
  ] ++ (if usetabnine then [./coc/tabnine.nix] else [
    # ./neco.nix
    # ./html.nix
    ./coc/python.nix
    ./coc/vimtex.nix
  ]);

  plugins = [
    ./vim-polyglot.nix
    ./coc
    ./vim-multiple-cursors.nix
    ./themes/wombat256-vim.nix
    ./vim-sandwich.nix
    ./vim-commentary.nix
  ] ++ tmux-plugins
    ++ txt-plugins
    ++ layout-plugins
    ++ coc-plugins
    ;
in

assert validPluginFiles plugins;
let
  ps = compileAll plugins;
in {
  plugins = map (p: p.pkg) ps;
  extraConfig = lib.strings.concatStringsSep "\n" (map (p: p.extraConfig) ps);
}

