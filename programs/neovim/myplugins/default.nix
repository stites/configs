{ lib, pkgs, ... }:
let
  validPluginFiles = (pkgs.callPackage ../plugin/functions.nix {}).validPluginFiles;
  compileAll = (pkgs.callPackage ../plugin/compile.nix {}).compileAll;

  plugins = [
    ./vim-polyglot.nix
    ./vim-multiple-cursors.nix
    ./themes/wombat256-vim.nix
    ./vim-sandwich.nix
    ./vim-commentary.nix
    ./vim-eunuch.nix
  ] ++ tmux-plugins
    ++ txt-plugins
    ++ layout-plugins
    ++ coc-plugins
    ;

  tmux-plugins = [
    ./vim-tmux-navigator.nix
    ./tslime.nix
  ];

  txt-plugins = [
    ./goyo-vim.nix
    ./vim-textobj-sentence.nix
    ./vim-speeddating.nix # increment dates with <c-x> and <c-a>
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
    ./coc/lists.nix
    ./coc/yank.nix
    ./coc/json.nix
    ./coc/snippets.nix
    ./coc/prettier.nix
  ] ++ (if usetabnine then [./coc/tabnine.nix] else [
    # ./neco.nix
    # ./html.nix
    ./coc/python.nix
    ./coc/vimtex.nix
  ]);

in

assert validPluginFiles plugins;
let
  ps = compileAll plugins;
in rec {
  plugins = map (p: p.pkg) ps;
  extraConfig = lib.strings.concatStringsSep "\n" (map (p: p.extraConfig) ps);
  coc1 = (map (p: pkgs.callPackage p {}) coc-plugins);
  coc2 = map (p: lib.attrsets.optionalAttrs (p ? coc-settings) p.coc-settings)
          (map (p: pkgs.callPackage p {}) coc-plugins);

  coc-settings = let
      allsettings =
        map (p: lib.attrsets.optionalAttrs (p ? coc-settings) p.coc-settings)
          (map (p: pkgs.callPackage p {}) coc-plugins);
    in
      lib.attrsets.foldAttrs (new: memo: assert memo == null || memo == new; new) null allsettings;
}

