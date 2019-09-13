{ pkgs, lib, ... }:
{
  description = "handle languages syntax (and more)";
  pkg = pkgs.vimPlugins.vim-polyglot;
  extraConfig = [
    # languaes to disable (in case you want to use something more powerful
    # "let g:polyglot_disabled = ['elm', 'haskell']"
  ];
}

