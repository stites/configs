{ pkgs, ... }:
let
  confroot = (pkgs.callPackage ../../hosts {}).confroot;
  my-dictionary = with pkgs; buildEnv {
    name = "my-dictionary";
    paths = [
      dict
      # dictdDBs.eng2rus
      dictdDBs.wiktionary
      dictdDBs.wordnet

      aspell
      aspellDicts.en
    ];
  };
in
{
  home.packages = [ my-dictionary ];
  home.file = {
    ".aspell.en.pws".source                   = "${confroot}/programs/aspell/my-aspell-ws";
    ".aspell.en.prepl".source                 = "${confroot}/programs/aspell/my-aspell-repl";
  };
}
