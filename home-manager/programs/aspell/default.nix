{...}:
let
  confroot = "${builtins.getEnv "HOME"}/git/configs/home-manager/";
in
{
  home.file = {
    ".aspell.en.pws".source                   = "${confroot}/programs/aspell/my-aspell-ws";
    ".aspell.en.prepl".source                 = "${confroot}/programs/aspell/my-aspell-repl";
  };
}
