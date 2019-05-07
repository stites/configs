let
  confroot = "${builtins.getEnv "HOME"}/git/configs/home-manager/";
in
{
  xdg.configFile."termonad/termonad.hs" = {
    source = "${confroot}/programs/termonad/termonad.hs";
    onChange = "rm -rf ~/.cache/termonad/termonad-linux-x86_64";
  };
}
