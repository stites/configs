let
  confroot = (import ../../vars.nix).confroot;
in
{
  xdg.configFile."termonad/termonad.hs" = {
    source = "${confroot}/programs/termonad/termonad.hs";
    onChange = "rm -rf ~/.cache/termonad/termonad-linux-x86_64";
  };
}
