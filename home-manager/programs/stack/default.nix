{...}:
let
  confroot = "${builtins.getEnv "HOME"}/git/configs/home-manager/";
in
{
  home.file = {
    ".stack/config.yaml".source               = "${confroot}/programs/stack/local.yaml";
    ".stack/global-project/stack.yaml".source = "${confroot}/programs/stack/global.yaml";
  };
}
