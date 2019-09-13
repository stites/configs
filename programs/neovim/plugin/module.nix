{ lib, ... }:

with lib;

let
  pluginConfigType = mkOptionType {
    name = "vim plugin";
    description = "vim plugin type";
    check = x: hasAttr "pkg" x && hasAttr "extraConfig" x;
    merge = mergeEqualOption;
  };

  pluginType = with types; package;
  # pluginType = with types; either package
  #   name = "vim plugin package";
  #   description = "vim plugin package, either a package from pkgs.vimPlugins or _github_ details to load the plugin";
  #   check = x: isDerivation x || isStorePath x;
  #   merge = mergeEqualOption;
  # };
in
{
  options = {
    disable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to disable the plugin, because I am the only person using this the default is 'enabled'";
    };

    pkg = mkOption {
      type = types.package;
      description = "plugin package";
    };

    extraConfig = mkOption {
      type = types.lines;
      description = "plugin package";
    };

    dependencies = mkOption {
      type = types.listOf pluginType;
      description = "plugin package";
    };

    # on = mkOption {
    #   type = types.string;
    #   description = "command to load plugin on";
    # };

    # for = mkOption {
    #   type = with types; listOf string;
    #   description = "file types to run plugin on";
    # };
  };
}
