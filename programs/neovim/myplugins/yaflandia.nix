{ pkgs, pluginBuilder, ... }:
{
  # [?????] c++ theme???
  pkg = pluginBuilder {
    name = "yaflandia";
    rev = "master";
    homepage = https://github.com/JBakamovic/yaflandia;
  };
  extraConfig = [
  ];
}
