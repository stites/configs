{ pluginBuilder, ... }:
{
  description = "";
  pkg = pluginBuilder rec {
    name = "vim-github-colorscheme";
    tarball = "${homepage}/archive/master.tar.gz";
    homepage = https://github.com/endel/vim-github-colorscheme;
  };
}

