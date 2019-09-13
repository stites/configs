{ pkgs, pluginBuilder }:
{
  rc = "";
  plugins = with pkgs.vimPlugins; [
    ##############################################
    # Colorschemes, layouts, and icons
    ##############################################
    vim-airline                          # add statusbar
    vim-airline-themes                   # theme statusbar
    vim-devicons                         # add unknown icons for statusbar

    gruvbox                              # [?????] retro theme
    papercolor-theme                     # [?????]
    wombat256-vim                        # [dark]  haskell favorite theme

    # [?????]
    (pluginBuilder {
      name = "oceanic-next";
      tarball = "https://github.com/mhartington/oceanic-next/archive/master.tar.gz";
      homepage = https://github.com/mhartington/oceanic-next;
    })

    # [?????]
    (pluginBuilder {
      name = "vim-kalisi";
      tarball = "https://github.com/freeo/vim-kalisi/archive/master.tar.gz";
      homepage = https://github.com/freeo/vim-kalisi;
    })

    # [light] github colorscheme
    (pluginBuilder {
      name = "vim-github-colorscheme";
      tarball = "https://github.com/endel/vim-github-colorscheme/archive/master.tar.gz";
      homepage = https://github.com/endel/vim-github-colorscheme;
    })

    ######################################
    # Bars, panels, and files
    ######################################
    fzfWrapper
    fzf-vim          # Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    ack-vim          # file search
    tagbar           # http://vimawesome.com/plugin/tagbar


    # (pluginBuilder {
    #   name = "papercolor-theme";
    #   tarball = "${homepage}/archive/master.tar.gz";
    #   homepage = https://github.com/NLKNguyen/papercolor-theme;
    # })
  ];
}
