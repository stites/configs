{ lib, pkgs, pluginBuilder, ... }:
let
  vim-sandwich = pluginBuilder {
      name = "vim-sandwich";
      tarball = "https://github.com/machakann/vim-sandwich/archive/master.tar.gz";
      homepage = https://github.com/machakann/vim-sandwich;
  };
in
{
  plugins = [
    vim-sandwich
    # vim-surround               # 'tpope/vim-surround'   " http://vimawesome.com/plugin/surround-vim

    # # HTML, XML
    # (pluginBuilder { # Plug 'docunext/closetag.vim', { 'for': 'html' }
    #   name = "closetag-vim";
    #   tarball = "https://github.com/docunext/closetag.vim/archive/master.tar.gz";
    #   homepage = https://github.com/docunext/closetag.vim;
    # })
  ];
  rc = lib.strings.concatStringsSep "\n" [
    # ================================================== #
    # vim-sandwich                                       #
    # ================================================== #

    # -------------------------------------------------- #
    # Use vim-surround keymap
    # -------------------------------------------------- #
    # If you want to use with vim-surround keymappings, add the following line to your vimrc.
    "runtime ${vim-sandwich}/share/vim-plugins/vim-sandwich/macros/sandwich/keymap/surround.vim"

    # `ys`, `yss`, `yS`, `ds`, `cs` in normal mode and `S` in visual mode are available.
    # Not in vim-surround, but `dss` and `css` are also available, these are similar as
    # `ds` and `cs` but determine deleted/replaced texts automatically. See repo for detail.

    # NOTE that the vim-sandwich plugin should be in 'runtimepath' option when the :runtime command is executed.
    # If you are using a kind of plugin managers, like Vundle, plug or neobundle, the :execute command should be
    # placed after vundle#end()/plug#end()/neobundle#end() in your vimrc.

    # vundle#begin()
    # ~~~
    # Plugin 'machakann/vim-sandwich'
    # ~~~
    # vundle#end()
    # runtime macros/sandwich/keymap/surround.vim
    # Or just use :source command instead.
    # source {full/path/to}/macros/sandwich/keymap/surround.vim
    # Please change {full/path/to} part for your environment.

    # -------------------------------------------------- #
    # vim-sandwich provides several textobjects
    # -------------------------------------------------- #

    # To select a text surrounded by braket or same characters user input.
    #    |<---- is( ---->|
    # {[(a_surrounded_text)]}
    #   |<----- as( ----->|
    #
    #   |<----- is[ ----->|
    # {[(a_surrounded_text)]}
    #  |<------ as[ ------>|
    #
    #  |<------ is{ ------>|
    # {[(a_surrounded_text)]}
    # |<------- as{ ------->|
    #
    #      |<  is_ >|
    # {[(a_surrounded_text)]}
    #     |<-  as_ ->|
    ''
    xmap is <Plug>(textobj-sandwich-query-i)
    xmap as <Plug>(textobj-sandwich-query-a)
    omap is <Plug>(textobj-sandwich-query-i)
    omap as <Plug>(textobj-sandwich-query-a)
    ''

    # Textobjects to select the nearest surrounded text automatically.
    #    |<---- iss ---->|
    # {[(a_surrounded_text)]}
    #   |<----- ass ----->|
    #
    #    |<---- iss ---->|
    # ({[a_surrounded_text]})
    #   |<----- ass ----->|
    #
    #    |<---- iss ---->|
    # [({a_surrounded_text})]
    #   |<----- ass ----->|
    "xmap iss <Plug>(textobj-sandwich-auto-i)"
    "xmap ass <Plug>(textobj-sandwich-auto-a)"
    "omap iss <Plug>(textobj-sandwich-auto-i)"
    "omap ass <Plug>(textobj-sandwich-auto-a)"

    # Textobjects to select a text surrounded by same characters user input.
    #      |<  im_ >|
    # {[(a_surrounded_text)]}
    #     |<-  am_ ->|
    "xmap im <Plug>(textobj-sandwich-literal-query-i)"
    "xmap am <Plug>(textobj-sandwich-literal-query-a)"
    "omap im <Plug>(textobj-sandwich-literal-query-i)"
    "omap am <Plug>(textobj-sandwich-literal-query-a)"

    # ==============================================================================
    # vim-textobj-sentence is best used on text and markdown
    ''
    augroup textobj_sentence
      autocmd!
      autocmd FileType markdown call textobj#sentence#init()
      autocmd FileType textile call textobj#sentence#init()
    augroup END

    let g:textobj#sentence#select = 's'
    let g:textobj#sentence#move_p = '('
    let g:textobj#sentence#move_n = ')'
    ''
  ];
}
