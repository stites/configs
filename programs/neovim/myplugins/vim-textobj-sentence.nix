{ pkgs, pluginBuilder, ... }:
{
  pkg = pluginBuilder rec {
    name = "vim-textobj-sentence";
    tarball = "${homepage}/archive/master.tar.gz";
    homepage = https://github.com/reedes/vim-textobj-sentence;
  };
  dependencies = [
    ./vim-textobj-user.nix
  ];
  extraConfig = [
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
