{ pkgs, lib, ... }:
{
  description = "show git changes in the gutter";
  pkg = pkgs.vimPlugins.vim-gitgutter;
  extraConfig = [
    ''let g:gitgutter_git_executable="${pkgs.git}/bin/git"''

    # "let g:gitgutter_map_keys = 0"      # Don't use vim-gitgutter mappings

    # "nmap ]h <Plug>(GitGutterNextHunk)" # jump to next hunk (change): Default ]c
    # "nmap [h <Plug>(GitGutterPrevHunk)" # jump to previous hunk (change): Default [c

    # Folding
    # Use the GitGutterFold command to fold all unchanged lines, leaving just the hunks visible. Use `zr` to unfold 3 lines of context above and below a hunk.
    # Execute GitGutterFold a second time to restore the previous view.
  ];
}

