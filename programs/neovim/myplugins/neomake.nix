{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.neomake;
  description = ''
    Neomake is a plugin for Vim/Neovim to asynchronously run programs.

    You can use it instead of the built-in :make command (since it can pick up
    your 'makeprg' setting), but its focus is on providing an extra layer of
    makers based on the current file (type) or project. Its origin is a
    proof-of-concept for Syntastic to be asynchronous.
  '';
  extraConfig = let
      # If you want to run Neomake automatically (in file mode), you can configure
      # it in your vimrc by using neomake#configure#automake, e.g. by picking one
      # of:
      neomakeConfig = {
        # When writing a buffer (no delay).
        write-mode = "call neomake#configure#automake('w')";
        # When writing a buffer (no delay), and on normal mode changes (after 750ms).
        writeAndNormal-mode = ms: "call neomake#configure#automake('nw', ${ms})"; # 750
        # When reading a buffer (after 1s), and when writing (no delay).
        readAndWrite-mode = ms: "call neomake#configure#automake('rw', ${ms})"; # 1000
        # Full config: when writing or reading a buffer, and on changes in insert and
        # normal mode (after 1s; no delay when writing).
        full-mode = ms: "call neomake#configure#automake('nrwi', ${ms})"; # 500
      };
    in [

      # # Advanced setup
      # The author liked to use the following, which uses different modes based on if
      # your laptop runs on battery (for MacOS or Linux):
      ''
      function! MyOnBattery()
        if has('macunix')
          return match(system('pmset -g batt'), "Now drawing from 'Battery Power'") != -1
        elsif has('unix')
          return readfile('/sys/class/power_supply/AC/online') == ['0']
        endif
        return 0
      endfunction

      if MyOnBattery()
        ${neomakeConfig.write-mode}
      else
        ${neomakeConfig.writeAndNormal-mode "1000"}
      endif
      ''
      # See :help neomake-automake (in doc/neomake.txt) for more information, e.g. how
      # to configure it based on certain autocommands explicitly, and for details about
      # which events get used for the different string-based modes.

      # ==============================================================================
      # haskell settings
      ''
      autocmd! BufWritePost *.hs Neomake
      let g:neomake_haskell_hlint_maker = {
          \ 'args': ['--verbose'],
          \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
          \ }
      let g:neomake_haskell_enabled_makers = ['hlint']
      ''
    ];
}
