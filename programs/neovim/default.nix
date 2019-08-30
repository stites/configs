{ lib, pkgs, stdenv, fetchgit, vimUtils, ... }:
let
  customPlugins = pkgs.callPackage ./plugins.nix {};
  pluginBuilder = pkgs.callPackage ./plugin-builder.nix {};

  configs = {
    prelude          = pkgs.callPackages ./configs/prelude.nix          { inherit pluginBuilder; };
    git              = pkgs.callPackages ./configs/git.nix              { inherit pluginBuilder; };
    cplusplus        = pkgs.callPackages ./configs/cplusplus.nix        { inherit pluginBuilder; };
    haskell          = pkgs.callPackages ./configs/haskell.nix          { inherit pluginBuilder; };
    lsp              = pkgs.callPackages ./configs/lsp.nix              { inherit pluginBuilder; };
    coc              = pkgs.callPackages ./configs/coc.nix              { inherit pluginBuilder; };
    python           = pkgs.callPackages ./configs/python.nix           { inherit pluginBuilder; };
    surround         = pkgs.callPackages ./configs/surround.nix         { inherit pluginBuilder; };
    spelling         = pkgs.callPackages ./configs/spelling.nix         { inherit pluginBuilder; };
    layout           = pkgs.callPackages ./configs/layout.nix           { inherit pluginBuilder; };
    textmanipulation = pkgs.callPackages ./configs/textmanipulation.nix { inherit pluginBuilder; };
    nix              = pkgs.callPackages ./configs/nix.nix              { inherit pluginBuilder; };
    tmux             = pkgs.callPackages ./configs/tmux.nix             { inherit pluginBuilder; };
  };
in
{
  xdg = {
    configFile = {
      "nvim/coc-settings.json".text = builtins.toJSON {
        "suggest.timeout" = 500;
        "coc.preferences.formatOnSaveFiletypes" = ["python"];

        "python.autoComplete.showAdvancedMembers" = false;
        "python.pythonPath" = "python";
        "python.formatting.provider" = "black";
        "python.formatting.blackPath" = "black";
        "python.linting.mypyEnabled" = true;
        "python.linting.mypyPath" = "mypy";
        "python.linting.pylintEnabled" = false;
        "python.linting.pylamaEnabled" = true;
        "python.linting.pylamaPath" = "pylama";
        "python.sortImports.path" = "isort";
      };
      "nvim/init.vim".text = ''
        set shell=/bin/sh
      '';
      "nvim/UltiSnips/python.snippets".source = ./UltiSnips/python.snippets;
      "nvim/UltiSnips/haskell.snippets" = {
        text = lib.strings.concatStringsSep "\n" [
          (builtins.readFile ./UltiSnips/haskell.snippets)
          ''
            snippet box "" !b
            -------------------------------------------------------------------------------
            -- |
            -- Module    :  `!v HaskellModuleName()`
            -- Copyright :  (c) Sam Stites 2017
            -- License   :  BSD-3-Clause
            -- Maintainer:  ${(import ../../secrets.nix).piis.address-rot13}
            -- Stability :  experimental
            -- Portability: non-portable
            -------------------------------------------------------------------------------

            endsnippet
          ''
        ];
      };
      "lsp/settings.json".text = ''
        {
          "languageServerHaskell": {
            "hlintOn": true,
            "maxNumberOfProblems": 10,
            "useCustomHieWrapper": true,
            "useCustomHieWrapperPath": "hie-wrapper"
          }
        }
      '';
    };
    dataFile = {
      "vim_gmake" = {
        executable = true;
        target = "../bin/vim_gmake";
        text = ''
          #!/usr/bin/env sh
          case "$(uname -o)" in
            "FreeBSD") gmake ;;
            *) make ;;
          esac
        '';
      };

      "vl" = {
        executable = true;
        target = "../bin/vl";
        source = ./exes/vl.sh;
      };

      "vim-plug" = {
        target = "nvim/site/autoload/plug.vim";
        source = (builtins.fetchTarball {
          url = "https://github.com/junegunn/vim-plug/archive/master.tar.gz";
        }) + "/plug.vim";
      };
    };
  };

  home.packages = [ pkgs.nodejs pkgs.yarn ];

  nixpkgs.overlays = [
    (self: super: let
      pin = pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "3f4144c30a6351dd79b177328ec4dea03e2ce45f";
        sha256 = "1qg5n60n3fr6cypihnrjw451fadps5pysj5p0vvfb320mpfvlbjb";
      };
      unstable = import pin {};
    in {
      neovim = unstable.neovim;
      vimPlugins = unstable.vimPlugins;
    })
  ];

  programs.neovim = {
    enable = true;
    # extraPython3Packages = (ps: with ps; [
    #   mccabe
    #   mypy
    #   nose
    #   pycodestyle
    #   pydocstyle

    #   jedi
    #   flake8
    #   pygments
    #   pytest-mypy
    #   pyls-isort
    #   pyls-mypy
    #   pyflakes
    #   yapf
    # ]);
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    # CHECK OUT THIS FOR UPDATED CONTENT: https://nixos.wiki/wiki/Vim
    extraConfig = lib.strings.concatStringsSep "\n" [
      configs.prelude.rc
      configs.layout.rc
      configs.git.rc
      configs.cplusplus.rc
      configs.haskell.rc
      # configs.coc.rc
      configs.lsp.rc
      configs.python.rc
      configs.surround.rc
      configs.spelling.rc
      configs.nix.rc
      configs.textmanipulation.rc
      configs.tmux.rc

      # enable spell checking
      # autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us
      # autocmd FileType gitcommit setlocal spell spelllang=en_us

      # let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
      # let g:ctrlp_max_files=0
      # let g:ctrlp_show_hidden=1
      # let g:ctrlp_custom_ignore =
      #   \ {
      #   \   'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$',
      #   \   'file': '\v\.(o|hi|beam|dyn_hi|dyn_o)$'
      #   \ }

      # vim-session ==================================================================
      ''
      let g:session_autosave = 'no'
      ''

      # work with neomutt ============================================================
      ''
      au BufRead /tmp/neomutt-* set tw=72
      augroup filetypedetect
        " Mail
        autocmd BufRead,BufNewFile *mutt-*  setfiletype mail
      augroup END
      ''

      # bufkill-vim ==================================================================
      ''
      nmap <C-w> :BD<CR>
      ''

      # vim-polyglot =================================================================
      ''
      let g:polyglot_disabled = ['elm', 'haskell']
      ''

      # FZF commands:
      ''
      nmap ; :Buffers<CR>
      nmap <C-p> :Files<CR>
      nmap <Leader>t :Files<CR>
      nmap <Leader>r :Tags<CR>

      nmap <Esc><Esc> :cclose<CR>
      nmap \x :cclose<CR>
      ''

      # COLLISION WITH COC.NVIM
      # # ack.vim ======================================================================
      # # don't jump to the first result automatically.
      # ''
      # cnoreabbrev Ack Ack!
      # " use ripgrep or the_silver_searcher (in that order), follow symlinks
      # if executable('rg')
      #   let g:ackprg = 'rg -L --vimgrep'
      # elseif executable('ag')
      #   let g:ackprg = 'ag -f --vimgrep'
      # endif

      # " bind to a
      # nnoremap <Leader>a :Ack!<Space>

      # " search on the current word
      # nmap \f      :Ack "\b<cword>\b" <CR>
      # nmap <Esc>f  :Ack "\b<cword>\b" <CR>
      # ''

      # ===============================================================================
      ''
      let g:haskell_tabular = 1

      vmap a= :Tabularize /=<CR>
      vmap a; :Tabularize /::<CR>
      vmap a- :Tabularize /-><CR>
      vmap am :Tabularize / as<CR>
      vmap a, :Tabularize /,<CR>
      ''

      # ==============================================================================
      # https://github.com/sol/hpack
      # run hpack automatically on modifications to package.yaml
      ''
      autocmd BufWritePost package.yaml silent !hpack --silent
      ''

      # Add these to your vimrc to automatically keep the tags file up to date.
      # Unfortunately silent means the errors look a little ugly, I suppose I could
      # capture those and print them out with echohl WarningMsg.
      ''
      au BufWritePost *.hs  silent !codex update --force %
      au BufWritePost *.hsc silent !codex update --force %
      ''

      # ==============================================================================
      # disable haskell indents
      ''
      let g:haskell_indent_disable=1
      " enable type information while typing
      let g:necoghc_enable_detailed_browse = 1
      " use stack in necoghc
      let g:necoghc_use_stack = 0

      " use haskell-ide-engine
      set hidden
      let g:LanguageClient_serverCommands = {
          \ 'haskell': ['hie', '--lsp'],
          \ }

      " let g:LanguageClient_rootMarkers = {
      "     \ 'haskell': ['cabal.project', 'stack.yaml'],
      "     \ }
      nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
      nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
      nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>
      ''

      # ==============================================================================
      # disable syntastic in python
      ''
      let g:syntastic_mode_map = { 'passive_filetypes': ['python'] }
      ''

      # ==============================================================================
      # Use deoplete.
      ''
      let g:deoplete#enable_at_startup = 1
      " disable autocomplete
      " let g:deoplete#disable_auto_complete = 1
      " inoremap <silent><expr><C-Space> deoplete#mappings#manual_complete()
      ''
      # ==============================================================================
      # UltiSnips config
      ''
      inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
      let g:UltiSnipsExpandTrigger="<tab>"
      let g:UltiSnipsJumpForwardTrigger="<tab>"
      let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
      ''

      # ==============================================================================
      # neomake
      ''
      autocmd! BufWritePost *.hs Neomake
      let g:neomake_haskell_hlint_maker = {
          \ 'args': ['--verbose'],
          \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
          \ }
      let g:neomake_haskell_enabled_makers = ['hlint']
      ''

      # ==============================================================================
      # vim-session
      # https://peterodding.com/code/vim/session/
      # https://perma.cc/E487-SS2S
      ''
      let g:session_autoload = 'no'
      let g:session_autosave_periodic = 30   " minutes
      let g:session_directory = "/home/stites/.vim/session/"
      let g:session_lock_directory = "/home/stites/.vim/session-locks/"

      " Disable all session locking - I know what I'm doing :-).
      " let g:session_lock_enabled = 0

      ''
      # ==============================================================================
      # rainbow_parentheses.vim
      ''
      let g:rbpt_colorpairs = [
          \ ['red',         'firebrick3'],
          \ ['brown',       'RoyalBlue3'],
          \ ['Darkblue',    'SeaGreen3'],
          \ ['darkgray',    'DarkOrchid3'],
          \ ['darkgreen',   'firebrick3'],
          \ ['darkcyan',    'RoyalBlue3'],
          \ ['darkred',     'SeaGreen3'],
          \ ['darkmagenta', 'DarkOrchid3'],
          \ ['brown',       'firebrick3'],
          \ ['gray',        'RoyalBlue3'],
          \ ['black',       'SeaGreen3'],
          \ ['darkmagenta', 'DarkOrchid3'],
          \ ['Darkblue',    'firebrick3'],
          \ ['darkgreen',   'RoyalBlue3'],
          \ ['darkcyan',    'SeaGreen3'],
          \ ['darkred',     'DarkOrchid3'],
          \ ]


      " Activation based on file type
      augroup rainbow_lisp
        autocmd!
        " autocmd FileType * RainbowParenthesesActive
        autocmd FileType lisp,clojure,scheme,haskell,python RainbowParenthesesActivate
      augroup END

      " au VimEnter * RainbowParenthesesToggle
      " au Syntax * RainbowParenthesesLoadRound
      " au Syntax * RainbowParenthesesLoadSquare
      " au Syntax * RainbowParenthesesLoadBraces
      ''

      # Colors and Fonts {{{
      ''
      try
        colorscheme wombat256mod

        " https://sunaku.github.io/vim-256color-bce.html
        " perma: https://perma.cc/Q6GC-ZD4A
        set term=screen-256color
      catch
      endtry
      ''

      # ''
      # " " Adjust signscolumn to match wombat
      # " hi! link SignColumn LineNr
      # "
      # " " Use pleasant but very visible search hilighting
      # " hi Search ctermfg=white ctermbg=173 cterm=none guifg=#ffffff guibg=#e5786d gui=none
      # " hi! link Visual Search
      # "
      # " " Match wombat colors in nerd tree
      # " hi Directory guifg=#8ac6f2
      # "
      # " " Searing red very visible cursor
      # " hi Cursor guibg=red
      # ''

      # Use powerline fonts for airline
      ''
      if !exists('g:airline_symbols')
        let g:airline_symbols = {}
      endif

      let g:airline_powerline_fonts = 1
      let g:airline_symbols.space = "\ua0"
      ''


      ''
      function! ProseMode()
        call goyo#execute(0, [])
        set spell noci nosi noai nolist noshowmode noshowcmd
        set complete+=s
        set bg=light
        " if !has('gui_running')
        "   let g:solarized_termcolors=256
        " endif
        " colors solarized
      endfunction

      command! ProseMode call ProseMode()
      nmap \p :ProseMode<CR>
      ''

      # Files, backups and undo {{{

      ## USE LESSSPACE INSTEAD!
      ## # strip trailing whitespace everywhere
      ## ''
      ## fun! <SID>StripTrailingWhitespaces()
      ##     let l = line(".")
      ##     let c = col(".")
      ##     %s/\s\+$//e
      ##     call cursor(l, c)
      ## endfun
      ## autocmd FileType * autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
      ## " autocmd FileType * autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
      ## ''
      ## # autocmd FileType haskell,c,cpp,java,php,ruby,python,markdown autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()

      ## # # ALTERNATIVE: Utility function to delete trailing white space
      ## # ''
      ## # fun! DeleteTrailingWS()
      ## #   exe "normal mz"
      ## #   %s/\s\+$//ge
      ## #   exe "normal `z"
      ## # endfun
      ## # " autocmd BufWritePre * :call DeleteTrailingWS()
      ## # ''
      ## # }}}

      # Open file prompt with current path
      ''nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>''

      # Show undo tree
      "nmap <silent> <leader>u :MundoToggle<CR>"

      # Fuzzy find files
      ''
      nnoremap <silent> <Leader><space> :CtrlP<CR>
      let g:ctrlp_max_files=0
      let g:ctrlp_show_hidden=1
      let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }
      ''

      ''
      function! CmdLine(str)
        exe "menu Foo.Bar :" . a:str
        emenu Foo.Bar
        unmenu Foo
      endfunction
      ''
      # Moving around, tabs, windows and buffers {{{

      # Treat long lines as break lines (useful when moving around in them)
      "nnoremap j gj"
      "nnoremap k gk"

      "noremap <c-h> <c-w>h"
      "noremap <c-k> <c-w>k"
      "noremap <c-j> <c-w>j"
      "noremap <c-l> <c-w>l"

      # Disable highlight when <leader><cr> is pressed
      # but preserve cursor coloring
      ''
      nmap <silent> <leader><cr> :noh\|hi Cursor guibg=red<cr>

      " Return to last edit position when opening files (You want this!)
      augroup last_edit
        autocmd!
        autocmd BufReadPost *
             \ if line("'\"") > 0 && line("'\"") <= line("$") |
             \   exe "normal! g`\"" |
             \ endif
      augroup END
      ''
      # Remember info about open buffers on close
      "set viminfo^=%"

      # Open window splits in various places
      "nmap <leader>sh :leftabove  vnew<CR>"
      "nmap <leader>sl :rightbelow vnew<CR>"
      "nmap <leader>sk :leftabove  new<CR>"
      "nmap <leader>sj :rightbelow new<CR>"

      # previous buffer, next buffer
      "nnoremap <leader>bp :bp<cr>"
      "nnoremap <leader>bn :bn<cr>"

      # close every window in current tabview but the current
      "nnoremap <leader>bo <c-w>o"

      # delete buffer without closing pane
      "noremap <leader>bd :Bd<cr>"

      # fuzzy find buffers
      "noremap <leader>b<space> :CtrlPBuffer<cr>"


      # Neovim terminal configurations
      ''
      if has('nvim')
        " Use <Esc> to escape terminal insert mode
        tnoremap <Esc> <C-\><C-n>
        " Make terminal split moving behave like normal neovim
        tnoremap <c-h> <C-\><C-n><C-w>h
        tnoremap <c-j> <C-\><C-n><C-w>j
        tnoremap <c-k> <C-\><C-n><C-w>k
        tnoremap <c-l> <C-\><C-n><C-w>l
      endif
      " }}}
      ''

      # Editing mappings {{{

      # Spell checking {{{
      # Pressing ,ss will toggle and untoggle spell checking
      ''
      map <leader>ss :setlocal spell!<cr>
      ''
      # }}}

      # Alignment {{{

      # Stop Align plugin from forcing its mappings on us
      "let g:loaded_AlignMapsPlugin=1"
      # Align on equal signs
      "map <Leader>a= :Align =<CR>"
      # Align on commas
      "map <Leader>a, :Align ,<CR>"
      # Align on pipes
      "map <Leader>a<bar> :Align <bar><CR>"
      # Prompt for align character
      "map <leader>ap :Align"

      # }}}

      # Tags {{{

      "set tags+=./tags;$HOME,./codex.tags;$HOME"
      "set cst"
      "set csverb"

      # IGNORES ARE HERE BECAUSE THEY INTERFERE WITH CTAG LOOKUP
      "set wildignore+=*.min.*"       # Web ignores
      "set wildignore+=*.stack-work*" # Haskell ignores
      "set wildignore+=*.so"          # C ignores

      "map <leader>tt :TagbarToggle<CR>"

      # }}}
      ''
      " Completion {{{
      set completeopt+=longest

      " Use buffer words as default tab completion
      let g:SuperTabDefaultCompletionType = '<c-x><c-p>'

      " }}}
      ''

      # Tabnine experiment
      # "set rtp+=~/tabnine-vim"
    ];

    ###########################################################################
    # vim-plug automatically executes `filetype plugin indent on` and `syntax enable`.
    # You can revert the settings after the call. (e.g. filetype indent off, syntax off, etc.)
    plugins = (with pkgs.vimPlugins; [
      # HLedger
      (pluginBuilder rec { # Plug 'anekos/hledger-vim', { 'for': 'journal' }
        name = "hledger-vim";
        tarball = "${homepage}/archive/master.tar.gz";
        homepage = https://github.com/anekos/hledger-vim;
      })
      # Productivity
      (pluginBuilder rec {
        name = "utl-vim";
        tarball = "${homepage}/archive/master.tar.gz";
        homepage = https://github.com/vim-scripts/utl.vim;
      })
      vim-SyntaxRange
      calendar-vim
      vim-orgmode # 'jceb/vim-orgmode' | Plug 'mattn/calendar-vim' | Plug 'inkarkat/vim-SyntaxRange' | Plug 'vim-scripts/utl.vim'

      ##############################################
      # IDE support
      ##############################################
      vim-abolish             # Use with LSP to search for, substitute, and abbreviate multiple variants of a word
      vim-multiple-cursors    # multicursors
      vim-hier                # highlight errors
      vim-polyglot            # handle the rest of the languages
      rainbow_parentheses-vim # make parens pretty

    ] ++ configs.prelude.plugins
      ++ configs.git.plugins
      # ++ configs.cplusplus.plugins
      ++ configs.haskell.plugins
      # ++ configs.lsp.plugins
      ++ configs.coc.plugins
      ++ configs.python.plugins
      ++ configs.surround.plugins
      ++ configs.spelling.plugins
      ++ configs.layout.plugins
      ++ configs.textmanipulation.plugins
      ++ configs.nix.plugins
      ++ configs.tmux.plugins
    );
  };
}
