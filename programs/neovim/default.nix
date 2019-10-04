{ lib, pkgs, stdenv, fetchgit, vimUtils, ... }:
let
  customPlugins = pkgs.callPackage ./plugins.nix {};
  pluginBuilder = pkgs.callPackage ./plugin/builder.nix {};

  myplugins = pkgs.callPackage ./myplugins {};
in
{
  xdg = {
    configFile = {
      "nvim/coc-settings.json".text = builtins.toJSON myplugins.coc-settings;
      "nvim/undodir/.empty".text = "";
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
    extraPython3Packages = (ps: with ps; [
      mccabe
      mypy
      nose
      pycodestyle
      pydocstyle

      jedi
      flake8
      pygments
      pytest-mypy
      pyls-isort
      pyls-mypy
      pyflakes
      yapf
    ]);
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    # CHECK OUT THIS FOR UPDATED CONTENT: https://nixos.wiki/wiki/Vim
    extraConfig = (pkgs.callPackage ./config.nix {}).extraConfig + myplugins.extraConfig;

    # extraConfig = lib.strings.concatStringsSep "\n" (
    #   (import ./config.nix).extraConfig ++ [
    #   configs.layout.rc
    #   # configs.git.rc
    #   # configs.cplusplus.rc
    #   # configs.haskell.rc
    #   # configs.lsp.rc
    #   configs.coc.rc
    #   configs.python.rc
    #   configs.surround.rc
    #   configs.spelling.rc
    #   configs.textmanipulation.rc
    #   configs.tmux.rc

    #   # enable spell checking
    #   # autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us
    #   # autocmd FileType gitcommit setlocal spell spelllang=en_us

    #   # let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
    #   # let g:ctrlp_max_files=0
    #   # let g:ctrlp_show_hidden=1
    #   # let g:ctrlp_custom_ignore =
    #   #   \ {
    #   #   \   'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$',
    #   #   \   'file': '\v\.(o|hi|beam|dyn_hi|dyn_o)$'
    #   #   \ }

    #   # vim-session ==================================================================
    #   ''
    #   let g:session_autosave = 'no'
    #   ''

    #   # work with neomutt ============================================================
    #   ''
    #   au BufRead /tmp/neomutt-* set tw=72
    #   augroup filetypedetect
    #     " Mail
    #     autocmd BufRead,BufNewFile *mutt-*  setfiletype mail
    #   augroup END
    #   ''

    #   # bufkill-vim ==================================================================
    #   ''
    #   nmap <C-w> :BD<CR>
    #   ''

    #   # vim-polyglot =================================================================
    #   ''
    #   let g:polyglot_disabled = ['elm', 'haskell']
    #   ''

    #   # COLLISION WITH COC.NVIM
    #   # # ack.vim ======================================================================
    #   # # don't jump to the first result automatically.
    #   # ''
    #   # cnoreabbrev Ack Ack!
    #   # " use ripgrep or the_silver_searcher (in that order), follow symlinks
    #   # if executable('rg')
    #   #   let g:ackprg = 'rg -L --vimgrep'
    #   # elseif executable('ag')
    #   #   let g:ackprg = 'ag -f --vimgrep'
    #   # endif

    #   # " bind to a
    #   # nnoremap <Leader>a :Ack!<Space>

    #   # " search on the current word
    #   # nmap \f      :Ack "\b<cword>\b" <CR>
    #   # nmap <Esc>f  :Ack "\b<cword>\b" <CR>
    #   # ''

    #   # ===============================================================================
    #   ''
    #   let g:haskell_tabular = 1

    #   vmap a= :Tabularize /=<CR>
    #   vmap a; :Tabularize /::<CR>
    #   vmap a- :Tabularize /-><CR>
    #   vmap am :Tabularize / as<CR>
    #   vmap a, :Tabularize /,<CR>
    #   ''

    #   # ==============================================================================
    #   # https://github.com/sol/hpack
    #   # run hpack automatically on modifications to package.yaml
    #   ''
    #   autocmd BufWritePost package.yaml silent !hpack --silent
    #   ''

    #   # Add these to your vimrc to automatically keep the tags file up to date.
    #   # Unfortunately silent means the errors look a little ugly, I suppose I could
    #   # capture those and print them out with echohl WarningMsg.
    #   ''
    #   au BufWritePost *.hs  silent !codex update --force %
    #   au BufWritePost *.hsc silent !codex update --force %
    #   ''

    #   # ==============================================================================
    #   # disable haskell indents
    #   ''
    #   let g:haskell_indent_disable=1
    #   " enable type information while typing
    #   let g:necoghc_enable_detailed_browse = 1
    #   " use stack in necoghc
    #   let g:necoghc_use_stack = 0

    #   " use haskell-ide-engine
    #   set hidden
    #   let g:LanguageClient_serverCommands = {
    #       \ 'haskell': ['hie', '--lsp'],
    #       \ }

    #   " let g:LanguageClient_rootMarkers = {
    #   "     \ 'haskell': ['cabal.project', 'stack.yaml'],
    #   "     \ }
    #   nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
    #   nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
    #   nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>
    #   ''

    #   # ==============================================================================
    #   # disable syntastic in python
    #   ''
    #   let g:syntastic_mode_map = { 'passive_filetypes': ['python'] }
    #   ''

    #   # ==============================================================================
    #   # Use deoplete.
    #   ''
    #   let g:deoplete#enable_at_startup = 1
    #   " disable autocomplete
    #   " let g:deoplete#disable_auto_complete = 1
    #   " inoremap <silent><expr><C-Space> deoplete#mappings#manual_complete()
    #   ''
    #   # ==============================================================================
    #   # UltiSnips config
    #   ''
    #   inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
    #   let g:UltiSnipsExpandTrigger="<tab>"
    #   let g:UltiSnipsJumpForwardTrigger="<tab>"
    #   let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
    #   ''

    #   # ==============================================================================
    #   # vim-session
    #   # https://peterodding.com/code/vim/session/
    #   # https://perma.cc/E487-SS2S
    #   ''
    #   let g:session_autoload = 'no'
    #   let g:session_autosave_periodic = 30   " minutes
    #   let g:session_directory = "/home/stites/.vim/session/"
    #   let g:session_lock_directory = "/home/stites/.vim/session-locks/"

    #   " Disable all session locking - I know what I'm doing :-).
    #   " let g:session_lock_enabled = 0

    #   ''
    #   # ==============================================================================
    #   # rainbow_parentheses.vim
    #   ''
    #   let g:rbpt_colorpairs = [
    #       \ ['red',         'firebrick3'],
    #       \ ['brown',       'RoyalBlue3'],
    #       \ ['Darkblue',    'SeaGreen3'],
    #       \ ['darkgray',    'DarkOrchid3'],
    #       \ ['darkgreen',   'firebrick3'],
    #       \ ['darkcyan',    'RoyalBlue3'],
    #       \ ['darkred',     'SeaGreen3'],
    #       \ ['darkmagenta', 'DarkOrchid3'],
    #       \ ['brown',       'firebrick3'],
    #       \ ['gray',        'RoyalBlue3'],
    #       \ ['black',       'SeaGreen3'],
    #       \ ['darkmagenta', 'DarkOrchid3'],
    #       \ ['Darkblue',    'firebrick3'],
    #       \ ['darkgreen',   'RoyalBlue3'],
    #       \ ['darkcyan',    'SeaGreen3'],
    #       \ ['darkred',     'DarkOrchid3'],
    #       \ ]


    #   " Activation based on file type
    #   augroup rainbow_lisp
    #     autocmd!
    #     " autocmd FileType * RainbowParenthesesActive
    #     autocmd FileType lisp,clojure,scheme,haskell,python RainbowParenthesesActivate
    #   augroup END

    #   " au VimEnter * RainbowParenthesesToggle
    #   " au Syntax * RainbowParenthesesLoadRound
    #   " au Syntax * RainbowParenthesesLoadSquare
    #   " au Syntax * RainbowParenthesesLoadBraces
    #   ''

    #   # ''
    #   # " " Adjust signscolumn to match wombat
    #   # " hi! link SignColumn LineNr
    #   # "
    #   # " " Use pleasant but very visible search hilighting
    #   # " hi Search ctermfg=white ctermbg=173 cterm=none guifg=#ffffff guibg=#e5786d gui=none
    #   # " hi! link Visual Search
    #   # "
    #   # " " Match wombat colors in nerd tree
    #   # " hi Directory guifg=#8ac6f2
    #   # "
    #   # " " Searing red very visible cursor
    #   # " hi Cursor guibg=red
    #   # ''

    #   # Files, backups and undo {{{

    #   ## # # ALTERNATIVE: Utility function to delete trailing white space
    #   ## # ''
    #   ## # fun! DeleteTrailingWS()
    #   ## #   exe "normal mz"
    #   ## #   %s/\s\+$//ge
    #   ## #   exe "normal `z"
    #   ## # endfun
    #   ## # " autocmd BufWritePre * :call DeleteTrailingWS()
    #   ## # ''
    #   ## # }}}
    #   # not installed
    #   # # Fuzzy find files
    #   # ''
    #   # nnoremap <silent> <Leader><space> :CtrlP<CR>
    #   # let g:ctrlp_max_files=0
    #   # let g:ctrlp_show_hidden=1
    #   # let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }
    #   # ''

    #   ''
    #   function! CmdLine(str)
    #     exe "menu Foo.Bar :" . a:str
    #     emenu Foo.Bar
    #     unmenu Foo
    #   endfunction
    #   ''
    #   # Moving around, tabs, windows and buffers {{{

    #   # Disable highlight when <leader><cr> is pressed
    #   # but preserve cursor coloring
    #   ''
    #   nmap <silent> <leader><cr> :noh\|hi Cursor guibg=red<cr>

    #   " Return to last edit position when opening files (You want this!)
    #   augroup last_edit
    #     autocmd!
    #     autocmd BufReadPost *
    #          \ if line("'\"") > 0 && line("'\"") <= line("$") |
    #          \   exe "normal! g`\"" |
    #          \ endif
    #   augroup END
    #   ''
    #   # Open window splits in various places
    #   "nmap <leader>sh :leftabove  vnew<CR>"
    #   "nmap <leader>sl :rightbelow vnew<CR>"
    #   "nmap <leader>sk :leftabove  new<CR>"
    #   "nmap <leader>sj :rightbelow new<CR>"

    #   # previous buffer, next buffer
    #   "nnoremap <leader>bp :bp<cr>"
    #   "nnoremap <leader>bn :bn<cr>"

    #   # close every window in current tabview but the current
    #   "nnoremap <leader>bo <c-w>o"

    #   # delete buffer without closing pane
    #   "noremap <leader>bd :Bd<cr>"

    #   # fuzzy find buffers
    #   "noremap <leader>b<space> :CtrlPBuffer<cr>"

    #   # Alignment {{{

    #   # Stop Align plugin from forcing its mappings on us
    #   "let g:loaded_AlignMapsPlugin=1"
    #   # Align on equal signs
    #   "map <Leader>a= :Align =<CR>"
    #   # Align on commas
    #   "map <Leader>a, :Align ,<CR>"
    #   # Align on pipes
    #   "map <Leader>a<bar> :Align <bar><CR>"
    #   # Prompt for align character
    #   "map <leader>ap :Align"

    #   # }}}
    #   ''
    #   " Completion {{{
    #   set completeopt+=longest

    #   " Use buffer words as default tab completion
    #   let g:SuperTabDefaultCompletionType = '<c-x><c-p>'

    #   " }}}
    #   ''

    #   # Tabnine experiment
    #   # "set rtp+=~/tabnine-vim"
    # ]);

    ###########################################################################
    # vim-plug automatically executes `filetype plugin indent on` and `syntax enable`.
    # You can revert the settings after the call. (e.g. filetype indent off, syntax off, etc.)
    plugins = myplugins.plugins;
    # plugins = (with pkgs.vimPlugins; [
    #   # HLedger
    #   (pluginBuilder rec { # Plug 'anekos/hledger-vim', { 'for': 'journal' }
    #     name = "hledger-vim";
    #     tarball = "${homepage}/archive/master.tar.gz";
    #     homepage = https://github.com/anekos/hledger-vim;
    #   })
    #   # Productivity
    #   (pluginBuilder rec {
    #     name = "utl-vim";
    #     tarball = "${homepage}/archive/master.tar.gz";
    #     homepage = https://github.com/vim-scripts/utl.vim;
    #   })
    #   vim-SyntaxRange
    #   calendar-vim
    #   vim-orgmode # 'jceb/vim-orgmode' | Plug 'mattn/calendar-vim' | Plug 'inkarkat/vim-SyntaxRange' | Plug 'vim-scripts/utl.vim'

    #   ##############################################
    #   # IDE support
    #   ##############################################
    #   vim-abolish             # Use with LSP to search for, substitute, and abbreviate multiple variants of a word
    #   # MIGRATED vim-multiple-cursors    # multicursors
    #   vim-hier                # highlight errors
    #   # MIGRATED vim-polyglot            # handle the rest of the languages
    #   rainbow_parentheses-vim # make parens pretty

    # ] ++ configs.prelude.plugins
    #   # ++ configs.git.plugins
    #   # ++ configs.cplusplus.plugins
    #   # ++ configs.haskell.plugins
    #   # ++ configs.lsp.plugins
    #   ++ configs.coc.plugins
    #   ++ configs.python.plugins
    #   ++ configs.surround.plugins
    #   ++ configs.spelling.plugins
    #   ++ configs.layout.plugins
    #   ++ configs.textmanipulation.plugins
    #   ++ configs.tmux.plugins
    # );
  };
}
