{ pkgs, lib }:

let
  concatStringsSep = lib.strings.concatStringsSep;
  colors = import ./bash/colors.nix;
  homeDir = builtins.getEnv "HOME";
  pyenv = import ./bash/pyenv.nix { inherit lib; };
  git = import ./bash/git.nix { inherit pkgs; };
  nvm = import ./bash/nvm.nix { inherit lib; };
  rbenv = import ./bash/rbenv.nix;
  fasd = import ./bash/fasd.nix { inherit lib; };
  prompt = import ./bash/prompt.nix;
  nix = import ./bash/nix.nix;
  haskell = import ./bash/haskell.nix { inherit lib; };
  functions = import ./bash/functions.nix { inherit lib; };
in
{
  enable = true;

  # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
  historyIgnore = [ "ls" "cd" "exit" ];
  historySize = 100000;
  # historyControl = [ "ignoredups" "ignorespace" "erasedups" ]; #"ignoreboth" ];
  historyFileSize = 200000;
  shellOptions = [
    "histappend"     # append to the history file, don't overwrite it
    "nocaseglob"     # auto corrects the case
    "checkwinsize"   # check the window size after each command and, if
                     # necessary, update the values of LINES and COLUMNS.
    "globstar"       # If set, the pattern "**" used in a pathname
                     # expansion context will match all files and zero or
                     # more directories and subdirectories.
  ];

  # These are sourced in ~/.profile and do _not_ get reloaded at exec $SHELL
  sessionVariables = {
    #IFS=$'\n\t'
    # ^^^ unofficial bash mode: https://perma.cc/UQ45-72E5
    EDITOR="nvim";
    BROWSER="firefox"; # toggle with lynx for headless servers
    PAGER="most";

    # extra exports that nix doesn't support
    HISTCONTROL="ignoredups:ignorespace:erasedups:ignoreboth";

    # potentially removable
    LC_ALL="en_US.UTF-8";
    LANG="en_US.UTF-8";

    # bash automatically fetches the last command that starts with the
    # given term: E.G. you type in ‘ssh’ and press the ‘Page Up’ key and bash
    # scrolls through your history for this. Store function in .inputrc
    INPUTRC="$HOME/.inputrc";

    # colored GCC warnings and errors
    # export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

    # Use https://github.com/rdnetto/powerline-hs
    POWERLINE_COMMAND="${homeDir}/.local/bin/powerline-hs";
    POWERLINE_CONFIG_COMMAND="${pkgs.coreutils}/bin/true";

    # make less more friendly for non-text input files, see lesspipe(1)
    # the output of eval "$(SHELL=/bin/sh lesspipe.sh)"
    LESSOPEN="|${pkgs.lesspipe}/bin/lesspipe.sh %s"; # FIXME << do we even need this?

  } // colors // pyenv.variables // prompt.variables;

  shellAliases = {
    # ===================== #
    # .bashrc functions     #
    # ===================== #
    vrc  = "vim $HOME/.bashrc";
    src  = "exec $SHELL";
    mr   = "monitors reset";
    mh   = "monitors home";
    ms   = "monitors sentenai";
    tree = "tree -C";

    # Relative Jumps:
    "~"="cd ~ ";
    ".."="cd ..";
    "..."="cd ../..";
    "...."="cd ../../..";
    "....."="cd ../../../..";

    # == mail functions for bash == #
    neomutt="cd ~/.mail/attachments && neomutt";
    mutt="${pkgs.neomutt}/bin/neomutt";

    # tmux aliases
    protonmail-bridge="${pkgs.tmux}/bin/tmux new-session -d -s mail 'Desktop-Bridge --cli'";
    ws="${pkgs.tmux}/bin/tmux attach -t stites";
    mail="${pkgs.tmux}/bin/tmux attach -t mail";

    # vim aliases
    vimd="${pkgs.neovim}/bin/nvim --headless ";
    vimattach="${pkgs.neovim}/bin/nvim --servername VIM_SERVER --remote-tab ";
    visnippets="${pkgs.neovim}/bin/nvim ~/.config/nvim/snippets/haskell.snippets";

    # == ghci to bash == #
    ":q"="exit";
    ":r"="myReload";

    preview="${pkgs.fzf}/bin/fzf --preview 'bat --color \"always\" {}'";

    cat  = "${pkgs.bat}/bin/bat";
    less = "${pkgs.most}/bin/most";
    ping = "${pkgs.prettyping}/bin/prettyping --nolegend";
    top  = "${pkgs.ncdu}/bin/htop";
    du   = "${pkgs.ncdu}/bin/ncdu --color dark -rr -x --exclude .git --exclude node_modules --exclude .stack-work --exclude dist-newstyle";
    help = "${pkgs.tldr}/bin/tldr";
    curl = "${pkgs.httpie}/bin/http";

    # the lss
    la   = "${pkgs.exa}/bin/exa";
    ls   = "${pkgs.exa}/bin/exa -s extension";
    l    = "${pkgs.exa}/bin/exa -s extension --group-directories-first -l";
    ll   = "${pkgs.exa}/bin/exa -s extension --group-directories-first -a -l";
    lll  = "${pkgs.exa}/bin/exa -s extension --group-directories-first -aa -l";

    # the greps
    grep="grep --color=auto";
    fgrep="fgrep --color=auto";
    egrep="egrep --color=auto";
    igrep="egrep -i --color=auto";
    vgrep="vgrep -v";

    # dictd:https://www.unixmen.com/look-dictionary-definitions-via-terminal/
    define="${pkgs.dict}/bin/dictd -d gcide ";

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alert = "notify-send --urgency=low -i \\\"$([ $? = 0 ] && echo terminal || echo error)\\\" \\\"$(history|tail -n1|sed -e '\\''s/^\\s*[0-9]\\+\\s*//;s/[;&|]\\s*alert$//'\\'')\\\"'";


    # disabled unless we install nginx
    # "nginx.conf"="vim ~/.config/nginx/conf/nginx.conf";
    # "nginx"="nginx -p ~/.config/nginx/";
  } // git.shellAliases // haskell.shellAliases;

  initExtra = (concatStringsSep "\n" [
    "set +o vi"

    # enable programmable completion features (you don't need to enable
    # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
    # sources /etc/bash.bashrc).
    # FIXME: remove this? it's legacy from debian and ubuntu
    ''
    if ! shopt -oq posix; then
      if [ -f ${pkgs.bash-completion}/etc/profile.d/bash_completion.sh ]; then
        source ${pkgs.bash-completion}/etc/profile.d/bash_completion.sh
      else
        echo "warning! <nixpkgs>.bash-completion/etc/profile.d/bash_completion.sh not found. maybe garbage collection occured?"
      fi
    fi
    ''

    functions.functions
    functions.initConfig

    (if builtins.pathExists "/bin"            then "safe_path_add /bin"            else "")
    (if builtins.pathExists "/sbin"           then "safe_path_add /sbin"           else "")
    (if builtins.pathExists "/usr/bin"        then "safe_path_add /usr/bin"        else "")
    (if builtins.pathExists "/usr/sbin"       then "safe_path_add /usr/sbin"       else "")
    (if builtins.pathExists "/usr/local/bin"  then "safe_path_add /usr/local/bin"  else "")
    (if builtins.pathExists "/usr/local/sbin" then "safe_path_add /usr/local/sbin" else "")

    git.functions

    fasd.initConfig

    haskell.functions
    haskell.initConfig

    nix.functions
    nix.initConfig

    nvm.initConfig

    pyenv.initConfig

    rbenv.initConfig

    prompt.functions
    prompt.initConfig

    # this is all rust needs
    (if builtins.pathExists "${homeDir}/.cargo/bin" then ''safe_path_add "${homeDir}/.cargo/bin"'' else "")

    # ========================================== #
    #     Silence that fucking hardware bell     #
    # ========================================== #
    ''
    [[ -n "$DISPLAY" ]] && xset b off
    ''

    # ============================================================ #
    # core aliases which must be added last -- PR for home-manager #
    # ============================================================ #
    ''
    alias find="${pkgs.fd}/bin/fd"
    safe_source "$HOME/.fonts/*.sh"
    ''
  ]);
}
