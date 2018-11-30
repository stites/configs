let
  secrets = import ../secrets.nix;
in
{
  enable = true;

  userName  = "Sam Stites";
  userEmail = secrets.piis.address;

  signing = {
    key = secrets.gpg.signing-key;
    signByDefault = true;
  };

  aliases = {
    brn = "for-each-ref --sort=-committerdate refs/heads/ --format='%(committerdate:short) %(authorname) %(refname:short)'";
    btree = ''
      log \
        --all \
        --graph \
        --decorate=short \
        --color \
        --format=format:'%C(bold blue)%h%C(reset) %C(auto)%d%C(reset) %C(black)[%cr]%C(reset) %C(black)%an - %s %C(reset)'
    '';
  };

  ignores = [
    "*~"
    "*.swp"
    ".DS_Store"
    ".agignore"
    ".stites"
    "*/infinitest.filters"
    "codex.tags"
    "tags"
    "cscope.files"
    "cscope.out"
    "hscope.out"
    ".ghci"
    ".sosrc"
    ".envrc"
    "tmp"
    "foo"
    "bar"
    "?"
    ".myenv"
    "data/*.gz"
  ];

  extraConfig = ''
    [rerere]
    enabled = true
    autoupdate = true

    [core]
    ignorecase = false
    eol = lf
    whitespace = blank-at-eol,space-before-tab,tab-in-indent,tabwidth=2
    editor = nvim
    # excludesfile = ~/.config/git/ignore # covered by nix

    [color]
    ui = auto

    [grep]
    lineNumber = true
    patternType = perl

    [advice]
    pushNonFastForward = false
    statusHints = false

    [diff]
    renames = copies
    mnemonicprefix = true

    [pager]
    diff = diff-so-fancy | less --tabs=1,5 -RFX
    show = diff-so-fancy | less --tabs=1,5 -RFX

    [branch]
    autosetupmerge = true

    [push]
    default = tracking

    [merge]
    stat = true
    tool = vimdiff

    [url "https://"]
    insteadOf = git://

    [help]
    autocorrect = 1

    [http]
    sslVerify = false

    [github]
    user = stites

    [url "git://github.com/ghc/packages-"]
    insteadOf = git://github.com/ghc/packages/

    [credential]
    helper = store --file ~/.config/git/credentials
    # helper = cache --timeout=30000

    [filter "lfs"]
    smudge = git-lfs smudge -- %f
    process = git-lfs filter-process
    required = true
    clean = git-lfs clean -- %f

    [bulkworkspaces]
    all = $HOME/git
  '';
}
