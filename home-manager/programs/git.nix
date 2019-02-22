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
    #### Vim ####
    "*~"
    "*.swp"

    #### OSX ####
    ".DS_Store"

    #### Common Stuff ####

    # personal files / folder
    ".stites"

    # tmp file names
    "tmp"
    "foo"
    "bar"

    # single-character-files
    "?"

    # project-level ignores and configs
    ".agignore"
    ".sosrc"

    ### Tags ###
    # Ignore tags created by etags, ctags, gtags (GNU global) and cscope
    "TAGS"
    ".TAGS"
    "!TAGS/"
    "tags"
    ".tags"
    "!tags/"
    "gtags.files"
    "GTAGS"
    "GRTAGS"
    "GPATH"
    "GSYMS"
    "cscope.files"
    "cscope.out"
    "cscope.in.out"
    "cscope.po.out"

    # haskell tags
    "codex.tags"
    "hscope.out"

    #### NixOS ####
    "result"

    #### Node ####
    # Dependency directories
    "node_modules/"
    "jspm_packages/"

    ".grunt"           # Grunt intermediate storage (https://gruntjs.com/creating-plugins#storing-task-files)
    "bower_components" # Bower dependency directory (https://bower.io/)
    "build/Release"    # Compiled binary addons (https://nodejs.org/api/addons.html)
    "typings/"         # TypeScript v1 declaration files
    ".npm"             # Optional npm cache directory
    ".cache"           # parcel-bundler cache (https://parceljs.org/)
    ".vuepress/dist"   # vuepress build output
    ".dynamodb/"       # DynamoDB Local files

    #### Git ####
    "/.git/"

    ### Haskell ###
    "dist/"
    "dist-newstyle/"
    ".cabal-sandbox/"
    ".stack-work/"
    ".HTF/"

    # project-level ghci config
    ".ghci"

    ### Python ###
    # Byte-compiled / optimized / DLL files
    "__pycache__/"
    "*.py[cod]"
    "*$py.class"

    # Distribution / packaging
    ".Python"
    "build/"
    "develop-eggs/"
    "dist/"
    "eggs/"
    ".eggs/"
    "sdist/"
    "wheels/"
    "pip-wheel-metadata/"
    "share/python-wheels/"
    "*.egg-info/"

    # Unit test / coverage reports
    ".tox/"
    ".nox/"
    ".hypothesis/"
    ".pytest_cache/"

    # Jupyter Notebook
    ".ipynb_checkpoints"

    # Environments
    "env.bak/"
    "venv.bak/"

    ### Rust ####
    "/target/"
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

    [help]
    autocorrect = 1

    [http]
    sslVerify = false

    [github]
    user = stites

    [url "ssh://git@github.com/"]
    insteadOf = https://github.com/

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
