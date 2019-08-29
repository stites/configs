{ lib, pkgs, ... }:
{
  plugins = [
    # Python
    # Plug 'nvie/vim-flake8', { 'for': 'python' }
    # Plug 'vim-scripts/indentpython.vim', { 'for': 'python' }
    # Plug 'szymonmaszke/vimpyter'
  ];
  rc = lib.strings.concatStringsSep "\n" [
    # Read coco format as python (coco is "functional python")
    "au! BufNewFile,BufRead *.coco set filetype=python"

    ''
    " au BufNewFile,BufRead *.py
    "     \ set tabstop=4
    "     \ set softtabstop=4
    "     \ set shiftwidth=4
    "     \ set textwidth=79
    "     \ set expandtab
    "     \ set autoindent
    "     \ set fileformat=unix
    ''

    # Python ignores
    ''
    set wildignore+=__pycache__/*,*.py[cod],*$py.class,*.ipynb,.Python,env/*,build/*
    set wildignore+=develop-eggs/*,dist/*,downloads/*,eggs/*,.eggs/*,lib/*,lib64/*
    set wildignore+=parts/*,sdist/*,var/*,*.egg-info/*,.installed.cfg,*.egg,*.manifest
    set wildignore+=*.spec,pip-log.txt,pip-delete-this-directory.txt,htmlcov/*
    set wildignore+=__pycache__/*,.tox/*,.coverage,.coverage.*,.cache,nosetests.xml
    set wildignore+=coverage.xml,cover,.hypothesis/*,*.mo,*.pot,*.log,local_settings.py
    set wildignore+=instance/*,.webassets-cache,.scrapy,docs/_build/*,target/*
    set wildignore+=.ipynb_checkpoints,.python-version,celerybeat-schedule,.env,venv/*
    set wildignore+=ENV/*,.spyderproject,.ropeproject,.DS_Store,*.sublime-workspace
    ''


  ];
}
