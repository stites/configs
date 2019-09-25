{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.coc-python;
  coc-settings = {
    "python.jediEnabled" = false; # use Microsoft Python Language Server, coc-python will download latest stable MPLS for you.
    ## Options
    #
    #- `python.autoComplete.addBrackets`:Automatically add brackets for functions, not work for MPLS., default: `false`
    #- `python.autoComplete.extraPaths`:List of paths to libraries and the like that need to be imported by auto complete engine. E.g. when using Google App SDK, the paths are not in system path, hence need to be added into this list., default: `[]`
    #- `python.autoComplete.showAdvancedMembers`:Controls appearance of methods with double underscores in the completion list., default: `true`
    #- `python.autoComplete.typeshedPaths`:Specifies paths to local typeshed repository clone(s) for the Python language server., default: `[]`
    #- `python.autoUpdateLanguageServer`:Automatically update the language server., default: `true`
    #- `python.disableInstallationCheck`:Whether to check if Python is installed (also warn when using the macOS-installed Python)., default: `false`
    #- `python.envFile`:Absolute path to a file containing environment variable definitions., default: `"${workspaceFolder}/.env"`
    #- `python.trace.server`:Trace level of tsserver, default: `"off"`
    #- `python.formatting.autopep8Args`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.formatting.autopep8Path`:Path to autopep8, you can use a custom version of autopep8 by modifying this setting to include the full path., default: `"autopep8"`
    #- `python.formatting.provider`:Provider for formatting. Possible options include 'autopep8', 'black', and 'yapf'., default: `"autopep8"`
    #- `python.formatting.blackArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.formatting.blackPath`:Path to Black, you can use a custom version of Black by modifying this setting to include the full path., default: `"black"`
    #- `python.formatting.yapfArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.formatting.yapfPath`:Path to yapf, you can use a custom version of yapf by modifying this setting to include the full path., default: `"yapf"`
    #- `python.globalModuleInstallation`:Whether to install Python modules globally when not using an environment., default: `false`
    #- `python.jediEnabled`:Enables Jedi as IntelliSense engine instead of Microsoft Python Analysis Engine., default: `true`
    #- `python.jediMemoryLimit`:Memory limit for the Jedi completion engine in megabytes. Zero (default) means 1024 MB. -1 means unlimited (disable memory limit check), default: `0`
    #- `python.jediPath`:Path to directory containing the Jedi library (this path will contain the 'Jedi' sub directory)., default: `""`
    #- `python.analysis.diagnosticEnabled`: Enable diagnostic support of language server, default: `true`
    #- `python.analysis.openFilesOnly`:Only show errors and warnings for open files rather than for the entire workspace., default: `true`
    #- `python.analysis.diagnosticPublishDelay`:Delay before diagnostic messages are transferred to the problems list (in milliseconds)., default: `1000`
    #- `python.analysis.typeshedPaths`:Paths to look for typeshed modules., default: `[]`
    #- `python.analysis.errors`:List of diagnostics messages to be shown as errors., default: `[]`
    #- `python.analysis.warnings`:List of diagnostics messages to be shown as warnings., default: `[]`
    #- `python.analysis.information`:List of diagnostics messages to be shown as information., default: `[]`
    #- `python.analysis.disabled`:List of suppressed diagnostic messages., default: `[]`
    #- `python.analysis.logLevel`:Defines type of log messages language server writes into the output window., default: `"Error"`
    #- `python.analysis.symbolsHierarchyDepthLimit`:Limits depth of the symbol tree in the document outline., default: `10`
    #- `python.linting.enabled`:Whether to lint Python files., default: `true`
    #- `python.linting.flake8Args`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.flake8CategorySeverity.E`:Severity of Flake8 message type 'E'., default: `"Error"`
    #- `python.linting.flake8CategorySeverity.F`:Severity of Flake8 message type 'F'., default: `"Error"`
    #- `python.linting.flake8CategorySeverity.W`:Severity of Flake8 message type 'W'., default: `"Warning"`
    #- `python.linting.flake8Enabled`:Whether to lint Python files using flake8, default: `false`
    #- `python.linting.flake8Path`:Path to flake8, you can use a custom version of flake8 by modifying this setting to include the full path., default: `"flake8"`
    #- `python.linting.ignorePatterns`:Patterns used to exclude files or folders from being linted., default: `[".vscode/*.py","**/site-packages/**/*.py"]`
    #- `python.linting.lintOnSave`:Whether to lint Python files when saved., default: `true`
    #- `python.linting.maxNumberOfProblems`:Controls the maximum number of problems produced by the server., default: `100`
    #- `python.linting.banditArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.banditEnabled`:Whether to lint Python files using bandit., default: `false`
    #- `python.linting.banditPath`:Path to bandit, you can use a custom version of bandit by modifying this setting to include the full path., default: `"bandit"`
    #- `python.linting.mypyArgs`:Arguments passed in. Each argument is a separate item in the array., default: `["--ignore-missing-imports","--follow-imports=silent","--show-column-numbers"]`
    #- `python.linting.mypyCategorySeverity.error`:Severity of Mypy message type 'Error'., default: `"Error"`
    #- `python.linting.mypyCategorySeverity.note`:Severity of Mypy message type 'Note'., default: `"Information"`
    #- `python.linting.mypyEnabled`:Whether to lint Python files using mypy., default: `false`
    #- `python.linting.mypyPath`:Path to mypy, you can use a custom version of mypy by modifying this setting to include the full path., default: `"mypy"`
    #- `python.linting.pep8Args`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.pep8CategorySeverity.E`:Severity of Pep8 message type 'E'., default: `"Error"`
    #- `python.linting.pep8CategorySeverity.W`:Severity of Pep8 message type 'W'., default: `"Warning"`
    #- `python.linting.pep8Enabled`:Whether to lint Python files using pep8, default: `false`
    #- `python.linting.pep8Path`:Path to pep8, you can use a custom version of pep8 by modifying this setting to include the full path., default: `"pep8"`
    #- `python.linting.prospectorArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.prospectorEnabled`:Whether to lint Python files using prospector., default: `false`
    #- `python.linting.prospectorPath`:Path to Prospector, you can use a custom version of prospector by modifying this setting to include the full path., default: `"prospector"`
    #- `python.linting.pydocstyleArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.pydocstyleEnabled`:Whether to lint Python files using pydocstyle, default: `false`
    #- `python.linting.pydocstylePath`:Path to pydocstyle, you can use a custom version of pydocstyle by modifying this setting to include the full path., default: `"pydocstyle"`
    #- `python.linting.pylamaArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.pylamaEnabled`:Whether to lint Python files using pylama., default: `false`
    #- `python.linting.pylamaPath`:Path to pylama, you can use a custom version of pylama by modifying this setting to include the full path., default: `"pylama"`
    #- `python.linting.pylintArgs`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.linting.pylintCategorySeverity.convention`:Severity of Pylint message type 'Convention/C'., default: `"Information"`
    #- `python.linting.pylintCategorySeverity.error`:Severity of Pylint message type 'Error/E'., default: `"Error"`
    #- `python.linting.pylintCategorySeverity.fatal`:Severity of Pylint message type 'Fatal/F'., default: `"Error"`
    #- `python.linting.pylintCategorySeverity.refactor`:Severity of Pylint message type 'Refactor/R'., default: `"Hint"`
    #- `python.linting.pylintCategorySeverity.warning`:Severity of Pylint message type 'Warning/W'., default: 
    #`"Warning"`
    #- `python.linting.pylintEnabled`:Whether to lint Python files using pylint., default: `true`
    #- `python.linting.pylintPath`:Path to Pylint, you can use a custom version of pylint by modifying this setting to include the full path., default: `"pylint"`
    #- `python.linting.pylintUseMinimalCheckers`:Whether to run Pylint with minimal set of rules., default: `true`
    #- `python.pythonPath`:Path to Python, you can use a custom version of Python by modifying this setting to include the full path., default: `"python"`
    #- `python.condaPath`:Path to the conda executable to use for activation (version 4.4+)., default: `""`
    #- `python.pipenvPath`:Path to the pipenv executable to use for activation., default: `"pipenv"`
    #- `python.poetryPath`:Path to the poetry executable., default: `"poetry"`
    #- `python.sortImports.args`:Arguments passed in. Each argument is a separate item in the array., default: `[]`
    #- `python.sortImports.path`:Path to isort script, default using inner version, default: `""`
    #- `python.terminal.activateEnvironment`:Activate Python Environment in Terminal created using the Extension., default: `true`
    #- `python.terminal.executeInFileDir`:When executing a file in the terminal, whether to use execute in the file's directory, instead of the current open folder., default: `false`
    #- `python.terminal.launchArgs`:Python launch arguments to use when executing a file in the terminal., default: `[]`
    #- `python.venvFolders`:Folders in your home directory to look into for virtual environments., default: `["envs",".pyenv",".direnv"]`
    #- `python.venvPath`:Path to folder with a list of Virtual Environments (e.g. ~/.pyenv, ~/Envs, ~/.virtualenvs)., default: `""`
    #- `python.workspaceSymbols.ctagsPath`:Fully qualified path to the ctags executable (else leave as ctags, assuming it is in current path)., default: `"ctags"`
    #- `python.workspaceSymbols.enabled`:Set to 'false' to disable Workspace Symbol provider using ctags., default: `true`
    #- `python.workspaceSymbols.exclusionPatterns`:Pattern used to exclude files and folders from ctags See http://ctags.sourceforge.net/ctags.html., default: `["**/site-packages/**"]`
    #- `python.workspaceSymbols.rebuildOnFileSave`:Whether to re-build the tags file on when changes made to python files are saved., default: `true`
    #- `python.workspaceSymbols.rebuildOnStart`:Whether to re-build the tags file on start (defaults to true)., default: `true`
    #- `python.workspaceSymbols.tagFilePath`:Fully qualified path to tag file (exuberant ctags file), used to provide workspace symbols., default: `"${workspaceFolder}/.vscode/tags"`
  };
}
