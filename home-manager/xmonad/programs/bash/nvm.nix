{ lib }:

let
  concatStringsSep = lib.strings.concatStringsSep;
  homeDir = builtins.getEnv "HOME";
in
{
  initConfig = concatStringsSep "\n" [
    # load nvm if exported variable does not exist
    (if ! builtins.pathExists "${homeDir}/.nvm" then "" else ''
      export NVM_DIR="$HOME/.nvm"

      # See: https://gist.github.com/stites/085998c020f7d2fdfb2cf2f3638d2f73
      # See: http://broken-by.me/lazy-load-nvm/
      lazynvm() {
        unset -f nvm node npm
        export NVM_DIR=~/.nvm
        safe_source "$NVM_DIR/nvm.sh"  # This loads nvm
      }

      nvm() {
        lazynvm
        nvm "$@"
      }

      node() {
        lazynvm
        node "$@"
      }

      npm() {
        lazynvm
        npm "$@"
      }
    '')
    (if ! builtins.pathExists "${homeDir}/.nvm/bash_completion" then "" else "source ${homeDir}/.nvm/bash_completion")
  ];

}
