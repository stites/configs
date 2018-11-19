{ lib }:

let
  homeDir = builtins.getEnv "HOME";
  variables = {
    PYENV_ROOT= "${homeDir}/.pyenv";
    PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true";
    PYENV_VIRTUALENV_DISABLE_PROMPT=1;  # supress annoying warning for a feature I don't use
  };

  # https://github.com/aigamedev/scikit-neuralnetwork/issues/215#issuecomment-286191287
  # env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.6.* to install pythons which allow for creating shared objects

  # things installed:
  #  +  https://github.com/yyuu/pyenv-virtualenvwrapper
  #  +  https://github.com/yyuu/pyenv
  # be wary of this:
  #  +  https://stackoverflow.com/questions/24358572/pyenv-virtualenv-not-activating-tried-everything-i-know
in
{
  inherit variables;
  initConfig =
    (if ! builtins.pathExists variables.PYENV_ROOT then "" else (lib.strings.concatStringsSep "\n" [ ''
      safe_path_add "${variables.PYENV_ROOT}/bin"

      # ================================ #
      # eval "$(pyenv init -)"
      safe_path_add "${variables.PYENV_ROOT}/shims"

      export PYENV_SHELL=bash
      ''
      "
      _pyenv() {
        COMPREPLY=()
        local completions
        local word
        local words
        word=\"\${COMP_WORDS[COMP_CWORD]}\"
        if [ \"$COMP_CWORD\" -eq 1 ]; then
          # shellcheck disable=SC2207
          COMPREPLY=( $(compgen -W \"$(pyenv commands)\" -- \"$word\") )
        else
          words=(\"\${COMP_WORDS[@]}\")
          unset \"words[0]\"
          unset \"words[$COMP_CWORD]\"
          completions=$(pyenv completions \"\${words[@]}\")
          # shellcheck disable=SC2207
          COMPREPLY=( $(compgen -W \"$completions\" -- \"$word\") )
        fi
      }
      complete -F _pyenv pyenv
      "
      ''
      (nohup command pyenv rehash </dev/null >/dev/null 2>&1 &) || true

      pyenv() {
        local command
        command="$1"
        if [ "$#" -gt 0 ]; then
          shift
        fi

        case "$command" in
        activate|deactivate|rehash|shell|virtualenvwrapper|virtualenvwrapper_lazy)
          eval "$(pyenv "sh-$command" "$@")";;
        *)
          command pyenv "$command" "$@";;
        esac
      }

      # ================================ #
      # eval "$(pyenv virtualenv-init -)"
      safe_path_add "${variables.PYENV_ROOT}/plugins/pyenv-virtualenv/shims"
      ''

      "
      export PYENV_VIRTUALENV_INIT=1;
      _pyenv_virtualenv_hook() {
        local ret=$?
        if [ -n \"\${VIRTUAL_ENV:-}\" ]; then
          eval \"$(pyenv sh-activate --quiet || pyenv sh-deactivate --quiet || true)\" || true
        else
          eval \"$(pyenv sh-activate --quiet || true)\" || true
        fi
        return $ret
      };

      if [[ \"\${PROMPT_COMMAND:-}\" != *\"_pyenv_virtualenv_hook\"* ]]; then
        PROMPT_COMMAND=\"_pyenv_virtualenv_hook; \${PROMPT_COMMAND:-}\"
      fi
      "
  ]));
}

