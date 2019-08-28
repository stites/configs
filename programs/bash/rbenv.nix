{ }:

{ initConfig =
    (if ! builtins.pathExists ''${builtins.getEnv "HOME"}/.rbenv/bin'' then "" else ''
      # Add rbenv to PATH for scripting. Make sure this is the last PATH variable change.
      safe_path_add "$HOME/.rbenv/bin"

      # command -v rbenv >/dev/null && eval "$(rbenv init -)"
      if command -v rbenv >/dev/null; then
        export RBENV_SHELL=bash
        safe_path_add "$HOME/.rbenv/shims"
        safe_source '/usr/lib/rbenv/libexec/../completions/rbenv.bash'
        nohup command rbenv rehash > /dev/null 2>&1 &
        rbenv() {
          local command
          command="$1"
          if [ "$#" -gt 0 ]; then
            shift
          fi

          case "$command" in
          rehash|shell|update|use)
            eval "$(rbenv "sh-$command" "$@")";;
          *)
            command rbenv "$command" "$@";;
          esac
        }
      else
        echo "rbenv found but failed to load it in PATH"
      fi
    '');
}
