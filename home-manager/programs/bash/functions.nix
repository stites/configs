{ pkgs, lib }:

let
  concatStringsSep = lib.strings.concatStringsSep;

  mkFunction = { name, description, for-prompt ? false, body} : ''
    # Function ${name}
    # @description ${if (for-prompt ? true) then description + ". To be used in $PROMPT_COMMAND" else description}
    function ${name} {
      ${body}
    }
    export -f ${name}
  '';

  no-nix-shell = body: ''
    # >>> [START] Don't run this in nix-shells
    if [ -z "$IN_NIX_SHELL" ]; then
    ${body}

    # <<< [END] Don't run the above in nix-shells
    fi
  '';


in
{
  inherit mkFunction no-nix-shell;

  # ========================= #
  # User-defined functions    #
  # ========================= #
  functions = (concatStringsSep "\n" [

    (mkFunction {
      name = "safe_path_add";
      description = "add path to $PATH variable, if it is not already there.";
      body = ''
        if [ -d "$1" ]; then
          if [[ "$PATH" =~ $1 ]]; then
            export PATH="$1:$PATH"
          elif [[ "$PATH" == *"$1"* ]]; then
            # echo "ADDING WITH FALLBACK CHECK $1"
            export PATH="$1:$PATH"
          elif echo "$PATH" | ${pkgs.gnugrep}/bin/grep -o -E "(^|:)$1" | head -1 &> /dev/null; then
            # echo "ADDING WITH SLOWER, GREP FALLBACK"
            export PATH="$1:$PATH"
          fi
        fi
      '';})

    (mkFunction {
      name = "can_safe_add";
      description = "TODO";
      body = ''
        local strings="$1"
        local newstr="$2"

        if [ -d "$newstr" ]; then
          if [[ "$strings" =~ $newstr ]]; then
            # exists in end? but we add????
            return 0
          elif [[ "$strings" == *"$newstr"* ]]; then
            # exists in middle? but we add????
            return 0
          elif echo "$strings" | ${pkgs.gnugrep}/bin/grep -o -E "(^|:)$newstr" | head -1 &> /dev/null; then
            # exists in beginning? but we add????
            return 0
          fi
        fi
        return 1
      '';})

    (mkFunction {
      name = "safe_prompt_add";
      description = "add a command to the $PROMPT_COMMAND only if it is not already present";
      body = "
        local promptcmd
        promptcmd=\"\${PROMPT_COMMAND:-}\"
        if [[ \"$promptcmd\" != *\"$1\"* ]]; then
          export PROMPT_COMMAND=\"$1; $promptcmd\"
        fi
      ";})

    (mkFunction {
      name = "find_git_branch";
      description = "TODO";
      body = "
        # Based on: http://stackoverflow.com/a/13003854/170413
        local branch tag_0 tag t0 SLOW_DIR
        SLOW_DIR=$HOME/.bash/_find_git_brach.last
        MILLI_CAP=40
        touch \"$SLOW_DIR\"

        if branch=$(\\git rev-parse --abbrev-ref HEAD 2> /dev/null) && [[ \"$branch\" == \"HEAD\" ]]; then
          if [[ \"$(\\cat \"$SLOW_DIR\")\" == \"$PWD\" ]]; then
            branch=\"$(\\git rev-parse --short=7 HEAD)-notags\"
          else
            t0=$(date +%s%3N)
            tag_0=\"$(\\git describe --tags --abbrev=0 2> /dev/null)\"
            tdiff=$(($(date +%s%3N)-t0))

            if (( tdiff > MILLI_CAP )); then
              echo \"$PWD\" > \"$SLOW_DIR\"
              branch=\"$(\\git rev-parse --short=7 HEAD)-notags\"
            else
              echo \"\" > \"$SLOW_DIR\"

              tag=\"$(\\git describe --tags 2> /dev/null)\"

              branch=\"\${branch//[^a-z0-9\\/]/-}\"
              if [[ -n \"$tag\" ]] && [[ \"$tag_0\" == \"$tag\" ]]; then
                branch=\"tag:\${tag//[^a-z0-9\\/]/.}\"

              elif [[ \"$branch\" == \"HEAD\" ]]; then
                branch=\"$(\\git rev-parse --short=7 HEAD)\"
              fi
            fi
          fi
        fi
        echo \"$branch\"
      ";})

    (mkFunction {
      name = "find_git_dirty";
      description = "TODO";
      body = ''
        # find all new files, but only show the first one (we don't need more)
        # git ls-files --others --exclude-standard | head -n 1
        # See https://stackoverflow.com/questions/11122410/fastest-way-to-get-git-status-in-bash
        # for more speed tips
        if [[ "$(git status -uno --porcelain 2> /dev/null)" != "" ]]; then
          echo '*'
        fi
      '';})

    (mkFunction {
      name = "safe_source";
      description = "TODO";
      body = ''
        # File may not exist, so don't follow for shellcheck linting (SC1090).
        # shellcheck source=/dev/null
        [[ ! -f "$1" ]] || source "$1"
      '';})

    (mkFunction {
      name = "append_history";
      description = "After each command, append to the history file and reread it.";
      for-prompt = true;
      body = "history -a; history -c; history -r;"; })

    (mkFunction {
      name = "log_all_commands";
      for-prompt = true;
      description = "log all commands to a logfile.";
      body = ''
        if [ "$(id -u)" -ne 0 ]; then
          echo "$(date '+%Y-%m-%d.%H:%M:%S') $(pwd) $(history 1)" >> ~/.bash/log/bash-history-"$(date '+%Y-%m-%d')".log
        fi
      '';})

    (mkFunction {
      name = "myReload";
      description = "source bashrc if in shell, rebuild stack if in stack project";
      body = ''
        # if [ -z "`ls | ${pkgs.gnugrep}/bin/grep 'stack.yaml'`" ]; then
          exec "$SHELL"
        #else
        #  stack build
        #fi
      '';})

    (mkFunction {
      name = "virtualenvPrompt";
      description = ''
        # @description because of numerous PROMPT_COMMANDs, virtualenv gets overwritten.
        # Requires virtual environments to be written to a path including the phrase \"virtualenv\"
        #
        # @mutates variable venv_prompt'';
      body = "
        if test -z \"$VIRTUAL_ENV\" ; then
          venv_prompt=\"\"
        else
          venv_prompt=\"\${BLUE}[$(basename \\\"\"$VIRTUAL_ENV\"\\\")]\${RESET} \"
        fi
        export venv_prompt
      ";})

    (mkFunction {
      name = "dotFolder";
      description = "touch personal dot folder if it does not exist";
      body = ''
        if [ ! -d "$HOME/.stites/" ]; then
          mkdir ~/.stites
        fi
      '';})

    (mkFunction {
      name = "notes";
      description = "open up notes";
      body = ''
        dotFolder
        vim ~/.stites
      '';})

    (mkFunction {
      name = "workingmemory";
      description = "open up workingmemory";
      body = ''
        dotFolder
        vim ~/.stites/workingmemory.md
      '';})

    (mkFunction {
      name = "draft";
      description = "TODO";
      body = "
        local DRAFT_FOLDER=\"$HOME/.stites/emails/drafts/\"
        mkdir -p \"$DRAFT_FOLDER\"
        local NUM_DRAFTS
        NUM_DRAFTS=$(\\exa -l \"$DRAFT_FOLDER\" | wc -l)

        if echo \"$NUM_DRAFTS\" >> /dev/null; then
          echo \"You have $NUM_DRAFTS in $DRAFT_FOLDER\"
          sleep 1
        fi
        local NEXT_DRAFT=$(( NUM_DRAFTS+1 ))
        nvim -c 'set tw=72 et' '+/^$' \"\${DRAFT_FOLDER}draft_\${NEXT_DRAFT}\"
      ";})

    (mkFunction {
      name = "stack-intero";
      description = "run stack ghci with intero as the backend";
      body = ''
        stack ghci --with-ghc intero
      '';})

    (mkFunction {
      name = "retry";
      description = "retry a command five times";
      body = ''
        for _ in 1 2 3 4 5; do
          "$@" && break
          if $?; then
            sleep 15
          fi
        done
      '';})

    (mkFunction {
      name = "note";
      description = "open up a note in .stites/";
      body = ''vim "$HOME/.stites/$1"''; })

    ]);

  # ========================= #
  # Export functions          #
  # ========================= #
  initConfig = ''
  '';
}

