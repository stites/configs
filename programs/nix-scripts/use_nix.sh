#!/usr/bin/env bash

use_nix() {
  echo "Hello" 3>&1 1>&2 2>&3
  if ! validate_version; then
    echo "This .envrc requires direnv version 2.18.2 or above."
    exit 1
  fi

  # define all local variables
  local shell
  local files_to_watch=()

  local opt OPTARG OPTIND # define vars used by getopts locally
  while getopts ":n:s:w:" opt; do
    case "${opt}" in
      s)
        shell="${OPTARG}"
        files_to_watch=("${files_to_watch[@]}" "${shell}")
        ;;
      w)
        files_to_watch=("${files_to_watch[@]}" "${OPTARG}")
        ;;
      :)
        fail "Invalid option: $OPTARG requires an argument"
        ;;
      \?)
        fail "Invalid option: $OPTARG"
        ;;
    esac
  done
  shift $((OPTIND -1))

  local f
  for f in "${files_to_watch[@]}"; do
    if ! [[ -f "${f}" ]]; then
      fail "cannot watch file ${f} because it does not exist"
    fi
  done

  # ====================================================

  local path="$(nix-instantiate --find-file nixpkgs)"

  if [ -f "${path}/.version-suffix" ]; then
    local version="$(< $path/.version-suffix)"
  elif [ -f "${path}/.git" ]; then
    local version="$(< $(< ${path}/.git/HEAD))"
  fi

  local cache=".direnv/cache-${version:-unknown}"

  local update_drv=0
  if [[ ! -e "$cache" ]] || \
    [[ "$HOME/.direnvrc" -nt "$cache" ]] || \
    [[ .envrc -nt "$cache" ]] || \
    [[ default.nix -nt "$cache" ]] || \
    [[ shell.nix -nt "$cache" ]];
  then
    [ -d .direnv ] || mkdir .direnv
    local tmp=$(nix-shell --show-trace --pure "$@" \
      --run "\"$direnv\" dump bash")
    echo "$tmp" > "$cache"
    update_drv=1
  else
    log_status using cached derivation
  fi
  local term_backup=$TERM path_backup=$PATH
  if [ -z ${TMPDIR+x} ]; then
    local tmp_backup=$TMPDIR
  fi

  eval "$(< $cache)"
  export PATH=$PATH:$path_backup TERM=$term_backup TMPDIR=$tmp_backup
  if [ -z ${tmp_backup+x} ]; then
    export TMPDIR=${tmp_backup}
  else
    unset TMPDIR
  fi

  # `nix-shell --pure` will invalid ssl certificate paths
  if [ "${SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
    unset SSL_CERT_FILE
  fi
  if [ "${NIX_SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
    unset NIX_SSL_CERT_FILE
  fi

  # This part is based on https://discourse.nixos.org/t/what-is-the-best-dev-workflow-around-nix-shell/418/4
  if [ "$out" ] && (( $update_drv )); then
    local drv_link=".direnv/drv"
    local drv="$(nix show-derivation $out | grep -E -o -m1 '/nix/store/.*.drv')"
    local stripped_pwd=${PWD/\//}
    local escaped_pwd=${stripped_pwd//-/--}
    local escaped_pwd=${escaped_pwd//\//-}
    ln -fs "$drv" "$drv_link"
    ln -fs "$PWD/$drv_link" "/nix/var/nix/gcroots/per-user/$LOGNAME/$escaped_pwd"
    log_status renewed cache and derivation link
  fi

  if [[ $# = 0 ]]; then
    watch_file default.nix
    watch_file shell.nix

    # watch all the files we were asked to watch for the environment
    for f in "${files_to_watch[@]}"; do
      watch_file "${f}"
    done
  fi
}

fail() {
  log_error "${@}"
  exit 1
}

hash_contents() {
  if has md5sum; then
    cat "${@}" | md5sum | cut -c -32
  elif has md5; then
    cat "${@}" | md5 -q
  fi
}

hash_file() {
  if has md5sum; then
    md5sum "${@}" | cut -c -32
  elif has md5; then
    md5 -q "${@}"
  fi
}

validate_version() {
  local version="$("$(command -v direnv)" version)"
  local major="$(echo "${version}" | cut -d. -f1)"
  local minor="$(echo "${version}" | cut -d. -f2)"
  local patch="$(echo "${version}" | cut -d. -f3)"

  if [[ "${major}" -gt 2 ]]; then return 0; fi
  if [[ "${major}" -eq 2 ]] && [[ "${minor}" -gt 18 ]]; then return 0; fi
  if [[ "${major}" -eq 2 ]] && [[ "${minor}" -eq 18 ]] && [[ "${patch}" -ge 2 ]]; then return 0; fi
  return 1
}

savedrv () {
  if [ "$out" ]
  then
    drv="$(nix show-derivation "$out" | perl -ne 'if(/"(.*\.drv)"/){print$1;exit}')"
    if [ "$drv" ] && [ -e "$drv" ]
    then
      ln -fs "$drv" .drv
      ln -fs "$PWD/.drv" "/nix/var/nix/gcroots/per-user/$LOGNAME/$(basename "$PWD")"
    fi
  fi
}

use_nix "$@"
