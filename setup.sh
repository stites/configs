#!/usr/bin/env bash

[ -n "$DEBUG" ] && set -x
set -e

if ! command -v git 1>/dev/null 2>&1; then
  echo "setup.sh: Git is not installed, can't continue." >&2
  exit 1
fi

# provision folders (mkdir -p always succeeds)
mkdir -p ~/.tmux/plugins
mkdir -p ~/.config/nvim/autoload
mkdir -p ~/.bash/log

function colorize {
  if [ -t 1 ]; then printf "\e[%sm%s\e[m" "$1" "$2"
  else echo -n "$2"
  fi
}

function checkout {
  [ -d "$2" ] || git clone --depth 1 "$1" "$2"
}

function install_fasd {
  echo "install fasd?"
  # prompt
  curl https://github.com/clvv/fasd/tarball/1.0.1 -L -o fasd_dir
  tar xvf fasd_dir
  cd clvv-fasd-4822024
  sudo make install
  cd ..
  rm -rf fasd_dir
  rm -rf clvv-fasd-4822024
}

function pkg_managers {
  case "$(uname -a)" in
    *"Ubuntu"*)
      echo "hello Ubuntu!"
      sudo apt update -y && sudo apt upgrade -y
      sudo apt install fasd
      sudo apt install vim wget git neovim curl git-lfs
      sudo apt install fonts-firacode
      ;;
    *"ARCH"*)
      echo "hello Archlinux!"
      sudo aura -Ay
      sudo aura -A fasd-git
      sudo pacman -S vim wget git neovim git-lfs
      ;;
    *"FreeBSD"*)
      echo "hello FreeBSD!"
      sudo pkg update && sudo pkg upgrade
      sudo pkg install vim wget git neovim git-lfs
      ;;
  esac
}

if [ -n "$USE_GIT_URI" ]; then
  GITHUB="git://github.com"
else
  GITHUB="https://github.com"
fi

checkout "$GITHUB/jimeh/tmuxifier.git" "$HOME/.tmuxifier"
checkout "$GITHUB/tmux-plugins/tpm" "$HOME/.tmux/plugins/tpm"

checkout "$GITHUB/pyenv/pyenv.git"            "$HOME/.pyenv"
checkout "$GITHUB/pyenv/pyenv-doctor.git"     "$HOME/.pyenv/plugins/pyenv-doctor"
checkout "$GITHUB/pyenv/pyenv-installer.git"  "$HOME/.pyenv/plugins/pyenv-installer"
checkout "$GITHUB/pyenv/pyenv-update.git"     "$HOME/.pyenv/plugins/pyenv-update"
checkout "$GITHUB/pyenv/pyenv-virtualenv.git" "$HOME/.pyenv/plugins/pyenv-virtualenv"
checkout "$GITHUB/pyenv/pyenv-which-ext.git"  "$HOME/.pyenv/plugins/pyenv-which-ext"

if [ ! -f "$HOME/.config/nvim/autoload/plug.vim" ]; then
  curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

pkg_managers

