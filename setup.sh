#!/usr/bin/env bash

# provision folders (mkdir -p always succeeds)
mkdir -p ~/.tmux/plugins
mkdir -p ~/.config/nvim/autoload
mkdir -p ~/.bash/log

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
      sudo apt install vim wget git neovim curl
      ;;
    *"ARCH"*)
      echo "hello Archlinux!"	
      sudo aura -Ay
      sudo aura -A fasd-git
      sudo pacman -S vim wget git neovim
      ;;
    *"FreeBSD"*)
      echo "hello FreeBSD!"	
      sudo pkg update && sudo pkg upgrade
      sudo pkg install vim wget git neovim
      ;;
  esac
}

if [ ! -d "$HOME/.tmuxifier" ]; then
  git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier
fi

if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

if [ ! -f "$HOME/.config/nvim/autoload/plug.vim" ]; then
  curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

pkg_managers
