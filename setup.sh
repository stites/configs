#!/bin/bash

pkgmanager=$(uname -a | grep -i ubuntu 1> /dev/null && echo "apt-get" || echo "yum"  )
os_type=$(uname -a    | grep -i ubuntu 1> /dev/null && echo "ubuntu"  || echo "linux")

function pkg_managers {
  sudo $pkgmanager update -y && sudo $pkgmanager upgrade
  sudo $pkgmanager install -y vim wget git neovim
}

function init_bashrcs {
  echo "setting up bashrcs"
  mkdir  -p ~/.bash/log/
  cp -f {.,~}/.bash_colors
  cp -f {.,~}/.bash_profile 
  cp -f {.,~}/.bashrc      
  cp -f {.,~}/.bashrc_git 
  cp -f {.,~}/.bashrc_ssh
  cp -f {.,~}/.bashrc_tmux
  # cp -f {.,~}/.bashrc_vim # implies neovim
  cp -f {.,~}/.tmux.conf
  cp -f {.,~}/.gitignore_global
  cp -Rf {.,~}/.tmuxinator
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


case "$1" in
  host)
    # Your Start Code
    # missing!
    ;;
  guest)
    pkg_managers
    install_fasd
    init_bashrcs
    echo 'PS1="${NORMAL}[${BRIGHT_BLUE}\u${BLUE}@${os_type}${NORMAL}|${BRIGHT_BLACK}\W${NORMAL}] ${RESET}"' >> ~/.bashrc
    ;;
  *)
    echo "Usage: $0 {guest|host}" >&2
    ;;
esac

source ~/.bashrc
