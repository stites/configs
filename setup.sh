#!/bin/bash

function os_type {
  ARCH=$(uname -r | grep -i arch && echo "arch" || echo "")
  UBUNTU=$(uname -a | grep ubuntu && echo "ubuntu" || echo "")
  # do better stuff here
}
function init {
  pkgmanager=$(uname -a | grep ubuntu && echo "apt-get" || echo "yum")
  os_type=$(uname -a | grep ubuntu && echo "ubuntu" || echo "centOS")
  sudo $pkgmanager update -y && sudo $pkgmanager upgrade -y
  sudo $pkgmanager install -y vim wget git

  curl http://j.mp/spf13-vim3 -L -o - | sh
  curl https://github.com/clvv/fasd/tarball/1.0.1 -L -o fasd_dir
  tar xvf fasd_dir
  cd clvv-fasd-4822024
  sudo make install
  cd ..
  rm -rf fasd_dir
  rm -rf clvv-fasd-4822024

  cp -f ./.bash_colors.sh ~/.bash_colors.sh
  cp -f ./.bash_profile   ~/.bash_profile
  cp -f ./.bashrc         ~/.bashrc
}

case "$1" in
  host)
    # Your Start Code
    # Gotta fix this one a little bit!
    ;;
  guest)
    init
    echo 'PS1="${NORMAL}[${BRIGHT_BLUE}\u${BLUE}@`uname -n`${NORMAL}|${BRIGHT_BLACK}\W${NORMAL}] ${RESET}"' >> ~/.bashrc
    ;;
  *)
    echo "Usage: $0 {guest|host}" >&2
    ;;
esac

source ~/.bashrc
source ~/.bash_profile
