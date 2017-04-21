#!/usr/bin/env bash

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
  pkgmanager=$(uname -a | grep ubuntu && echo "apt-get" || echo "yum")
  os_type=$(uname -a | grep ubuntu && echo "ubuntu" || echo "centOS")
  sudo $pkgmanager update -y && sudo $pkgmanager upgrade -y
  sudo $pkgmanager install -y vim wget git
}

case "$1" in
  host)
    # Your Start Code
    # missing!
    ;;
  guest)
    pkg_managers
    install_fasd
    echo 'PS1="${NORMAL}[${BRIGHT_BLUE}\u${BLUE}@${os_type}${NORMAL}|${BRIGHT_BLACK}\W${NORMAL}] ${RESET}"' >> ~/.bashrc
    ;;
  *)
    echo "Usage: $0 {guest|host}" >&2
    ;;
esac

source ~/.bashrc
