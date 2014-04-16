case "$1" in
  host)
    # Your Start Code
    # Gotta fix this one a little bit!
    ;;
  guest)
    cp -f ./.bash_profile  ~/.bash_profile
    cp -f ./.bashrc        ~/.bashrc
    echo 'PS1="[\e[34;1m\u\[\e[0;34m\]@\H \[\e[1;30m\]\W\e[0m]\$ "' >> ~/.bashrc

    pkgmanager=$(uname -a | grep ubuntu && echo "apt-get" || echo "yum")
    sudo $pkgmanager install -y vim wget
    #curl http://j.mp/spf13-vim3 -L -o - | sh 

    curl https://github.com/clvv/fasd/tarball/1.0.1 -L -o fasd_dir
    tar xvf fasd_dir
    cd clvv-fasd-4822024
    sudo make install
    cd ..
    rm -rf fasd_dir
    rm -rf clvv-fasd-4822024

   #source ~/.bashrc
   #source ~/.bash_profile
    ;;
  *)
    echo "Usage: $0 {guest|host}" >&2
    exit 1
    ;;
esac
