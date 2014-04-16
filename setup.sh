case "$1" in
  host)
    # Your Start Code
    # Gotta fix this one a little bit!
    ;;
  guest)
    cp -f ./.bash_profile  ~/.bash_profile
    cp -f ./.bashrc        ~/.bashrc
    source ~/.bashrc
    source ~/.bash_profile
    echo 'PS1="[\e[34;1m\u\[\e[0;34m\]@\H \[\e[1;30m\]\W\e[0m]\$ "' >> ~/.bashrc
    curl http://j.mp/spf13-vim3 -L -o - | sh 
    src
    ;;
  *)
    echo "Usage: $0 {guest|host}" >&2
    exit 1
    ;;
esac
