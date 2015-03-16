if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
function gitmv {
  mv $1 $2
  git-mv $2 $1
}
export -f gitmv
