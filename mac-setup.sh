# from:
# http://lapwinglabs.com/blog/hacker-guide-to-setting-up-your-mac
# Check for Homebrew,
# Install if we don't have it
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update homebrew recipes
# brew tall GNU core utilities (those that come with OS X are outdated)
brew install coreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils

# Install Bash 4
brew install bash

# Install more recent versions of some OS X tools
brew tap homebrew/dupes
brew install homebrew/dupes/grep --with-default-names

# you'll have to add to path
$PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH

# add binaries
binaries=(
  graphicsmagick
  webkit2png
  rename
  zopfli
  ffmpeg
  python
  sshfs
  trash
  node
  tree
  ack
  hub
  git
)

echo "installing binaries..."
brew install ${binariese[@]}

# cleanup
brew cleanup

# mac apps:
brew install caskroom/cask/brew-cask

# Apps
apps=(
  alfred
  dropbox
  google-chrome
  qlcolorcode
  screenflick
  slack
  transmit
  appcleaner
  firefox
  hazel
  qlmarkdown
  seil
  vagrant
  arq
  flash
  iterm2
  qlprettypatch
  shiori
  sublime-text3
  virtualbox
  flux
  qlstephen
  sketch
  vlc
  cloudup
  nvalt
  quicklook-json
  skype
  transmission
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "installing apps..."
brew cask install --appdir="/Applications" ${apps[@]}

# link alfred
brew cask alfred link

# install fonts:
brew tap caskroom/fonts
# fonts
fonts=(
  font-m-plus
  font-clear-sans
  font-roboto
  font-inconsolata-g-for-powerline
)

# install fonts
echo "installing fonts..."
brew cask install ${fonts[@]}

# install mackup
pip install mackup
mackup restore
mackup backup

# Post setup:
wget https://gist.githubusercontent.com/brandonb927/3195465/raw/376c300cba389908363391d8f2a23e72528dc54d/osx-for-hackers.sh
