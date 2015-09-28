if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

# Add GHC 7.10.2 to the PATH, via https://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/ghc-7.10.2.app"
if [ -d "$GHC_DOT_APP" ]; then
  export PATH="${HOME}/.local/bin:${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

if [ -e /Users/stites/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/stites/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
