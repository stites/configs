TODO
==========
- [ ] add hosts to nixos (maybe by virtue of [fps/nixos-addblock-hosts](https://github.com/fps/nixos-addblock-hosts) or SevenBlack/hosts)
- [x] encode `bashrc`s into nix
- [ ] switch to wireguard
- [ ] grothendieck -> NVME
- [ ] grothendieck -> zfs + mirroring
- [ ] theme gtk
- [ ] theme taffybar
- [ ] set up notifier with taffybar
- [ ] remove `os/`
- [ ] remove `nix/`
- [ ] remove `conjobs/`
- [ ] remove `config/{alacritty,haskell-vim-now,redshift}`
- [ ] revamp `glirc/` -- replace with weechat?
- [ ] encode `home/` into home-manager
- [ ] make `nixpkgs` top-level
- [ ] set up cuda on grothendieck
- [ ] set up NixOS so that critical battery usage hibernates system, inactive screen goes to hybrid-suspend
- [ ] NixOS keyboard bindings
- [ ] add backlight kernel module
- [ ] Variational methods missing from [wikipedia](https://en.wikipedia.org/wiki/Bayesian_inference) @.@

Setup
==========

To initiate configs on a computer run:

    ./setup.sh
    ./run_sus deploy

Other packages to install:

+ StevenBlack/hosts
+ redshift (caffeine for mac)
+ nix
+ albert
+ FiraCode font (dependency from urxvt), and NerdFonts

Mac-specific:
- Dropbox, Flux, iTerm, Slack, Path Finder, Spectacle
- download Java, DCEVM
+ download fonts (clone powerline/fonts, run install.sh)
+ download iterm color schemes:
   - git clone mbadolato/iTerm2-Color-Schemes

