# SEE: http://beyermatthias.de/blog/2015/11/25/how-to-setup-neo-vim-on-nixos/
#
# # in a configuration.nix, you must add this file:
# environment.systemPackages = let
#   # vimPackages = import ./some/directory/vimPackages.nix pkgs;
#
#   neovimPackages = import ./some/directory/neovimPackages.nix pkgs;
# in neovimPacakges ++ with pkgs; [ other packages ];


# Note: vim is not prefixed with a pkgs. here because we define it in the file itself.
[ vim pkgs.ctags ]

customization = {
    vimrcConfig = (import ./customization.nix { pkgs = pkgs; });
} // { name = "vim"; };

