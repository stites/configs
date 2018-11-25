{
  allowUnfree = true;
  # overlays = [ (self: super: {
  #   weechat = super.weechat.overrideAttrs (oldAttrs: {
  #     version = "devel";
  #     src = self.fetchurl {
  #       url = "https://weechat.org/files/src/weechat-devel.tar.bz2";
  #       sha256 = "102i38hra1g0hml684hsilr2psc1klp3gy206zbndbzm9y75yddq";
  #     };
  #   });
  # } ) ];

  packageOverrides = pkgs_: with pkgs_; {
    haskellEnv = buildEnv {
      name = "haskellEnv";
      paths = [ python36 neovim cabal-install ];
    };

    golangEnv = buildEnv {
      name = "golangEnv";
      paths = [ dep2nix go2nix go ];
    };
  };
}
