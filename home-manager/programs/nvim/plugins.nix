{ vimUtils, fetchFromGitHub, stdenv }:

{
  vim-textobj-sentence = vimUtils.buildVimPlugin rec {
    name = "vim-textobj-sentence";
    src = fetchFromGitHub {
      owner = "reedes";
      repo = name;
      rev = "master";
      sha256 = "0mggnsfxdb83a0sa0363qv6gssckmyni1kp5khpbrg5r7c6l0qfj";
    };
    meta = {
      homepage = https://github.com/reedes/vim-textobj-sentence;
      maintainers = [ ];
    };
  };
   vim-textobj-user = vimUtils.buildVimPlugin rec {
    name = "vim-textobj-user";
    src = fetchFromGitHub {
      owner = "kana";
      repo = name;
      rev = "master";
      sha256 = "15wnqkxjjksgn8a7d3lkbf8d97r4w159bajrcf1adpxw8hhli1vc";
    };
    meta = {
      homepage = https://github.com/kana/vim-textobj-user;
      maintainers = [ ];
    };
  };

  papercolor-theme = vimUtils.buildVimPlugin rec {
    name = "papercolor-theme";
    src = fetchFromGitHub {
      owner = "NLKNguyen";
      repo = name;
      rev = "master";
      sha256 = "061551ih3pndnlimxqwa5hrxwn8knpzf87hr6q3zwr9pdfmqpm9l";
    };
    meta = {
      homepage = https://github.com/NLKNguyen/papercolor-theme;
      maintainers = [ stdenv.lib.maintainers.stites ];
    };
  };

  vim-bufkill = vimUtils.buildVimPlugin rec {
    name = "vim-bufkill";
    src = fetchFromGitHub {
      owner = "qpkorr";
      repo = name;
      rev = "master";
      sha256 = "1nji86vjjbfjw4xy52yazq53hrlsr7v30xkx2awgiakz7ih0bdxa";
    };
    meta = {
      homepage = https://github.com/qpkorr/vim-bufkill;
      maintainers = [ ];
    };
  };

  vim-bbye = vimUtils.buildVimPlugin rec {
    name = "vim-bbye";
    src = fetchFromGitHub {
      owner = "moll";
      repo = name;
      rev = "master";
      sha256 = "0dlifpbd05fcgndpkgb31ww8p90pwdbizmgkkq00qkmvzm1ik4y4";
    };
    meta = {
      homepage = https://github.com/moll/vim-bbye;
      maintainers = [ ];
    };
  };

  closetag-vim = vimUtils.buildVimPlugin rec {
    name = "closetag-vim";
    src = fetchFromGitHub {
      owner = "docunext";
      repo = "closetag.vim";
      rev = "master";
      sha256 = "1khnvyxhg2x1rn11p46prpfm11fywyp9jni8dc7zrqib1lhrlsmj";
    };
    meta = {
      homepage = https://github.com/docunext/closetag.vim;
      maintainers = [ stdenv.lib.maintainers.stites ];
    };
  };

  terminalkeys-vim = vimUtils.buildVimPlugin {
    name = "terminalkeys-vim";
    src = fetchFromGitHub {
      owner = "nacitar";
      repo = "terminalkeys.vim";
      rev = "master";
      sha256 = "130sn8d2gzrw628rychk53946vvpwhsj2layhpsa0srp0h9l6snx";
    };
    meta = {
      homepage = https://github.com/nacitar/terminalkeys.vim;
      maintainers = [ ];
    };
  };

  oceanic-next = vimUtils.buildVimPlugin rec {
    name = "oceanic-next";
    src = fetchFromGitHub {
      owner = "mhartington";
      repo = name;
      rev = "master";
      sha256 = "0j7hwi2apm8g9gxsa0ha93f8hah85xx1dyfrcf85zwnydhjl3ac3";
    };
    meta = {
      homepage = https://github.com/mhartington/oceanic-next;
      maintainers = [ ];
    };
  };

  yaflandia = vimUtils.buildVimPlugin rec {
    name = "yaflandia";
    src = fetchFromGitHub {
      owner = "JBakamovic";
      repo = name;
      rev = "master";
      sha256 = "0hn404qi3kh9115dk0fyqzywllyz4jqznjms4lay7nsgcab40asy";
    };
    meta = {
      homepage = https://github.com/JBakamovic/yaflandia;
      maintainers = [ ];
    };
  };

  vim-misc = vimUtils.buildVimPlugin rec {
    name = "vim-misc";
    src = fetchFromGitHub {
      owner = "xolox";
      repo = name;
      rev = "master";
      sha256 = "0rd9788dyfc58py50xbiaz5j7nphyvf3rpp3yal7yq2dhf0awwfi";
    };
    meta = {
      homepage = https://github.com/xolox/vim-misc;
      maintainers = [ ];
    };
  };

  vim-session = vimUtils.buildVimPlugin rec {
    name = "vim-session";
    src = fetchFromGitHub {
      owner = "xolox";
      repo = name;
      rev = "master";
      sha256 = "0r6k3fh0qpg95a02hkks3z4lsjailkd5ddlnn83w7f51jj793v3b";
    };
    meta = {
      homepage = https://github.com/xolox/vim-session;
      maintainers = [ ];
    };
  };

  hspec-vim = vimUtils.buildVimPlugin {
    name = "hspec-vim";
    src = fetchFromGitHub {
      owner = "hspec";
      repo = "hspec.vim";
      rev = "master";
      sha256 = "0lq2sphh2mfciva184b4b3if202hr4yls4d2gzbjx7ibch45zb9i";
    };
    meta = {
      homepage = https://github.com/hspec/hspec.vim;
      maintainers = [ ];
    };
  };

  vim-endwise = vimUtils.buildVimPlugin rec {
    name = "vim-endwise";
    src = fetchFromGitHub {
      owner = "tpope";
      repo = name;
      rev = "master";
      sha256 = "0lq2sphh2mfciva184b4b3if202hr4yls4d2gzbjx7ibch45zb9i";
    };
    meta = {
      homepage = https://github.com/tpope/vim-endwise;
      maintainers = [ ];
    };
  };

  hledger-vim = vimUtils.buildVimPlugin rec {
    name = "hledger-vim";
    src = fetchFromGitHub {
      owner = "anekos";
      repo = name;
      rev = "master";
      sha256 = "0khgfbkx8w4fiix2fcnhg4b4sj98hsrkbg9srqdp3a58dpi105zi";
    };
    meta = {
      homepage = https://github.com/anekos/hledger-vim;
      maintainers = [ ];
    };
  };

  vim-swoop = vimUtils.buildVimPlugin rec {
    name = "vim-swoop";
    src = fetchFromGitHub {
      owner = "pelodelfuego";
      repo = name;
      rev = "master";
      sha256 = "0kqpmgs3x5snb8b83xh6ybqi4nw4l6ljvywgrdhf9jc6wx7ipa9j";
    };
    meta = {
      homepage = https://github.com/pelodelfuego/vim-swoop;
      maintainers = [ ];
    };
  };

  vim-bracketed-paste = vimUtils.buildVimPlugin rec {
    name = "vim-bracketed-paste";
    src = fetchFromGitHub {
      owner = "ConradIrwin";
      repo = name;
      rev = "master";
      sha256 = "1hhi7ab36iscv9l7i64qymckccnjs9pzv0ccnap9gj5xigwz6p9h";
    };
    meta = {
      homepage = https://github.com/ConradIrwin/vim-bracketed-paste;
      maintainers = [ ];
    };
  };

  vim-mundo = vimUtils.buildVimPlugin rec {
    name = "vim-mundo";
    src = fetchFromGitHub {
      owner = "simnalamburt";
      repo = name;
      rev = "master";
      sha256 = "021lyyibqxkvjamb8yjmf6vvw6d1xfypnm1mw6jkjdp2zm46m40h";
    };
    meta = {
      homepage = https://github.com/simnalamburt/vim-mundo;
      maintainers = [ ];
    };
  };

  # fzf = vimUtils.buildVimPlugin rec {
  #   name = "fzf";
  #   buildInputs = [go];
  #   src = (fetchFromGitHub {
  #     owner = "junegunn";
  #     repo = name;
  #     rev = "master";
  #     sha256 = "05lrdp95c3nl9nrpkl13svxp72gwgsvxc6l4hgk03i38fd5ip7s7";
  #   });

  #   meta = {
  #     homepage = https://github.com/junegunn/fzf-wrapper;
  #     maintainers = [ ];
  #   };
  # };


  ghcid = vimUtils.buildVimPlugin rec {
    name = "ghcid";
    src = (fetchFromGitHub {
      owner = "ndmitchell";
      repo = name;
      rev = "master";
      sha256 = "0bwcd4k4sl5nhyyk2660p4dd2gr7in1jrzx8z556idzd1pfv96h2";
    }) + "/plugins/nvim";

    meta = {
      homepage = https://github.com/ndmitchell/ghcid;
      maintainers = [ ];
    };
  };

  vim-kalisi = vimUtils.buildVimPlugin rec {
    name = "vim-kalisi";
    src = fetchFromGitHub {
      owner = "freeo";
      repo = name;
      rev = "master";
      sha256 = "0f4z92ap5qz4dxh1z5g9vhnfv9w8jfs1kp2d24dknkjbng57p5n3";
    };
    meta = {
      homepage = https://github.com/freeo/vim-kalisi;
      maintainers = [ ];
    };
  };

  utl-vim = vimUtils.buildVimPlugin {
    name = "utl-vim";
    src = fetchFromGitHub {
      owner = "vim-scripts";
      repo = "utl.vim";
      rev = "master";
      sha256 = "0ax68nmzlka9193n2h82qzvhzv4dv6lm7rg3b1vhj2pn1r6ci6p4";
    };
    meta = {
      homepage = https://github.com/vim-scripts/utl.vim;
      maintainers = [ ];
    };
  };

  vim-github-colorscheme = vimUtils.buildVimPlugin rec {
    name = "vim-github-colorscheme";
    src = fetchFromGitHub {
      owner = "endel";
      repo = name;
      rev = "master";
      sha256 = "0j3m4hd3sykjk7vs96yh82lmfgga5n6r5b0vqzjg1r2sgyxzig05";
    };
    meta = {
      homepage = https://github.com/endel/vim-github-colorscheme;
      maintainers = [ ];
    };
  };

  vim-brittany = vimUtils.buildVimPlugin rec {
    name = "vim-brittany";
    src = fetchFromGitHub {
      owner = "meck";
      repo = name;
      rev = "master";
      sha256 = "1mrv1sc837v3zmq4wl09kkb3hil7add99npjnflivdm3fc8lar3q";
    };
    meta = {
      homepage = https://github.com/meck/vim-brittany;
      maintainers = [ stdenv.lib.maintainers.stites ];
    };
  };
}
