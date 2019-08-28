{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-full
        # scheme-medium
        collection-fontsrecommended
        algorithms
        latexmk;
        # xelatex;
    };
  };
}
