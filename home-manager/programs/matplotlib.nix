{
  # Note that this doesn't install matplotlib, it only configures the global properties of it.
  programs.matplotlib = {
    # Whether to enable matplotlib, a plotting library for python.
    enable = true;
    # Add terms to the matplotlibrc file to control the default matplotlib behavior.
    config = {
      backend = "Qt5Agg";
      axes = {
        grid = true;
        facecolor = "black";
        edgecolor = "FF9900";
      };
      grid.color = "FF9900";
    };
    # Additional commands for matplotlib that will be added to the matplotlibrc file.
    # extraConfig = "";
  };
}
