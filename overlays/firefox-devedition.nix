self: super: {
  firefox-devedition = super.wrapFirefox self.firefox-devedition-bin-unwrapped {
    browserName = "firefox";
    nameSuffix = "-devedition";
    name = "firefox-devedition"; # name adds "-bin" which has been removed in recent derivations
    desktopName = "Firefox DevEdition";
  };
}
