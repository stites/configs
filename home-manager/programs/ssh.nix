{
  enable = true;
  forwardAgent = true;
  serverAliveInterval = 120;
  compression = true;
  controlMaster = "auto";
  controlPath = "/tmp/%r@%h:%p";
  controlPersist = "10m";
  matchBlocks = {
    gauss = { hostname = "10.0.6.48"; };
    erdos = { hostname = "10.0.6.89"; };
    genbu = { hostname = "10.0.6.154"; };
    grothendieck = { hostname = "10.0.6.124"; };
    kovalevskaya = { hostname = "10.0.6.103"; };
    mirzakhani = {
      hostname = "10.0.6.132";
      forwardX11Trusted = true;
    };
    mirzakhani-local = {
      hostname = "10.0.0.12";
      forwardX11Trusted = true;
    };
  };
}
