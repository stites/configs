rec {
  homedir = builtins.getEnv "HOME";
  confroot = "${homedir}/git/configs/";
}
