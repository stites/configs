{
  networking.wireless = {
    # this would allow us to do declarative wireless networking via wpa_supplicant
    enable = false;
    networks = {
      "Sentenai 5G" = { psk = "chinatown"; };
      "PsiOne"      = { psk = "ankadrvchera"; };
      "samborn"     = { psk = "Lila and Lena, April 11"; };
    };
  };
}
