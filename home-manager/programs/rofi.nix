{ termcommand }:
{
  programs.rofi = {
    enable = true;
    borderWidth = 1;
    terminal = termcommand;
    theme = "Arc";
  };
}
