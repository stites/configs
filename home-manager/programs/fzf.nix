let
  fdcommand = "fd --hidden --no-ignore --exclude .git  --ignore-file ~/.config/git/ignore ";
in
{
  enable = true;
  enableBashIntegration = true;
  defaultCommand = fdcommand;
  defaultOptions = [ "--bind='ctrl-o:execute(nvim {})+abort'" ];
  fileWidgetCommand= fdcommand + " --exclude result";
}
