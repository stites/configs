let
  fdcommand = "fd --hidden --no-ignore --exclude .direnv --exclude .git  --ignore-file ~/.config/git/ignore ";
in
{
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    defaultCommand = fdcommand;
    defaultOptions = [ "--bind='ctrl-o:execute(nvim {})+abort'" ];
    fileWidgetCommand= fdcommand + " --exclude result";
  };
}
