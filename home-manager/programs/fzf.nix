{
  enable = true;
  enableBashIntegration = true;
  defaultCommand = "fd --hidden --no-ignore --exclude .git  --ignore-file ~/.config/git/ignore";
  defaultOptions = [ "--bind='ctrl-o:execute(nvim {})+abort'" ];
  fileWidgetCommand="$FZF_DEFAULT_COMMAND";
}
