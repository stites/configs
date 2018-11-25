{
  enable = true;
  enableBashIntegration = true;
  defaultCommand = "rg --files --hidden";
  defaultOptions = [ "--bind='ctrl-o:execute(nvim {})+abort'" ];
  fileWidgetCommand="$FZF_DEFAULT_COMMAND";
}
