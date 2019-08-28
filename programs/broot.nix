{
  programs.broot.enable = true;
  programs.broot.enableBashIntegration = true;
  # programs.broot.skin = {}; # Default: {}
  # Color configuration.
  # Complete list of keys (expected to change before the v1 of broot):
  #     char_match
  #     code
  #     directory
  #     exe
  #     file
  #     file_error
  #     flag_label
  #     flag_value
  #     input
  #     link
  #     permissions
  #     selected_line
  #     size_bar_full
  #     size_bar_void
  #     size_text
  #     spinner
  #     status_error
  #     status_normal
  #     table_border
  #     tree
  #     unlisted
  # Add _fg for a foreground color and _bg for a background colors.
  # Example:
  # {
  #   status_normal_fg = "grayscale(18)";
  #   status_normal_bg = "grayscale(3)";
  #   status_error_fg = "red";
  #   status_error_bg = "yellow";
  #   tree_fg = "red";
  #   selected_line_bg = "grayscale(7)";
  #   permissions_fg = "grayscale(12)";
  #   size_bar_full_bg = "red";
  #   size_bar_void_bg = "black";
  #   directory_fg = "lightyellow";
  #   input_fg = "cyan";
  #   flag_value_fg = "lightyellow";
  #   table_border_fg = "red";
  #   code_fg = "lightyellow";
  # }

  # programs.broot.verbs
  # Default: {
  #   "create {subpath}" = { execution = "\$EDITOR {directory}/{subpath}"; };
  #   "edit" = { execution = "\$EDITOR {file}"; shortcut = "e"; };
  #   "p" = { execution = ":parent"; };
  #   "view" = { execution = "less {file}"; };
  #   }
  # Define new verbs. The attribute name indicates how the verb is called by the user, with placeholders for arguments.
  # The possible attributes are:
  #   execution (mandatory) how the verb is executed
  #   shortcut (optional) an alternate way to call the verb (without the arguments part)
  #   leave_broot (optional) whether to quit broot on execution (default: true)
  #   from_shell (optional) whether the verb must be executed from the parent shell (default: false)
  # Example:
  #   {
  #     "p" = { execution = ":parent"; };
  #     "edit" = { shortcut = "e"; execution = "$EDITOR {file}" ; };
  #     "create {subpath}" = { execution = "$EDITOR {directory}/{subpath}"; };
  #     "view" = { execution = "less {file}"; };
  #     "blop {name}\\.{type}" = {
  #       execution = "/bin/mkdir {parent}/{type} && /usr/bin/nvim {parent}/{type}/{name}.{type}";
  #       from_shell = true;
  #     };
  #   }
}

