{
  home.file.".codex".text = ''
    currentProjectIncluded: true
    hackagePath: ${builtins.getEnv "HOME"}/.cabal/packages/hackage.haskell.org/
    tagsFileHeader: false
    tagsFileName: codex.tags
    tagsFileSorted: false
    tagsCmd: hasktags --ctags --extendedctag --follow-symlinks --output="$TAGS" "$SOURCES"
    stackOpts: ""
  '';
}

