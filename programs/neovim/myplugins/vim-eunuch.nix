{ pkgs, ... }:
{
  description = ''
    Vim sugar for the UNIX shell commands that need it the most. Features include:

    - :Delete: Delete a buffer and the file on disk simultaneously.
    - :Unlink: Like :Delete, but keeps the now empty buffer.
    - :Move: Rename a buffer and the file on disk simultaneously.
    - :Rename: Like :Move, but relative to the current file's containing directory.
    - :Chmod: Change the permissions of the current file.
    - :Mkdir: Create a directory, defaulting to the parent of the current file.
    - :Cfind: Run find and load the results into the quickfix list.
    - :Clocate: Run locate and load the results into the quickfix list.
    - :Lfind/:Llocate: Like above, but use the location list.
    - :Wall: Write every open window. Handy for kicking off tools like guard.
    - :SudoWrite: Write a privileged file with sudo.
    - :SudoEdit: Edit a privileged file with sudo.
    - File type detection for sudo -e is based on original file name.
    - New files created with a shebang line are automatically made executable.
    - New init scripts are automatically prepopulated with /etc/init.d/skeleton.
  '';
  pkg = pkgs.vimPlugins.vim-eunuch;
}
