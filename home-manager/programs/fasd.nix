{
  home.file.".fasdrc".text = ''
    # Fasd defaults to track your "$PWD". Set this to 0 to disable this behavior.
    # _FASD_TRACK_PWD=0

    # List of blacklisted strings. Commands matching them will not be processed, defaults to "--help"
    # _FASD_BLACKLIST="--help"

    # List of all commands that needs to be shifted, defaults to "sudo busybox".
    # _FASD_SHIFT="sudo busybox"

    # List of all commands that will be ignored, defaults to "fasd ls echo".
    # _FASD_IGNORE="fasd ls echo"
  '';
}

