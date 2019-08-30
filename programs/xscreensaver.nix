{
  services.xscreensaver.enable = true;
  services.xscreensaver.settings = {
    timeout = "0:02:00";
    lock = true;
    lockTimeout = "0:05:00";
    passwdTimeout = "0:00:30";
    visualID = "default";
    installColormap = true;
    verbose = false;
    timestamp = true;
    splash = true;
    splashDuration = "0:00:05";
    fade = false;
    unfade = false;
    captureStderr = false;
    # XScreenSaver manages screen blanking and display energy saving (DPMS) independently of X itself and overrides it.
    dpmsEnabled = true;
    dpmsQuickOff = false;
    dpmsStandby = "2:00:00";
    dpmsSuspend = "2:00:00";
    dpmsOff = "4:00:00";

    # User switching from the lock screen:
    # Add `xscreensaver.newLoginCommand` to ~/.Xresources, to use LightDM's switching mode:
    newLoginCommand = "dm-tool switch-to-greeter";

    mode = "blank";
    selected = "-1";

    pointerPollTime = "0:00:05"; # default 0:00:05
    pointerHysteresis = "10"; # default 10
    windowCreationTimeout = "0:00:30"; # default 0:00:00

    initialDelay = "0:00:00"; # default 0:00:00
    GetViewPortIsFullOfLies = false; # default false
    procInterrupts = true; # default true
    xinputExtensionDev = false; # default false
    overlayStderr = false; # default false
    authWarningSlack = "20"; # default 20
  };
}

