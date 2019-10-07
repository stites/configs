{ pkgs, ... }:
{
  programs.htop = {
    enable = true;
    colorScheme = 0; # 0-6; 0
    detailedCpuTime = true; # bool; false
    fields = [ "PID" "USER" "STATE" "M_RESIDENT" "PERCENT_MEM" "PERCENT_CPU" "TIME" "COMM" ];
    hideUserlandThreads = true; # bool; false
    showProgramPath = false; # bool; true # these are all going to be nix-paths or current-system
    meters = {
      left = [ "AllCPUs" "Memory" "Swap" ];
      right = [
        "Hostname"
        "Clock"
        "Uptime"
        "Battery"
        "Blank"
        "Tasks"
        "LoadAverage"
        { kind = "Memory"; mode = 2; }
      ];
    };
  };
}
