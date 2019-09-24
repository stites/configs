{ pkgs, lib, ... }:

with lib.strings;

let
  key-mapper = prefix: { key, cmd, silent ? false, plugin ? false}:
      "${prefix}map ${lib.optionalString silent "<silent>"} ${key} " + (if plugin then "<Plug>(${cmd})" else cmd);

  keylib = {
    nmap     = key-mapper "n";
    xmap     = key-mapper "x";
    nnoremap = key-mapper "nnore";
    inoremap = key-mapper "inore";
  };
in
{
  lib = keylib;
  leader = "<leader>";
  # TODO: make an attrset so you can find user-defined collisions.
}
