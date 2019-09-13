{ pkgs, lib, ... }:

with lib.types; with lib.attrsets; with lib.lists; with lib.strings; with builtins;

let
  call = p: pkgs.callPackage p {};

  attrIsDeps = p: isList p && p == "dependencies";
  hasDeps = p: hasAttr "dependencies" p;
  getDeps = p: if hasDeps p then p.dependencies else [];

  compile = p:
    assert p ? pkg ; # && (p ? description || p ? pkg.meta.description);
    assert p ? name || p ? pkg.name;
    assert !(p ? extraConfig) || (isString p.extraConfig || (listOf string).check p.extraConfig);
    optionalAttrs (!(hasAttr "disable" p) || !p.disable) {
      # "${if p ? name then p.name else p.pkg.name}" = {
      "${if p ? name then p.name else p.pkg.name}" = {
        name = "${if p ? name then p.name else p.pkg.name}";
        inherit (p) pkg;
        extraConfig = optionalString (p ? extraConfig)
          (if isString p.extraConfig then p.extraConfig else concatStringsSep "\n" p.extraConfig);
      };
    };

  mergeplugs = new: memo:
    assert hasAttr "pkg" new;
    assert hasAttr "extraConfig" new;
    assert !(hasAttr "pkg" memo) || (hasAttr "pkg" new && memo.pkg == new.pkg);
    {
      inherit (new) pkg;
      extraConfig = if (memo ? extraConfig) then memo.extraConfig + "\n" + new.extraConfig else new.extraConfig;
    };

  countAll = count (a: true);

  calltree = path:
    let
      recurse = memo: queue:
        let
          next = call (head queue);
          go = name: value:
            if name == "dependencies" && isList value
            then recurse [] value
            else value;
        in
          if countAll queue == 0
          then memo
          else recurse (memo ++ singleton (mapAttrs go next)) (tail queue);

    in recurse [] [path];

  flatten-plugins = ps:
    assert isList ps;
    let
      recurse = memo: queue:
        let
          next = head queue;
        in
          if countAll queue == 0
          then memo
          else recurse (memo ++ singleton (compile next)) (tail queue ++ getDeps next);
    in recurse [] ps;

  smoosher = gplugs: let
      smooshed = foldAttrs mergeplugs {} gplugs;
      pkgs = map (p: p.pkg) (attrValues smooshed);
      countSmoosh = countAll pkgs;
      countPkgs = countAll (unique pkgs);
    in
      assert countSmoosh == countPkgs; # this should never happen
      smooshed;

  smoosh = p: smoosher (flatten-plugins (calltree p));
  smooshAll = ps: smoosher (concatMap (p: flatten-plugins (calltree p)) ps);
in
{
  inherit compile mergeplugs countAll calltree flatten-plugins smoosh smooshAll;
}
