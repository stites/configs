{ pkgs, lib, ... }:

with lib.types; with lib.attrsets; with lib.lists; with lib.strings; with builtins;

let
  pluginBuilder = pkgs.callPackage ./builder.nix {};
  call = p: pkgs.callPackage p { inherit pluginBuilder; };

  attrIsDeps = p: isList p && p == "dependencies";
  hasDeps = p: hasAttr "dependencies" p;

  compile = depth: p:
    assert p ? pkg ; # && (p ? description || p ? pkg.meta.description);
    assert p ? name || p ? pkg.name;
    assert !(p ? extraConfig) || (isString p.extraConfig || (listOf string).check p.extraConfig);
    assert !(p ? extraConfigs); # make sure there isn't an extraConfigs, plural
    optionalAttrs (!(hasAttr "disable" p) || !p.disable) {
      # "${if p ? name then p.name else p.pkg.name}" = {
      "${if p ? name then p.name else p.pkg.name}" = {
        name = "${if p ? name then p.name else p.pkg.name}";
        inherit (p) pkg;
        inherit depth;
        priority = if p ? priority then p.priority else (-1);
        extraConfig = optionalString (p ? extraConfig)
          (if isString p.extraConfig then p.extraConfig else concatStringsSep "\n" p.extraConfig);
        # all other configs
        home = optionalAttrs (p ? home) p.home;
      };
    };

  mergeplugs = new: memo:
    assert hasAttr "pkg" new;
    assert hasAttr "extraConfig" new;
    assert !(hasAttr "pkg" memo) || (hasAttr "pkg" new && memo.pkg == new.pkg);
    {
      inherit (new) pkg;
      extraConfig = if (memo ? extraConfig) then memo.extraConfig + "\n" + new.extraConfig else new.extraConfig;
      priority = let
          new-priority = if new.priority > -1 then new.depth else new.priority;
        in
          if !(memo ? priority)
          then new-priority
          else
            if memo.priority > new-priority
            then memo.priority
            else new-priority;
      # somehow merge all configs
      # home = memo.home //
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
      getDeps = p: if hasDeps p then p.dependencies else [];
      pairWithDepth = d: xs: map (p: {plug=p; depth=d;}) xs;
      recurse = memo: queue:
        let
          next = (head queue).plug;
          ndepth = (head queue).depth;
          deps = pairWithDepth (ndepth+1) (getDeps next);
        in
          if countAll queue == 0
          then memo
          else recurse (memo ++ singleton (compile ndepth next)) (tail queue ++ deps);
    in recurse [] (pairWithDepth 0 ps);

  smoosher = gplugs: let
      smooshed = foldAttrs mergeplugs {} gplugs;
      pkgs = map (p: p.pkg) (attrValues smooshed);
      countSmoosh = countAll pkgs;
      countPkgs = countAll (unique pkgs);
    in
      assert countSmoosh == countPkgs; # this should never happen
      smooshed;

in rec {

  inherit compile mergeplugs countAll calltree flatten-plugins;

  smoosh = p: smoosher (flatten-plugins (calltree p));
  smooshAll = ps: smoosher (concatMap (p: flatten-plugins (calltree p)) ps);
  smooshAllSorted = ps: sort (a: b: a.priority > b.priority) (attrValues (smooshAll ps));

  validPluginFiles = fps: all validPluginFile fps;

  validPluginFile  = fp: true;
    # # broken right now
    # let
    #   f = pkgs.callPackage filepath {};
    #   expected = ["pkgs" "description" "extraConfig" "disable" "dependencies"];
    #   defaults = ["override" "overrideDerivation" ];
    #   fattrs = subtractLists (attrNames f) expected;
    # in f ? pkgs && fattrs == defaults;
}
