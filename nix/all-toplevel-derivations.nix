# this tries to recurse into pkgs to collect metadata about packages within nixpkgs 
# it needs a recusionDepth, because pkgs is actually not a tree but a graph so you 
# will go around in circles; also it helps bounding the memory needed to build this 
# we also pass a keyFilter to ignore certain package names 
# else, this just goes through the packages, tries to evaluate them, if that succeeds 
# it goes on and remembers their metadata
# there's a lot of obfuscation caused by the fact that everything needs to be tryEval'd 
# reason being that there's not a single thing in nixpkgs that is reliably evaluatable
{ lib
, pkgSet
, fn
, recursionDepth
, keyFilter
, ...
}:
let
  go = depth: set':
    let
      evaluateableSet = builtins.tryEval set';
    in
    if evaluateableSet.success && builtins.isAttrs evaluateableSet.value
    then
      let
        set = evaluateableSet.value;
      in
      (
        if (builtins.tryEval (lib.isDerivation set)).value
        then
          let
            meta = builtins.tryEval (fn set);
          in
          builtins.deepSeq meta (
            builtins.trace ("reached leaf: " + toString set)
              (
                if meta.success
                then [ meta.value ]
                else builtins.trace "package didn't evaluate" [ ]
              )
          )
        else if depth >= recursionDepth
        then builtins.trace ("max depth of " + toString recursionDepth + " reached") [ ]
        else
          let
            attrVals = builtins.tryEval (builtins.attrValues (lib.filterAttrs (k: _v: keyFilter k) set));
            go' = d: s:
              let
                gone' = builtins.tryEval (go d s);
              in
              if gone'.success
              then gone'.value
              else builtins.trace "could not recurse because of eval error" [ ];
          in
          if attrVals.success
          then
            (builtins.concatMap
              (go' (builtins.trace ("depth was: " + toString depth) (depth + 1)))
              attrVals.value)
          else builtins.trace "could not evaluate attr values because of eval error" [ ]
      )
    else builtins.trace "could not evaluate package or package was not an attrset" [ ];
in
go 0 pkgSet
