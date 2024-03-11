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
      canSet = builtins.tryEval set';
    in
    if canSet.success && builtins.isAttrs canSet.value
    then
      let
        set = canSet.value;
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
