# collects information about a single nixpkgs package
{ lib
, pkg
, ...
}:
with builtins;
assert lib.isDerivation pkg; let
  # trace with reason
  trc = info: pkg: trace (info + ": " + toString pkg);

  # if thing is a list, map the function, else apply f to thing and return a singleton of
  # it
  mapOrSingleton = f: x:
    if isList x
    then map f x
    else [ (f x) ];

  # things to save from the src attr (the derivation that was created by a fetcher)
  srcInfo = {
    urls = (pkg.src.urls or (trc "package didn't have src or url" pkg [ ])) ++ [ (pkg.src.url or null) ];
  };

  dp = builtins.tryEval pkg.drvPath;

  # things to save from the meta attr
  metaInfo =
    let
      m = pkg.meta or (trc "package didn't have meta" pkg { });
    in
    {
      homepage = m.homepage or (trc "package didn't have homepage" pkg null);
      description = m.description or (trc "package didn't have description" pkg null);
      licenseSpdxId =
        mapOrSingleton
          (
            l: {
              id = l.spdxId or (trc "package license doesn't have a spdxId" pkg null);
              name = l.fullName or (trc "package license doens't have a name" pkg null);
            }
          )
          (m.license or (trc "package does not have a license" pkg null));

      # based on heuristics, figure out whether something is an application for now this only checks whether this
      # componnent has a main program
      type =
        if m ? mainProgram
        then "application"
        else "library";

      name = pkg.pname or pkg.name or (trc "name is missing" pkg null);
      version = pkg.version or (trc "version is missing" pkg null);
    };
in
if dp.success
then
  let
    info = builtins.toJSON (srcInfo // metaInfo // { drvPath = builtins.unsafeDiscardStringContext dp.value; });
  in
  info
else trc "drvPath of package could not be computed" pkg { }
