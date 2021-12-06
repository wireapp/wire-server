{ mkDerivation, attoparsec, base, bytestring, cereal, fetchgit, lib
, mtl, network, text, time
}:
mkDerivation {
  pname = "collectd";
  version = "0.0.0.2";
  src = fetchgit {
    url = "https://github.com/kim/hs-collectd";
    sha256 = "1a3jwj0h2vzgjvzaa4jghmxkjwbzisq4qc7dldc42zi4jaq7lix7";
    rev = "885da222be2375f78c7be36127620ed772b677c9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    attoparsec base bytestring cereal mtl network text time
  ];
  homepage = "https://github.com/kim/hs-collectd";
  description = "collectd Text + Binary Protocols";
  license = lib.licenses.mpl20;
}
