{ mkDerivation, base, containers, directory, filepath, lib, time }:
mkDerivation {
  pname = "hpc";
  version = "0.6.0.3";
  sha256 = "16dac73e55899815a32715c35fa5986da077d7e7887ce8ef1c72b4f33a73a2aa";
  revision = "3";
  editedCabalFile = "06dbiaf0sangq3zdyr3x9wkvs2fgyas3ipqkfwfmycax6j17jgyy";
  libraryHaskellDepends = [
    base containers directory filepath time
  ];
  description = "Code Coverage Library for Haskell";
  license = lib.licenses.bsd3;
}
