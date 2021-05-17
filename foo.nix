{ mkDerivation, aeson, base, bytestring, containers, hpack
, optparse-applicative, process, stdenv
}:
mkDerivation {
  pname = "nix-mate";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers process
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers optparse-applicative process
  ];
  testHaskellDepends = [ aeson base bytestring containers process ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/nix-mate#readme";
  license = stdenv.lib.licenses.bsd3;
}
