{ mkDerivation, base, containers, directory, filepath
, generic-deriving, lens, mtl, parsec, pretty, stdenv, tasty
, tasty-hunit
}:
mkDerivation {
  pname = "core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers generic-deriving lens mtl parsec pretty
  ];
  testHaskellDepends = [ base directory filepath tasty tasty-hunit ];
  homepage = "https://github.com/themattchan/core#readme";
  description = "Implementing functional languages";
  license = stdenv.lib.licenses.mit;
}
