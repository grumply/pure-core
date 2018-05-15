{ mkDerivation, base, containers, pure-default, pure-json
, pure-queue, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-core";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers pure-default pure-json pure-queue pure-txt
  ];
  homepage = "github.com/grumply/pure-core";
  license = stdenv.lib.licenses.bsd3;
}
