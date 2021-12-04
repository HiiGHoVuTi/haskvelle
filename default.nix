{ mkDerivation, aeson, base, bytestring, configurator, data-default
, directory, hpack, hs-duktape, lib, pretty-simple, process
, structured-cli, text, time, turtle, unordered-containers
}:
mkDerivation {
  pname = "haskvelle";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring configurator data-default directory
    hs-duktape pretty-simple process structured-cli text time turtle
    unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring configurator data-default directory
    hs-duktape pretty-simple process structured-cli text time turtle
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/HiiGHoVuTi/haskvelle#readme";
  license = lib.licenses.bsd3;
}
