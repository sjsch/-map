{ mkDerivation, base, bytestring, clay, directory, filepath, friday
, friday-devil, http-media, lucid, monad-logger, mtl, persistent
, persistent-sqlite, persistent-template, random, servant
, servant-lucid, servant-multipart, servant-rawm, servant-server
, stdenv, string-interpolate, text, tomland, wai-app-static, warp
}:
mkDerivation {
  pname = "lambdamap";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath friday friday-devil mtl
  ];
  executableHaskellDepends = [
    base bytestring clay directory filepath http-media lucid
    monad-logger mtl persistent persistent-sqlite persistent-template
    random servant servant-lucid servant-multipart servant-rawm
    servant-server string-interpolate text tomland wai-app-static warp
  ];
  jailbreak = true;
  license = stdenv.lib.licenses.bsd3;
}
