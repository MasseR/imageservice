{ mkDerivation, aeson, async, base, bytestring, comonad, conduit
, conduit-extra, containers, dhall, directory, either, ekg-core
, exceptions, filepath, foldl, generic-lens, genvalidity
, genvalidity-aeson, genvalidity-containers, genvalidity-hspec
, genvalidity-text, genvalidity-time, hspec, html-conduit
, http-client, http-client-tls, http-conduit, http-types
, JuicyPixels, JuicyPixels-extra, katip, lens, lens-aeson
, masse-prelude, mtl, network-uri, optparse-generic
, recursion-schemes, resourcet, safecopy, servant, servant-docs
, servant-server, split, sqlite-simple, stdenv, stm, temporary
, text, time, transformers, unliftio, validity, wai, wai-cors
, wai-middleware-metrics, warp, xml-conduit, xml-lens
}:
mkDerivation {
  pname = "imageservice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring comonad conduit conduit-extra
    containers dhall directory either ekg-core exceptions filepath
    foldl generic-lens genvalidity genvalidity-aeson
    genvalidity-containers genvalidity-text genvalidity-time
    html-conduit http-client-tls http-conduit http-types JuicyPixels
    JuicyPixels-extra katip lens lens-aeson masse-prelude mtl
    network-uri optparse-generic recursion-schemes resourcet safecopy
    servant servant-docs servant-server split sqlite-simple stm
    temporary text time transformers unliftio wai wai-cors
    wai-middleware-metrics warp xml-conduit xml-lens
  ];
  testHaskellDepends = [
    aeson async base bytestring comonad conduit conduit-extra
    containers dhall directory either ekg-core exceptions filepath
    generic-lens genvalidity genvalidity-aeson genvalidity-containers
    genvalidity-hspec genvalidity-text genvalidity-time hspec
    html-conduit http-client http-client-tls http-conduit http-types
    JuicyPixels JuicyPixels-extra katip lens lens-aeson masse-prelude
    mtl network-uri optparse-generic recursion-schemes resourcet
    safecopy servant servant-docs servant-server split sqlite-simple
    stm temporary text time transformers unliftio validity wai wai-cors
    wai-middleware-metrics warp xml-conduit xml-lens
  ];
  license = stdenv.lib.licenses.bsd3;
}
