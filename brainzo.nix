{ mkDerivation, aeson, base, blaze-html, bytestring, cgi
, config-ini, containers, curl, dbus, directory, fdo-notify, foldl
, happstack-lite, happstack-server, hpack, hspec, lib, MissingH
, mtl, process, regex-applicative-text, safe, sqlite-simple, strict
, system-filepath, text, time, turtle, unordered-containers
}:
mkDerivation {
  pname = "brainzo";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cgi config-ini containers curl dbus directory
    fdo-notify foldl MissingH mtl process regex-applicative-text safe
    sqlite-simple strict system-filepath text time turtle
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base blaze-html bytestring cgi config-ini containers curl dbus
    directory fdo-notify foldl happstack-lite happstack-server MissingH
    mtl process regex-applicative-text safe sqlite-simple strict
    system-filepath text time turtle unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring cgi config-ini containers curl dbus directory
    fdo-notify foldl hspec MissingH mtl process regex-applicative-text
    safe sqlite-simple strict system-filepath text time turtle
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/eddsteel/brainzo#readme";
  license = lib.licenses.bsd3;
}
