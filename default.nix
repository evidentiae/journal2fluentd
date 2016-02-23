{ mkDerivation, aeson, base, bytestring, iso8601-time
, libsystemd-journal, network, pipes, pipes-aeson, pipes-bytestring
, pipes-network, pipes-safe, stdenv, text, time, unix
, unordered-containers
}:
mkDerivation {
  pname = "journal2fluentd";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring iso8601-time libsystemd-journal network pipes
    pipes-aeson pipes-bytestring pipes-network pipes-safe text time
    unix unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
