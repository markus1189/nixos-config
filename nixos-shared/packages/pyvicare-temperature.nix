{ pythonPackages, writeScript }:

let
  python = with pythonPackages;
    let
      pyvicare = buildPythonPackage rec {
        pname = "PyViCare";
        version = "0.2.4";

        src = fetchPypi {
          inherit pname version;
          sha256 =
            "sha256:1i64iazl5m0h2c862sgd5p73bnizbp2f0jq6i8k3c5x6494vklav";
        };

        propagatedBuildInputs = [ simplejson requests_oauthlib ];
        doCheck = false;
      };
    in python.withPackages (ps: [ pyvicare ]);
in { username, password }:
writeScript "python-vi-care.py" ''
  #!${python}/bin/python

  import os
  from PyViCare.PyViCareGazBoiler import GazBoiler

  t=GazBoiler('${username}','${password}',"token.save")

  print(t.getOutsideTemperature())
''
