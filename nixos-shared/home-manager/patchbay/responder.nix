{ curl, cacert, patchbayUrl }:

{
  value = {
    Unit = { Description = "Respond to patchbay req"; };

    Install = { WantedBy = [ "network-online.target" ]; };

    Service = {
      Type = "simple";
      ExecStart = ''
        ${curl}/bin/curl \
          -v \
          -X POST \
          --no-progress-meter \
          --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
          --url '${patchbayUrl}'
      '';
      Restart = "always";
    };
  };
}
