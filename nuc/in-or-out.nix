{ token
, channel
, writeShellScriptBin
, curl
}:
let
  secrets = import ../nixos-shared/secrets.nix;
in
writeShellScriptBin {
  name = "in-or-out";
  pure = false;
  failFast = false;
  deps = [ curl ];
} ''
  read -r -d ''' BODY <<EOF
  {
    "channel": "${channel}",
    "text": "Kommst du am Freitag ins Office?",
    "attachments": [
      {
        "text": ":office: Ja"
      },
      {
        "text": ":fire_engine: Nein - Kunde"
      },
      {
        "text": ":house: Nein - Homeoffice"
      },
      {
        "text": ":desert_island: Nein - Urlaub"
      },
      {
        "text": ":shrug: Nein - Sonstiges"
      }
    ]
  }
  EOF

  curl -si -X POST \
           -H "Authorization: Bearer ${token}" \
           -H 'Content-type: application/json' \
           --data "''${BODY}" \
           https://slack.com/api/chat.postMessage
''
