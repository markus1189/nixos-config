{ config, pkgs, ...}:

{
  services.mopidy = {
    enable = false;
    configuration = ''
      [spotify]
      username = markus1189@gmail.com
      password = iY0e#FizFb#$z%MpfRVWPlvWxCacR7SOAXURvNv8QoOf^9O6$v
      client_id = d3392f3b-ef62-4905-8abd-4a6812c3cf21
      client_secret = Kl96DYzPqTzkgWgdgbRs6y3yG9PRoWxPoZDMND-ufmg=
      bitrate = 320
      private_session = true

      [http]
      hostname = ::
    '';

    extensionPackages = with pkgs; [ mopidy-spotify mopidy-iris ];
  };

  networking.firewall.allowedTCPPorts = pkgs.lib.optionals config.services.mopidy.enable [ 6680 ];
}
