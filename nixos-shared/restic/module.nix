{ config, pkgs, ... }:

{
  age.secrets.resticB2 = {
    file = ../../secrets/restic-b2.env.age;
    name = "restic-b2.env";
  };

  environment = {
    systemPackages = [
      pkgs.resticPhotoBackup
      pkgs.resticPhotoForget
    ];
  };

  nixpkgs = {
    overlays = [ (import ./overlay.nix) ];
  };
}
