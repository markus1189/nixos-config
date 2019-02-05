{ config, pkgs, ...}:

{
  environment = {
    systemPackages = [ pkgs.resticPhotoBackup pkgs.resticPhotoForget ];
  };

  nixpkgs = {
    overlays = [ (import ./overlay.nix) ];
  };
}
