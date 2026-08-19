_: super: {
  inherit ((super.callPackage ./default.nix { })) resticPhotoBackup;
  inherit ((super.callPackage ./default.nix { })) resticPhotoForget;
}
