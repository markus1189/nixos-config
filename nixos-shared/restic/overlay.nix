self: super: {
  resticPhotoBackup = (super.callPackage ./default.nix {}).resticPhotoBackup;
  resticPhotoForget = (super.callPackage ./default.nix {}).resticPhotoForget;
}
