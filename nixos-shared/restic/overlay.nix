_: super:
let
  # One instantiation, two attributes -- callPackage was previously run once
  # per attribute for the same package set.
  resticScripts = super.callPackage ./default.nix { };
in
{
  inherit (resticScripts) resticPhotoBackup resticPhotoForget;
}
