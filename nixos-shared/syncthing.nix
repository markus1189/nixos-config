# Declarative Syncthing mesh, parameterized by the importing host.
#
# Composes with `laptop/syncthing.nix` (which provides `enable`,
# `configDir`, `dataDir`, `user`, `systemService`). This file only
# adds devices, folders, and the override flags.
#
# Adoption is opt-in per host. As of 2026-05-20 only p1g8 imports
# this module; nixos-p1 and nuc still run from their GUI-managed
# config.xml. When migrating those later, the same module file
# stays — they just start importing it.
#
# overrideDevices/overrideFolders = true means the Nix declaration
# is authoritative: GUI changes to devices/folders on importing
# hosts get reverted on the next reload. Folder IDs below are
# copied from the existing config.xml on nixos-p1 (audit
# 2026-05-20) so sync state is preserved.

hostName:
{ config, lib, ... }:

let
  userHome = "/home/${config.lib._custom_.userName}";

  devices = {
    nixos-p1 = {
      id = "PBT7PDM-SECPBXH-H724YUU-CMKVFR6-F32UKAG-FTDX4JV-6HJOIXK-ZZ3RFQA";
      addresses = [ "dynamic" ];
      # Whoever imports this module (other than nixos-p1 itself)
      # trusts nixos-p1 to introduce other peers + folders.
      introducer = true;
    };
    nuc = {
      id = "G4G5COC-OVNF6RC-HGYFMZ7-M2ESBD4-SM4524H-Q6W4H3U-WDQ22D7-VQLTEAU";
      addresses = [ "dynamic" ];
    };
    p1g8 = {
      id = "U7FVYJ3-47AZA2N-SGRVRPT-I26AJXU-M5C7AP4-LR7TJLI-X7KHK4O-LA4PMQ3";
      addresses = [ "dynamic" ];
    };
    S24U = {
      id = "X6DGBOA-UKD3Y2B-JT2TN7T-QXIG2UZ-556RGDO-BV7RCD7-NAKUBXQ-AUTKTAU";
      addresses = [ "dynamic" ];
    };
  };

  # Folder name -> list of all participating device names (incl. self).
  # Audit baseline = nixos-p1's config.xml @ 2026-05-20. p1g8 mirrors
  # nixos-p1 exactly (decision 2026-05-20: all 14 folders).
  folderMembership = {
    remind        = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    ePubs         = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    timejot       = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    Buecher       = [ "nuc" "nixos-p1" "p1g8" ];
    jrnl          = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    activities    = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    buku          = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    ShareToFolder = [        "nixos-p1" "p1g8" "S24U" ];
    rides         = [ "nuc" "nixos-p1" "p1g8" ];
    runs          = [ "nuc" "nixos-p1" "p1g8" ];
    PhotoLogs     = [        "nixos-p1" "p1g8" "S24U" ];
    pen_and_paper = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    Inbox         = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
    finance       = [ "nuc" "nixos-p1" "p1g8" "S24U" ];
  };

  # Folder ID -> human-readable name. IDs preserve sync continuity
  # with the existing peers; without these, Syncthing would mint
  # new IDs and nixos-p1/nuc/S24U would see them as new folders.
  folderIds = {
    remind        = "7w3sr-tjmd4";
    ePubs         = "bldcc-uuzfe";
    timejot       = "dudaq-5whha";
    Buecher       = "fkwvi-pjazp";
    jrnl          = "gvuip-mhtmw";
    activities    = "hxnix-vtagq";
    buku          = "phgrh-e7j2r";
    ShareToFolder = "rh3eg-wjgqe";
    rides         = "spw9m-bqrpq";
    runs          = "ssidi-kckkk";
    PhotoLogs     = "tephm-fyigj";
    pen_and_paper = "unmei-apdtd";
    Inbox         = "x6nxp-oaslb";
    finance       = "ykdhx-5pemk";
  };

  # Folders this host participates in.
  myFolders = lib.filterAttrs
    (_: members: builtins.elem hostName members)
    folderMembership;

  # Peer device names for a folder (= all members minus self).
  peersOf = name: builtins.filter (d: d != hostName) folderMembership.${name};
in
{
  # Declared devices: all known peers except self.
  services.syncthing.settings.devices =
    lib.filterAttrs (n: _: n != hostName) devices;

  # Folders this host participates in, preserving original IDs.
  # path defaults to ~/Syncthing/<name> (matches existing layout).
  services.syncthing.settings.folders = builtins.mapAttrs (name: members: {
    id = folderIds.${name};
    path = "${userHome}/Syncthing/${name}";
    devices = peersOf name;
  }) myFolders;

  services.syncthing.overrideDevices = true;
  services.syncthing.overrideFolders = true;

  # Ensure the parent dir exists for Syncthing to populate folder
  # subdirs into. Syncthing creates the folder dirs themselves;
  # the parent is on us.
  systemd.tmpfiles.rules = [
    "d ${userHome}/Syncthing 0755 ${config.lib._custom_.userName} users -"
  ];
}
