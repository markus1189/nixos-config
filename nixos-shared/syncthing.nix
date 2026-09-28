# Declarative Syncthing mesh, keyed on the importing host's
# `networking.hostName` (device names below match hostnames exactly).
#
# Composes with `./syncthing-base.nix` (which provides `enable`,
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
#
# Each folder is written whole (the module POSTs the folder object),
# so a folder setting that is not declared here -- versioning -- is
# reset to Syncthing's default on the next rebuild.

{ config, lib, ... }:

let
  hostName = config.networking.hostName;
  userHome = "/home/${config.my.userName}";

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
    S26U = {
      id = "MFFYFYP-NZWAYWI-44U6V42-7TXKQYO-F5SGUZR-SC3EO2S-V6BXT46-XLPJCQ7";
      addresses = [ "dynamic" ];
    };
  };

  # Folder name -> { id; members; } plus optional `path` (default
  # ~/Syncthing/<name>) and any other Syncthing folder attribute
  # (versioning, ignorePatterns, ...), passed through as is.
  # `id` preserves sync continuity with the existing peers; without it
  # Syncthing would mint a new ID and the peers would see a new folder.
  # `members` lists all participating device names, self included.
  #
  # Audit baseline = nixos-p1's config.xml @ 2026-05-20. p1g8 mirrors
  # nixos-p1 exactly (decision 2026-05-20: all 14 folders).
  # 2026-06-04: S26U replaces S24U (new phone) across all folders;
  # added Audiobooks (offered by S26U + nixos-p1, nuc also joins).
  # 2026-09-07: cooklang joins, offered to p1g8 by nuc and S26U.
  # nixos-p1 does not have it, so it is not a member.
  folders = {
    cooklang = {
      id = "exkwq-4skde";
      members = [
        "nuc"
        "p1g8"
        "S26U"
      ];
    };
    Audiobooks = {
      id = "azmve-vrodw";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    remind = {
      id = "7w3sr-tjmd4";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    ePubs = {
      id = "bldcc-uuzfe";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    timejot = {
      id = "dudaq-5whha";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    Buecher = {
      id = "fkwvi-pjazp";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
      ];
    };
    jrnl = {
      id = "gvuip-mhtmw";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    activities = {
      id = "hxnix-vtagq";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    buku = {
      id = "phgrh-e7j2r";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    ShareToFolder = {
      id = "rh3eg-wjgqe";
      members = [
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    rides = {
      id = "spw9m-bqrpq";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
      ];
    };
    runs = {
      id = "ssidi-kckkk";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
      ];
    };
    PhotoLogs = {
      id = "tephm-fyigj";
      members = [
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    pen_and_paper = {
      id = "unmei-apdtd";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    Inbox = {
      id = "x6nxp-oaslb";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    finance = {
      id = "ykdhx-5pemk";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
  };

  # Folders this host participates in.
  myFolders = lib.filterAttrs (_: f: builtins.elem hostName f.members) folders;
in
{
  # Declared devices: all known peers except self.
  services.syncthing.settings.devices = lib.filterAttrs (n: _: n != hostName) devices;

  # Folders this host participates in, preserving original IDs.
  # path defaults to ~/Syncthing/<name> (matches existing layout).
  services.syncthing.settings.folders = builtins.mapAttrs (
    name: f:
    {
      path = "${userHome}/Syncthing/${name}";
    }
    // removeAttrs f [ "members" ]
    // {
      # Peer device names (= all members minus self).
      devices = builtins.filter (d: d != hostName) f.members;
    }
  ) myFolders;

  services.syncthing.overrideDevices = true;
  services.syncthing.overrideFolders = true;

  # Ensure the parent dir exists for Syncthing to populate folder
  # subdirs into. Syncthing creates the folder dirs themselves;
  # the parent is on us.
  systemd.tmpfiles.rules = [
    "d ${userHome}/Syncthing 0755 ${config.my.userName} users -"
  ];
}
