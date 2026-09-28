# Declarative Syncthing mesh, keyed on the importing host's
# `networking.hostName` (device names below match hostnames exactly).
#
# Imports `./syncthing-base.nix` (which provides `enable`,
# `configDir`, `dataDir`, `user`, `systemService`) and adds devices,
# folders, and the override flags. Hosts import this file, never the
# base alone, so no host runs a GUI-managed Syncthing: p1g8 since
# 2026-05-20, nixos-p1 and nuc since 2026-09 (laptop/laptop.nix,
# nuc/configuration.nix).
#
# overrideDevices/overrideFolders = true means the Nix declaration
# is authoritative: GUI changes to devices/folders on importing
# hosts get reverted on the next reload. Folder IDs below are
# copied from the existing config.xml on nixos-p1 (audit
# 2026-05-20) so sync state is preserved.
#
# Each folder is written whole (the module POSTs the folder object),
# so a folder setting that is not declared here -- versioning -- is
# reset to Syncthing's default on the next rebuild. Ignore patterns
# are the exception: they live in .stignore and are only written when
# `ignorePatterns` is set; `[ ]` clears them, unset leaves them alone.

{ config, lib, ... }:

let
  hostName = config.networking.hostName;
  userHome = "/home/${config.my.userName}";

  # Keep the newest `keep` versions for `days` days, checked hourly: the
  # parameters the GUI had set.
  simpleVersioning = days: {
    type = "simple";
    params = {
      keep = "5";
      cleanoutDays = toString days;
    };
    cleanupIntervalS = 3600;
  };

  # Default for every folder that sets no versioning of its own: a file
  # deleted or replaced by a change from another device lands in
  # .stversions (one copy per name) for 14 days. Local changes are not
  # archived locally, only on the peers they reach. Added for the
  # 2026-09 migration, when histories that never met get merged.
  trashcanVersioning = {
    type = "trashcan";
    params.cleanoutDays = "14";
    cleanupIntervalS = 3600;
  };

  devices = {
    nixos-p1 = {
      id = "PBT7PDM-SECPBXH-H724YUU-CMKVFR6-F32UKAG-FTDX4JV-6HJOIXK-ZZ3RFQA";
      addresses = [ "dynamic" ];
      # No introducer: it re-added devices nixos-p1 still knew (the
      # retired S24U) behind overrideDevices' back. The mesh is fully
      # declared here, so nothing needs introducing.
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
  # 2026-09-27: S24U retired; LocusMaps declared (was S24U-only);
  # nuc joins PhotoLogs and ShareToFolder (its copies were S24U-only).
  # Versioning and ignores copied from the GUI configs they lived in.
  # 2026-09-28: S26U leaves activities, finance, pen_and_paper. S24U had
  # them, S26U never accepted them; declared since 2026-06-04 regardless.
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
      # ~4G of churn a month; a trashcan would hold that again on
      # every peer, for books that can be downloaded again. null is the
      # module's default and is stripped, so the folder gets none.
      versioning = null;
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    remind = {
      id = "7w3sr-tjmd4";
      # Was set in nuc's GUI.
      versioning = simpleVersioning 7;
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
      ];
    };
    buku = {
      id = "phgrh-e7j2r";
      # Was set in nixos-p1's GUI.
      versioning = simpleVersioning 14;
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
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    rides = {
      id = "spw9m-bqrpq";
      # Explicitly empty: clears nixos-p1's GUI-set gpx-only .stignore.
      # Unset would leave it alone (the module only writes ignores it is given).
      ignorePatterns = [ ];
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
      ];
    };
    runs = {
      id = "ssidi-kckkk";
      # Explicitly empty: clears nixos-p1's GUI-set gpx-only .stignore.
      # Unset would leave it alone (the module only writes ignores it is given).
      ignorePatterns = [ ];
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
      ];
    };
    PhotoLogs = {
      id = "tephm-fyigj";
      members = [
        "nuc"
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
    LocusMaps = {
      id = "xfd64-z5r8v";
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
        "S26U"
      ];
    };
    finance = {
      id = "ykdhx-5pemk";
      # Was nuc's .stignore.
      ignorePatterns = [ ".direnv" ];
      members = [
        "nuc"
        "nixos-p1"
        "p1g8"
      ];
    };
  };

  # Folders this host participates in.
  myFolders = lib.filterAttrs (_: f: builtins.elem hostName f.members) folders;
in
{
  imports = [ ./syncthing-base.nix ];

  # Declared devices: all known peers except self.
  services.syncthing.settings.devices = lib.filterAttrs (n: _: n != hostName) devices;

  # Folders this host participates in, preserving original IDs.
  # path defaults to ~/Syncthing/<name> (matches existing layout).
  services.syncthing.settings.folders = builtins.mapAttrs (
    name: f:
    {
      path = "${userHome}/Syncthing/${name}";
      versioning = trashcanVersioning;
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
