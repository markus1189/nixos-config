# Host-specific module for p1g8 (Lenovo ThinkPad P1 Gen 8).
# Counterpart of p1/p1.nix for the new parallel host. Everything not
# overridden here is inherited from ../laptop/laptop.nix + shared modules.
#
# See install-plan §1c for the design rationale, §5 for the NVIDIA /
# Blackwell choices, and §0 for the locked decision record.
{ config, pkgs, lib, ... }:
{
  lib._custom_ = {
    wirelessInterface = "wlp0s20f3";   # confirmed via live-ISO recon (decision #10)
    name = "p1g8";                     # drives nix.nixPath -> ~/repos/nixos-config/p1g8/configuration.nix
  };

  networking.hostName = "p1g8";
  system.stateVersion = "25.11";        # decision #11 — overrides p1.nix's "20.09"

  ## Boot ###################################################################
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 20;   # decision #6: bounded for 1 G ESP
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;    # Arrow Lake CPU + BE201 Wi-Fi need ≥6.13
  hardware.enableRedistributableFirmware = true;

  # NOTE: do NOT carry p1.nix's hand-set `boot.initrd.luks.devices` —
  # disko (./disko.nix) owns LUKS + fileSystems for this host now.
  # NOTE: p1.nix's `services.throttled` is intentionally NOT set here —
  # that's an Intel-throttle workaround for the old CPU; reassess on
  # Arrow Lake if/when symptoms appear.

  ## GPU — NVIDIA Blackwell + Intel iGPU, PRIME offload #####################
  # Decision #7 (confirmed): dGPU exposes 0 DRM connectors; all displays
  # are wired to the Intel iGPU, so offload is the only correct topology.
  # Bus IDs confirmed via live-ISO `lspci -D | grep -iE 'vga|3d'`.
  hardware.nvidia = {
    modesetting.enable = true;
    open = true;                                                 # MANDATORY: Blackwell has no proprietary module
    package = config.boot.kernelPackages.nvidiaPackages.production;  # beta/latest only as fallback
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true;                           # `nvidia-offload` wrapper
      intelBusId  = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };
  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  ## Memory #################################################################
  zramSwap.enable = true;                # daily working-set; the disko 32 G swapfile is OOM backstop

  ## Snapshots — snapper (decision #6) ######################################
  # The NixOS snapper module wires the timeline timer + cleanup +
  # `snapshotRootOnBoot`. There is no built-in pre/post `nixos-rebuild`
  # hook — see install-plan §4 for the shell-alias bracket pattern.
  services.snapper = {
    snapshotRootOnBoot = true;
    configs = {
      root = {
        SUBVOLUME = "/";
        TIMELINE_CREATE = true;
        TIMELINE_CLEANUP = true;
        TIMELINE_LIMIT_HOURLY = 6;
        TIMELINE_LIMIT_DAILY = 7;
        TIMELINE_LIMIT_WEEKLY = 4;
      };
      home = {
        SUBVOLUME = "/home";
        TIMELINE_CREATE = true;
        TIMELINE_CLEANUP = true;
        TIMELINE_LIMIT_HOURLY = 6;
        TIMELINE_LIMIT_DAILY = 7;
        TIMELINE_LIMIT_WEEKLY = 4;
      };
    };
  };
  # Snapper silently no-ops if the .snapshots dirs are missing.
  # disko creates the SUBVOLUMES; these tmpfiles rules create the
  # mountpoint *directories* with the right perms before snapper runs.
  systemd.tmpfiles.rules = [
    "d /.snapshots 0750 root root -"
    "d /home/.snapshots 0750 root root -"
  ];
}
