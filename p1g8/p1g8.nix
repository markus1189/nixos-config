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
  # i915 cx0_phy / C10 DPLL state-restore race on Arrow Lake-P (8086:7d51)
  # under kernel 7.0.x — see Ubuntu bug #2150605. Symptoms: "Failed to bring
  # PHY A to idle", flip_done timeouts, pixel_rate/port_clock mismatch on
  # resume from long s2idle dwell (≥2 h). PSR-off alone downgrades hard
  # hangs to slow (~10–50 s) recovery; DC/FBC-off shave more. Reassess once
  # the cx0_phy fix lands upstream.  (2026-05-20, expanded 2026-05-27)
  boot.kernelParams = [
    "i915.enable_psr=0"
    "i915.enable_dc=0"
    "i915.enable_fbc=0"
  ];
  # DDR5 SPD sensor: under Intel SPD-Write-Disable the driver fails to
  # resume (`returns -6`, ENXIO). Canonical's i801 "don't instantiate
  # spd5118" patch isn't in mainline 7.0.8 yet; blacklisting is safe — the
  # module only exposes memory-stick SPD metadata.  (2026-05-27)
  boot.blacklistedKernelModules = [ "spd5118" ];
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
    powerManagement.enable = true;                               # save VRAM to RAM on suspend (Blackwell — §5)
  };
  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  ## Memory #################################################################
  zramSwap.enable = true;                # daily working-set; the disko 32 G swapfile is OOM backstop

  ## SSD hygiene ############################################################
  # Required for `LUKS allowDiscards = true` (disko.nix) to actually do
  # TRIM — without this, the documented address-pattern leak buys nothing.
  services.fstrim.enable = true;

  ## TrackPoint (ported from p1.nix; same Elan device on P1 Gen 8) ###########
  hardware.trackpoint = {
    device = "TPPS/2 Elan TrackPoint";
    emulateWheel = true;
    enable = true;
    sensitivity = 112;
    speed = 97;
  };

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
