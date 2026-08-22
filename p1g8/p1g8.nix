# Host-specific module for p1g8 (Lenovo ThinkPad P1 Gen 8).
# Counterpart of p1/p1.nix for the new parallel host. Everything not
# overridden here is inherited from ../laptop/laptop.nix + shared modules.
#
# See install-plan §1c for the design rationale, §5 for the NVIDIA /
# Blackwell choices, and §0 for the locked decision record.
{
  config,
  pkgs,
  lib,
  ...
}:
{
  my.wirelessInterface = "wlp0s20f3"; # confirmed via live-ISO recon (decision #10)

  networking.hostName = "p1g8";
  system.stateVersion = "25.11"; # decision #11 — overrides p1.nix's "20.09"

  ## Boot ###################################################################
  # loader + kernelPackages (latest; Arrow Lake CPU + BE201 Wi-Fi need
  # ≥6.13) come from laptop/laptop.nix
  boot.loader.systemd-boot.configurationLimit = 20; # decision #6: bounded for 1 G ESP
  # The i915 cx0_phy / C10 DPLL workarounds (i915.enable_psr=0 + DPMS
  # disabled, Ubuntu bug #2150605) were dropped after the fix landed:
  # 062499cc4813 "drm/i915/mtl+: Enable PPS before PLL", mainline v7.2-rc1,
  # stable 7.1.6 — covers the eDP/PHY-A case seen here. If similar symptoms
  # appear on an *external* DP later, that's the still-open PHY-B sibling
  # issue, not a regression.  (2026-08-22)
  # DDR5 SPD sensor: under Intel SPD-Write-Disable the driver fails to
  # resume (`returns -6`, ENXIO). Canonical's i801 "don't instantiate
  # spd5118 under SPD Write Disable" patch is STILL unmerged as of 2026-08
  # (i2c-i801 master instantiates unconditionally; last list activity a
  # 2026-01 status ping). Ubuntu carries it as a SAUCE patch (LP#2114963).
  # Blacklisting is safe — the module only exposes memory-stick SPD
  # metadata. Re-check: https://patchew.org/linux/20250528-for-upstream-not-instantiate-spd5118-v1-1-8216e2d38918@canonical.com/
  # or un-blacklist and grep journal for `spd5118.*-6` after a resume
  # (failure mode is benign log noise).  (2026-05-27, re-checked 2026-08-22)
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
    open = true; # MANDATORY: Blackwell has no proprietary module
    package = config.boot.kernelPackages.nvidiaPackages.production; # beta/latest only as fallback
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true; # `nvidia-offload` wrapper
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    powerManagement.enable = true; # save VRAM to RAM on suspend (Blackwell — §5)
  };
  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [
    "modesetting"
    "nvidia"
  ];

  # Docker ≥28 discovers GPUs through CDI; with no spec it bails with
  # "failed to discover GPU vendor from CDI: no known GPU vendor found"
  # even though the driver works. This installs nvidia-ctk and generates
  # the CDI spec at /var/run/cdi/nvidia-container-toolkit.json (kind
  # nvidia.com/gpu). NOTE: on Docker 29 `--gpus all` still fails ("AMD CDI
  # spec not found"); the working invocation is the explicit CDI device
  # `--device nvidia.com/gpu=all` (e.g. the netbrain/zwift container — see
  # ~/Stuff/2026-05/28-scratch/zwift-nvidia-docker.md).  (2026-05-28)
  hardware.nvidia-container-toolkit.enable = true;

  ## Steam — PRIME offload to the NVIDIA dGPU ##############################
  # programs.steam pulls in the FHS-wrapped client + the 32-bit driver
  # stack (depends on laptop.nix's `hardware.graphics.enable32Bit`, already
  # set). The client UI itself renders on the Intel iGPU (offload topology —
  # dGPU has 0 display connectors, see GPU section). To make a *game* run on
  # the RTX PRO 2000, set its Steam launch options to:
  #     nvidia-offload %command%
  # (the wrapper sets __NV_PRIME_RENDER_OFFLOAD + __VK_LAYER_NV_optimus=
  # NVIDIA_only). Verify with `nvidia-smi` showing the game process, or
  # MANGOHUD=1 in the same launch options. OpenGL + Vulkan offload both
  # confirmed working at the driver level (2026-06-03).
  programs.steam.enable = true;

  ## Memory #################################################################
  zramSwap.enable = true; # daily working-set; the disko 32 G swapfile is OOM backstop

  # nixpkgs defaults memoryPercent to 50 (config/zram.nix:51), which on 62 G
  # of RAM gave a 31 G compressed-swap sponge -- the single biggest reason the
  # 2026-08-18 stall lasted rather than resolving. 25 % still covers the daily
  # working set (zstd measured ~3.5x here) without letting the machine grind
  # half its RAM through the compressor before anything gives.
  zramSwap.memoryPercent = 25;

  # Reclaim tuning for RAM-backed swap. All three are the kernel defaults
  # today (60 / 3 / 10), which are tuned for spinning disks.
  boot.kernel.sysctl = {
    # Read-ahead of 2^3 = 8 pages per swap-in amortises seek latency. zram has
    # no seek; that is eight decompressions to use one page. 0 is the
    # documented setting for RAM-backed swap.
    "vm.page-cluster" = 0;

    # Prefer swapping anonymous pages over evicting page cache -- correct when
    # swap *is* RAM. 200 is the cap (MAX_SWAPPINESS, mm/vmscan.c).
    # Caveat: this host also has the 32 G disk swapfile at priority -1, so
    # once zram fills, 180 applies to the disk tier too. Acceptable (by then
    # earlyoom should have fired), but do not copy this block to a host
    # without checking its swap topology first.
    "vm.swappiness" = 180;

    # Start reclaim earlier so the kernel is not doing emergency compression
    # at the exact moment the CPU is needed elsewhere.
    "vm.watermark_scale_factor" = 200;
  };

  # fstrim (needed here for LUKS allowDiscards to actually TRIM) and the
  # TrackPoint block come from laptop/laptop.nix

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
        # boot snapshots use the `number` algorithm, which TIMELINE_CLEANUP ignores
        NUMBER_CLEANUP = true;
        NUMBER_LIMIT = 10;
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
