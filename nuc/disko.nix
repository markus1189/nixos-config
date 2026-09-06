# Declarative partitioning for nuc (Intel NUC media box).
# Schema: GPT -> 1 GiB ESP -> Btrfs subvolumes. No LUKS.
#
# Dual-use file, same as p1g8/disko.nix:
#   - CLI:    `disko --mode destroy,format,mount ./nuc/disko.nix`
#             (one-time partitioning; WIPES the target disk).
#   - Module: imported by nuc/configuration.nix to synthesise
#             `fileSystems` / `swapDevices` at switch time. Adding that
#             import REQUIRES deleting `fileSystems."/"` and
#             `fileSystems."/boot"` from hardware-configuration.nix --
#             two definitions of one mountpoint is an eval conflict.
#
# Why no LUKS (decided 2026-09-06): SDDM autologins a wheel user into
# Plasma, so any self-unlocking scheme (TPM2, ESP keyfile) is defeated by
# whole-machine theft, which is the realistic threat -- not a lone stolen
# SSD. The only variant that protects is a boot passphrase, and that
# strands the unattended nightly work (autoUpgrade, restic, wireguard,
# telegram notifications) after every power cut. Disposal is covered by
# `nvme format --ses=1` at removal time instead. The box has a TPM
# (/dev/tpm0) if this is ever revisited; note that PCR policies break on
# bootloader/kernel updates, which here happen nightly at 04:21.
{
  disko.devices.disk.main = {
    # by-id, deliberately: four disks are attached, three of them full of
    # media, and `destroy` does not ask twice. CONFIRMED 2026-09-06 from
    # /dev/disk/by-id on the running system -- Samsung 990 EVO Plus 1 TB,
    # factory-fresh (percentage_used 0%, power_on_hours 0).
    device = "/dev/disk/by-id/nvme-Samsung_SSD_990_EVO_Plus_1TB_S7U4NU1YB11261Z";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          # 1 GiB, up from the old disk's 511 MiB: an ESP that overflows
          # breaks boot, and kernels+initrds accumulate per generation.
          size = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };

        root = {
          size = "100%";
          content = {
            type = "btrfs";
            extraArgs = [ "-f" ];
            subvolumes = {
              # Layout mirrors p1g8 minus the LUKS layer, so both hosts
              # read the same way. `@nix` is intentionally NOT snapshotted:
              # reproducible, huge, GC-churned -- snapshots would pin freed
              # blocks and turn nix-collect-garbage into a no-op.
              "@" = {
                mountpoint = "/";
                mountOptions = [
                  "compress=zstd"
                  "noatime"
                ];
              };
              "@home" = {
                mountpoint = "/home";
                mountOptions = [
                  "compress=zstd"
                  "noatime"
                ];
              };
              "@nix" = {
                mountpoint = "/nix";
                mountOptions = [
                  "compress=zstd"
                  "noatime"
                ];
              };
              "@log" = {
                mountpoint = "/var/log";
                mountOptions = [
                  "compress=zstd"
                  "noatime"
                ];
              };
              # Separate subvolumes so snapshotting `@` does not recurse
              # into `.snapshots`. Snapper is not wired up on nuc yet; the
              # subvolumes exist so adding it later is a config change and
              # not a repartition.
              "@snapshots" = {
                mountpoint = "/.snapshots";
                mountOptions = [
                  "compress=zstd"
                  "noatime"
                ];
              };
              "@home-snapshots" = {
                mountpoint = "/home/.snapshots";
                mountOptions = [
                  "compress=zstd"
                  "noatime"
                ];
              };
              # 8 GiB OOM backstop only. The box has 4 GB installed
              # (3.72 GiB usable, MemTotal 3901052 kB), so zram carries the
              # daily load (see configuration.nix); hibernation is off, so
              # this never needs to fit RAM. `noatime` only, no
              # `compress=zstd`: a swapfile must be NOCOW, which the disko
              # swapfile helper sets up.
              "@swap" = {
                mountpoint = "/swap";
                mountOptions = [ "noatime" ];
                swap.swapfile.size = "8G";
              };
            };
          };
        };
      };
    };
  };
}
