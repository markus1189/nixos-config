# Declarative partitioning for p1g8 (Lenovo ThinkPad P1 Gen 8).
# Schema: GPT -> 1 GiB ESP -> LUKS2 (passphrase) -> Btrfs subvolumes.
#
# Dual-use file:
#   - CLI:    `disko --mode destroy,format,mount ./p1g8/disko.nix`
#             (one-time partitioning on the live ISO; WIPES the disk).
#   - Module: imported by p1g8/configuration.nix as `./disko.nix` to
#             synthesise `fileSystems` / `boot.initrd.luks.devices`
#             at switch time, which is why `nixos-generate-config
#             --no-filesystems` is safe.
#
# See install-plan §3 for rationale; §6 Phase B for the go/no-go
# `nix-instantiate ... -A config.fileSystems` gate that proves this
# module produced the right fileSystems set before reboot.
{
  disko.devices.disk.main = {
    # CONFIRMED via live-ISO recon 2026-05-19:
    #   Samsung MZVLC1T0HFLU ~1.02 TB, ~87 GB factory data will be wiped.
    device = "/dev/nvme0n1";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          # 1 GiB is deliberate (decision #6): bounded so an overflowed
          # ESP (which itself breaks boot) is structurally hard to reach,
          # and roomy enough for `configurationLimit = 20` generations.
          size = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };

        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "crypted";
            # SSD TRIM passed through LUKS.
            # Trade-off: leaks block-usage pattern to an attacker with
            # device-level access. Accepted for laptop-class threat
            # model; the alternative is meaningful SSD lifetime loss.
            settings.allowDiscards = true;

            content = {
              type = "btrfs";
              extraArgs = [ "-f" ];
              subvolumes = {
                # `@`/`@home` are the live roots; `@snapshots` and
                # `@home-snapshots` are SEPARATE subvolumes so taking
                # a snapshot of `@` does not recurse into `.snapshots`.
                # `@nix` is intentionally NOT snapshotted -- reproducible,
                # huge, GC-churned; snapper would pin freed blocks and
                # turn `nix-collect-garbage` into a no-op.
                "@" = {
                  mountpoint = "/";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@home" = {
                  mountpoint = "/home";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@nix" = {
                  mountpoint = "/nix";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@log" = {
                  mountpoint = "/var/log";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@snapshots" = {
                  mountpoint = "/.snapshots";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@home-snapshots" = {
                  mountpoint = "/home/.snapshots";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                # 32 G swap (decision #5): OOM backstop only -- zram
                # does daily work, hibernation is off (decision #4) so
                # this never needs to fit RAM. `noatime` only; no
                # `compress=zstd` because a swapfile must be NOCOW and
                # the disko swapfile helper handles that.
                "@swap" = {
                  mountpoint = "/swap";
                  mountOptions = [ "noatime" ];
                  swap.swapfile.size = "32G";
                };
              };
            };
          };
        };
      };
    };
  };
}
