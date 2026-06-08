# p1g8/vagrant.nix
#
# Self-contained Vagrant + libvirt/KVM setup for p1g8 (customer project).
# Provider decision: libvirt/KVM (not VirtualBox). Scope: p1g8 only.
# Removable as a single unit — drop the import in configuration.nix to undo.
#
# NOTE: the `vagrant-libvirt` plugin itself is NOT declared here — it builds
# the native ruby-libvirt gem into the writable ~/.vagrant.d at runtime, which
# the read-only nix store can't host. Install it once, imperatively:
#
#   nix shell nixpkgs#libvirt nixpkgs#pkg-config nixpkgs#ruby \
#             nixpkgs#libxml2 nixpkgs#zlib --command bash -c '
#     export CONFIGURE_ARGS="with-libvirt-include=$(nix eval --raw nixpkgs#libvirt.dev.outPath)/include with-libvirt-lib=$(nix eval --raw nixpkgs#libvirt.outPath)/lib"
#     vagrant plugin install vagrant-libvirt'
#
{ config, pkgs, lib, ... }:
{
  virtualisation.libvirtd.enable = true;

  # libvirtd: manage VMs; kvm: /dev/kvm hardware accel. Merges with the
  # extraGroups list in laptop.nix (docker, vboxusers, video, ...).
  users.extraUsers.${config.lib._custom_.userName}.extraGroups = [
    "libvirtd"
    "kvm"
  ];

  environment.systemPackages = with pkgs; [
    vagrant        # the CLI itself
    virt-manager   # optional GUI to inspect/manage the libvirt domains
  ];

  # Vagrant's libvirt networks use bridges virbr0/virbr1/...; without
  # trusting them, guests hang at "Waiting for domain to get an IP address"
  # because the firewall drops their DHCP requests. Trusting the bridges is
  # the documented fix (NixOS wiki / Discourse).
  networking.firewall.trustedInterfaces = [ "virbr0" "virbr1" "virbr2" ];
}
