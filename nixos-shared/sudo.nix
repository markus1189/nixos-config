{ config, lib, ... }:

# Shared sudo policy for every host, imported next to ./user.nix (which
# creates the account it talks about). Hosts add only what is host-local:
# the laptops their insult-carrying sudo package, nuc a comment on why its
# timeout stays where it is.
#
# `execWheelOnly` makes the setuid wrapper root:wheel 4750 instead of 4755,
# so accounts outside wheel cannot exec sudo at all. That is the mitigation
# for the sudo holes which never needed a sudoers entry in the first place
# (CVE-2021-3156 "Baron Samedit", CVE-2025-32463 `--chroot`): reaching the
# binary was the entire prerequisite. Its one failure mode is an extraRules
# entry naming someone outside root/wheel, so the assertion below - srvos's -
# turns that into a build error instead of a login that stopped working.
{
  options.my.sudoTimeout = lib.mkOption {
    type = lib.types.ints.unsigned;
    default = 5;
    description = ''
      Minutes sudo caches credentials for the primary user: how long an
      unattended unlocked terminal remains root. 5 is sudo's own default,
      CIS 5.2.6 caps it at 15.
    '';
  };

  config = {
    security.sudo = {
      enable = true;
      execWheelOnly = true;

      # Scoped to the primary user, not global - no other account on these
      # hosts should inherit a credential cache. `lecture = never` drops
      # /var/db/sudo/lectured, mutable state kept for a message nobody reads.
      extraConfig = ''
        Defaults:${config.my.userName} timestamp_timeout=${toString config.my.sudoTimeout}
        Defaults lecture = never
      '';
    };

    assertions =
      let
        validUsers = users: users == [ ] || users == [ "root" ];
        validGroups = groups: groups == [ ] || groups == [ "wheel" ];
        validUserGroups = builtins.all (
          r: validUsers (r.users or [ ]) && validGroups (r.groups or [ ])
        ) config.security.sudo.extraRules;
      in
      [
        {
          assertion = config.security.sudo.execWheelOnly -> validUserGroups;
          message = ''
            security.sudo.extraRules grants sudo to a user other than 'root' or a
            group other than 'wheel'; execWheelOnly would leave that account
            unable to exec the binary. Adjust the rule, or set
            security.sudo.execWheelOnly = false.
          '';
        }
      ];
  };
}
