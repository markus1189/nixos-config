{ config, lib, pkgs, ... }:

with lib;

let
  usersWithKeys = attrValues (filterAttrs (n: u: u.symlinks != {}) config.users.users);

  activationScriptForUser = user: flip mapAttrsToList user.symlinks (reltarget: src:
    ''update_symlink "${src}" "${user.home}/${reltarget}"''
  );

  activationScript = ''
    ${builtins.readFile ./update-symlinks.sh}
    ${concatStringsSep "\n" (flatten (map activationScriptForUser usersWithKeys))}
  '';
in
{
  ###### interface
  options = {
    users.users = mkOption {
      type = with types; loaOf (submodule {
        options.symlinks = mkOption {
          default = {};
          description = ''
            An attrset of relative paths and targets to symlink in to
            the user's HOME.
          '';
        };
      });
    };
  };

  ###### implementation
  config = ({
    system.activationScripts.usersymlinks = activationScript;
  });
}
