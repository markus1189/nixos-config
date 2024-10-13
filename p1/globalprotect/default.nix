{ config, pkgs, ... }:

{
  services = {
    # globalprotect = {
    #   enable = true;
    #   csdWrapper = let
    #     hip-script = pkgs.mutate ./hipreport.sh {
    #       inherit (pkgs) bash envsubst coreutils gnused;
    #       hipreportfile = ./hip-report.xml;
    #     };
    #   in pkgs.writeShellScript "hipreport.sh" (pkgs.lib.readFile hip-script);
    # };
  };
}
