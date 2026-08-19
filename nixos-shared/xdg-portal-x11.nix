# Portal wiring for bare X11: xdg-desktop-portal plus the Screenshot backend
# in packages/portal-x11-shim.
#
# UNIMPORTED ON PURPOSE. flameshot still honours useX11LegacyScreenshot
# (laptop/home.nix), which captures in ~0.5s with no daemon in the path; going
# through the portal measured ~0.7s and adds two processes. This exists for the
# day flameshot removes that option, as its own docs threaten. To switch over:
# import this from laptop/laptop.nix and delete the useX11LegacyScreenshot line
# from the flameshot config.
#
# checks.portal-x11-shim in flake.nix evaluates this file so it cannot rot
# while unimported.
{ pkgs, ... }:
{
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdgDesktopPortalX11Shim ];

    # The key is the .portal file's basename. The shim declares only
    # Screenshot and Access, so "default" cannot hand it anything else --
    # which is what makes its auto-approving Access implementation safe.
    config.common.default = [ "x11-shim" ];
  };
}
