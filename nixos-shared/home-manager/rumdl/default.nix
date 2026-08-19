{ pkgs, ... }:
{
  # Markdown linter/formatter (Rust).  Replaces mdl, whose defaults flag
  # ordered lists written 1./2. and whose rule set lags the reference
  # implementation.  Also used by emacs via flymake (nixos-shared/packages/
  # emacs), where the binary is pinned into the closure instead of PATH.
  home.packages = [ pkgs.rumdl ];

  # Global user config: ~/Stuff is a note tree without per-project config
  # files, and MD013's 80-column limit is unusable for markdown tables.
  xdg.configFile."rumdl/rumdl.toml".source = ./rumdl.toml;
}
