# OOM handling for the ThinkPad hosts.
#
# Context: this machine wedged (2026-08-18) instead of killing anything --
# earlyoom was configured but `enable = false`, systemd-oomd monitored no
# slice, and sysrq was sync-only (16), so `Alt+SysRq+f` logged
# "This sysrq operation is disabled". Nothing could break the stall.
#
# zram makes the stall *deep*: swapping to RAM produces no iowait and no
# disk queue, just endless zstd on every core. So a killer must be armed.
{ config, pkgs, ... }:

{
  services = {
    earlyoom = {
      enable = true;
      enableDebugInfo = true;

      # ~10 % of 62 G = ~6.2 G MemAvailable -> SIGTERM the biggest process.
      # freeMemKillThreshold is left unset: earlyoom derives SIGKILL as
      # half of this (msg.c: `tuple.kill = tuple.term / 2`), i.e. 5 %.
      freeMemThreshold = 10;

      # Deliberately 100, on both. earlyoom kills on an AND, not an OR
      # (main.c:502):
      #
      #   m->MemAvailablePercent <= mem_term_percent
      #     && m->SwapFreePercent <= swap_term_percent
      #
      # so the swap value is a *gate*, not a trigger -- at the old 50 nothing
      # could die until 31.5 G of swap was burned. And since our swap is
      # mostly zram (i.e. RAM), "free swap percent" measures nothing real
      # here. 100 makes the comparison always true and lets freeMemThreshold
      # govern alone. Upstream sanctions this; main.c:214 reads
      # `// Using "-s 100" is a valid way to ignore swap usage`.
      #
      # Both values are set because freeSwapKillThreshold otherwise defaults
      # to *half* of freeSwapThreshold (nixpkgs earlyoom.nix:76) -- leaving it
      # implicit would re-gate the SIGKILL escalation at 50 %.
      freeSwapThreshold = 100;
      freeSwapKillThreshold = 100;
    };
  };

  # Second line of defence, and an earlier one: earlyoom polls MemAvailable
  # and kills one process; oomd acts on PSI stall and kills a whole cgroup,
  # which catches thrash before MemAvailable bottoms out. Running both is
  # normal (Fedora ships root + user slices; system.slice is deliberately
  # left off so oomd cannot reap inside system services on a laptop).
  #
  # Caveat: enableUserSlices drops ManagedOOMMemoryPressure into *every*
  # user-owned slice, session.slice included -- a bad enough memory event can
  # end the session rather than just the offending app.
  #
  # Note this does NOT populate oomctl's "Swap Monitored CGroups": the
  # nixpkgs module only emits ManagedOOMMemoryPressure, never ManagedOOMSwap.
  # That is fine -- oomd's swap trigger fires at SwapUsedLimit=90 %, i.e.
  # ~57 G of our 63 G, far too late to be useful.
  systemd.oomd = {
    enableRootSlice = true;
    enableUserSlices = true;
  };
}
