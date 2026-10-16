{ lib }:

let
  prices = lib.importJSON ./prices.json;
  limits = lib.importJSON ./limits.json;
in
{
  # USD per million tokens, pi's field names. Throws instead of defaulting to
  # 0, which every harness would show as a free model.
  costOf =
    id:
    prices.${id}
      or (throw "no Requesty price for ${id}: run nixos-shared/requesty-prices/update.py ${id}");

  # Tokens. Requesty reports 0 where it does not know a limit; those are
  # dropped so the harness keeps its own fallback.
  limitOf = id: lib.filterAttrs (_: v: v > 0) (limits.${id} or { });
}
