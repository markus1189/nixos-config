{ lib }:

let
  prices = lib.importJSON ./prices.json;
in
{
  # USD per million tokens, pi's field names. Throws instead of defaulting to
  # 0, which every harness would show as a free model.
  costOf =
    id:
    prices.${id}
      or (throw "no Requesty price for ${id}: run nixos-shared/requesty-prices/update.py ${id}");
}
