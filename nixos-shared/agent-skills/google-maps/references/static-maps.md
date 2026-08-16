# Static Maps reference

## Markers

Marker spec: `color:red|label:A|Hamburg`. Multiple marker groups: separate with `;`:

```bash
./scripts/maps-api.sh static-map "Germany" 6 800x600 \
  "color:red|label:A|Hamburg;color:blue|label:B|Berlin" /tmp/map.png
```

## URL length

Google caps static-map URLs around 8 KB. Encoded polylines and large marker sets can blow past this and return a 400. If you are composing many markers programmatically, keep the final URL under that limit or switch to the Maps Static POST form.
