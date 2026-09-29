#!/usr/bin/env python3
"""Measure how well the elfeed-score rules rank what gets pocketed.

Label: mh/pocketed = saved to read (pos); read without it = skipped (neg).
Needs a running Emacs with elfeed loaded. See docs/elfeed-score.md.
"""
import argparse
import os
import subprocess
import tempfile

import numpy as np
import pandas as pd

# First entry carrying mh/pocketed; nothing before it is labeled.
LABELS_SINCE = "2026-05-26"


def elisp(expr):
    out = subprocess.run(["emacsclient", "--eval", expr],
                         check=True, capture_output=True, text=True).stdout
    return out.strip()


def export(since):
    fd, path = tempfile.mkstemp(prefix="claude-code.", suffix=".tsv")
    os.close(fd)
    elisp(f'(mh/elfeed-score-export "{path}" "{since}")')
    return path


def all_rules():
    # One rule per line, same spelling as the export's `rules' column.
    fd, path = tempfile.mkstemp(prefix="claude-code.", suffix=".txt")
    os.close(fd)
    elisp(f"""(with-temp-file "{path}" (insert (mapconcat #'elfeed-score-rules-pp-rule-to-string
      (append elfeed-score-serde-title-rules elfeed-score-serde-feed-rules
              elfeed-score-serde-content-rules elfeed-score-serde-title-or-content-rules
              elfeed-score-serde-authors-rules elfeed-score-serde-tag-rules
              elfeed-score-serde-link-rules elfeed-score-serde-udf-rules) "\\n")))""")
    with open(path, encoding="utf-8") as fh:
        rules = [line for line in fh.read().split("\n") if line]
    os.unlink(path)
    return rules


def auc(score, y):
    r = score.rank()
    n1 = y.sum()
    n0 = len(y) - n1
    return (r[y].sum() - n1 * (n1 + 1) / 2) / (n1 * n0)


def recall_at(score, y, frac):
    top = score.sort_values(ascending=False, kind="stable").index[: int(len(score) * frac)]
    return y[top].sum() / y.sum()


def metrics(score, y, rng):
    s = score + rng.random(len(score)) * 1e-3  # break ties at random
    return {"AUC": auc(s, y), "recall@10%": recall_at(s, y, .10),
            "recall@25%": recall_at(s, y, .25)}


def feed_weight(train, test_feeds, k=10):
    """Shrunk, clipped log2 lift per feed: the ceiling hand rules could reach."""
    base = train.pos.mean()
    g = train.groupby("feed").pos.agg(["sum", "size"])
    w = np.clip(np.round(np.log2((g["sum"] + k * base) / (g["size"] + k) / base)), -3, 4)
    return test_feeds.map(w).fillna(0)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--export", help="reuse a TSV from mh/elfeed-score-export")
    ap.add_argument("--since", default=LABELS_SINCE)
    ap.add_argument("--recent-days", type=int, default=30)
    a = ap.parse_args()

    path = a.export or export(a.since)
    d = pd.read_csv(path, sep="\t", quoting=3, keep_default_na=False)
    if not a.export:
        os.unlink(path)
    d = d[d.label != "unread"].copy()
    d["pos"] = d.label == "pos"
    y = d.pos
    rng = np.random.default_rng(0)
    pd.set_option("display.width", 200)
    pd.set_option("display.max_colwidth", 60)

    print(f"labeled {len(d)}  pocketed {y.sum()}  base rate {y.mean():.3f}  "
          f"({d.date.min()} .. {d.date.max()})\n")

    cut = (pd.Timestamp(d.date.max()) - pd.Timedelta(days=a.recent_days)).strftime("%F")
    rows = {"current rules (rescored)": metrics(d.rescored, y, rng),
            "as displayed (stored)": metrics(d.stored, y, rng),
            f"current rules, last {a.recent_days}d":
                metrics(d.rescored[d.date >= cut], y[d.date >= cut], rng)}
    tr, te = d[d.date < cut], d[d.date >= cut]
    rows[f"+ learned feed weight, last {a.recent_days}d (headroom)"] = metrics(
        te.rescored + feed_weight(tr, te.feed), te.pos, rng)
    rows["random"] = metrics(pd.Series(0.0, index=d.index), y, rng)
    print(pd.DataFrame(rows).T.round(3).to_string(), "\n")

    # Per rule: does matching it predict a pocket?
    r = d.assign(rule=d.rules.str.split(" | ", regex=False)).explode("rule")
    r = r[r.rule != ""]
    per = r.groupby("rule").pos.agg(hits="size", pocketed="sum")
    try:
        per = per.reindex(sorted(set(per.index) | set(all_rules())), fill_value=0)
    except (subprocess.CalledProcessError, FileNotFoundError):
        pass
    per["rate"] = per.pocketed / per.hits.replace(0, np.nan)
    per["lift"] = per.rate / y.mean()
    print("== rules (lift = pocket rate / base rate; 0 hits = dead)")
    print(per.sort_values(["lift", "hits"], ascending=False).round(2).to_string(), "\n")

    f = d.groupby("feed").agg(n=("pos", "size"), pocketed=("pos", "sum"),
                              mean_score=("rescored", "mean"))
    f["rate"] = f.pocketed / f.n
    print("== underscored: pocket rate >= 4x base, mean score <= 0 (n >= 10)")
    print(f[(f.rate >= 4 * y.mean()) & (f.mean_score <= 0) & (f.n >= 10)]
          .sort_values("rate", ascending=False).round(2).to_string(), "\n")
    print("== noise: never pocketed, mean score >= 0 (n >= 100)")
    print(f[(f.pocketed == 0) & (f.mean_score >= 0) & (f.n >= 100)]
          .sort_values("n", ascending=False).round(2).to_string())


if __name__ == "__main__":
    main()
