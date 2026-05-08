"""
Mirror IRS SOI Tax-Exempt Organization Financial Data extracts + dictionaries
to s3://nccsdata/raw/soi/.

Source: https://www.irs.gov/statistics/soi-tax-stats-annual-extract-of-tax-exempt-organization-financial-data

Layout produced:
  s3://{bucket}/raw/soi/extracts/{YYYY}/{original_irs_filename}
  s3://{bucket}/raw/soi/dictionaries/{YYYY}/{original_irs_filename}
  s3://{bucket}/raw/soi/manifest.csv
  data/raw/soi/manifest.csv   (local mirror, gitignored)

Idempotency:
  Compares HEAD Content-Length + Last-Modified against the manifest. If both
  match, the file is skipped. Otherwise the file is downloaded, sha256'd, and
  uploaded; the manifest row is updated.

Usage:
  python scripts/mirror_irs_soi.py [--dry-run] [--year YYYY] [--bucket nccsdata]
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import re
import sys
import urllib.request
from datetime import datetime, timezone
from html import unescape
from pathlib import Path
from urllib.parse import urljoin

# boto3 is imported lazily inside main() so --dry-run works without it installed.

IRS_PAGE = "https://www.irs.gov/statistics/soi-tax-stats-annual-extract-of-tax-exempt-organization-financial-data"
IRS_BASE = "https://www.irs.gov"
DEFAULT_BUCKET = "nccsdata"
S3_PREFIX = "raw/soi"

ROOT = Path(__file__).resolve().parent.parent
LOCAL_MANIFEST = ROOT / "data/raw/soi/manifest.csv"

MANIFEST_FIELDS = [
    "year", "form", "kind", "source_url", "filename",
    "s3_key", "sha256", "bytes", "http_last_modified", "fetched_at",
]

# Filename patterns observed on the IRS page. Both old ("eofinextract") and
# new ("eoextract") prefixes appear; EZ/PF casing varies year-to-year.
EXTRACT_RE = re.compile(
    r"/pub/irs-soi/(?P<yy>\d{2})eo(?:fin)?extract"
    r"(?P<form>990(?:ez|pf)?|ez|pf)\.zip",
    re.IGNORECASE,
)
DOC_RE = re.compile(
    r"/pub/irs-soi/(?P<yy>\d{2})eofinextractdoc\.(?:xlsx|xls)",
    re.IGNORECASE,
)


def yy_to_yyyy(yy: str) -> int:
    n = int(yy)
    # IRS publishes 2012+, all values map into 20YY for the foreseeable future.
    return 2000 + n


def normalize_form(token: str) -> str:
    t = token.lower()
    if t in ("ez", "990ez"):
        return "990EZ"
    if t in ("pf", "990pf"):
        return "990PF"
    if t == "990":
        return "990"
    raise ValueError(f"unrecognized form token: {token}")


def fetch_page(url: str) -> str:
    req = urllib.request.Request(url, headers={"User-Agent": "nccs-data-core/mirror_irs_soi"})
    with urllib.request.urlopen(req, timeout=60) as resp:
        return resp.read().decode("utf-8", errors="replace")


def extract_links(html: str) -> list[dict]:
    """Return a list of {year, form, kind, source_url, filename} dicts."""
    items: dict[tuple, dict] = {}

    for m in EXTRACT_RE.finditer(html):
        path = m.group(0)
        year = yy_to_yyyy(m.group("yy"))
        form = normalize_form(m.group("form"))
        url = urljoin(IRS_BASE, path)
        key = (year, form, "extract")
        items[key] = {
            "year": year, "form": form, "kind": "extract",
            "source_url": url, "filename": path.rsplit("/", 1)[-1],
        }

    for m in DOC_RE.finditer(html):
        path = m.group(0)
        year = yy_to_yyyy(m.group("yy"))
        url = urljoin(IRS_BASE, path)
        # One doc per year covers all forms.
        key = (year, "ALL", "dictionary")
        items[key] = {
            "year": year, "form": "ALL", "kind": "dictionary",
            "source_url": url, "filename": path.rsplit("/", 1)[-1],
        }

    return sorted(items.values(), key=lambda r: (r["year"], r["form"], r["kind"]))


def s3_key_for(item: dict) -> str:
    sub = "extracts" if item["kind"] == "extract" else "dictionaries"
    return f"{S3_PREFIX}/{sub}/{item['year']}/{item['filename']}"


def head_source(url: str) -> tuple[int | None, str | None]:
    """Return (content_length, last_modified) for the IRS-hosted file."""
    req = urllib.request.Request(
        url, method="HEAD",
        headers={"User-Agent": "nccs-data-core/mirror_irs_soi"},
    )
    with urllib.request.urlopen(req, timeout=60) as resp:
        cl = resp.headers.get("Content-Length")
        lm = resp.headers.get("Last-Modified")
        return (int(cl) if cl else None, lm)


def load_manifest(path: Path) -> dict[tuple, dict]:
    if not path.exists():
        return {}
    out = {}
    with path.open(newline="") as f:
        for row in csv.DictReader(f):
            out[(int(row["year"]), row["form"], row["kind"])] = row
    return out


def write_manifest(path: Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    rows_sorted = sorted(rows, key=lambda r: (int(r["year"]), r["form"], r["kind"]))
    with path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=MANIFEST_FIELDS)
        w.writeheader()
        for r in rows_sorted:
            w.writerow({k: r.get(k, "") for k in MANIFEST_FIELDS})


def download_to_memory(url: str) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": "nccs-data-core/mirror_irs_soi"})
    with urllib.request.urlopen(req, timeout=600) as resp:
        return resp.read()


def upload(s3, bucket: str, key: str, body: bytes) -> None:
    s3.put_object(Bucket=bucket, Key=key, Body=body)


def _client_error():
    from botocore.exceptions import ClientError  # noqa: PLC0415
    return ClientError


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--bucket", default=DEFAULT_BUCKET)
    ap.add_argument("--year", type=int, action="append",
                    help="Restrict to specific year(s); repeatable.")
    ap.add_argument("--dry-run", action="store_true",
                    help="Plan only — print actions, do not download or upload.")
    ap.add_argument("--force", action="store_true",
                    help="Re-download even if size+last-modified match the manifest.")
    args = ap.parse_args()

    print(f"[1/5] Fetching IRS page: {IRS_PAGE}")
    html = fetch_page(IRS_PAGE)

    print("[2/5] Parsing download links")
    items = extract_links(html)
    if args.year:
        items = [i for i in items if i["year"] in set(args.year)]
    print(f"      Found {len(items)} artifacts")
    by_year: dict[int, list[str]] = {}
    for i in items:
        by_year.setdefault(i["year"], []).append(f"{i['form']}/{i['kind']}")
    for y in sorted(by_year):
        print(f"        {y}: {', '.join(sorted(by_year[y]))}")

    print(f"[3/5] Loading existing manifest from {LOCAL_MANIFEST}")
    manifest = load_manifest(LOCAL_MANIFEST)
    print(f"      {len(manifest)} prior entries")

    if args.dry_run:
        s3 = None
    else:
        import boto3  # noqa: PLC0415
        s3 = boto3.client("s3")

    print("[4/5] Reconciling against IRS HEAD metadata")
    actions: list[tuple[str, dict]] = []
    for item in items:
        key = (item["year"], item["form"], item["kind"])
        prior = manifest.get(key)
        try:
            cl, lm = head_source(item["source_url"])
        except Exception as e:
            print(f"      ! HEAD failed for {item['source_url']}: {e}")
            actions.append(("skip-head-error", item))
            continue

        unchanged = (
            prior is not None
            and not args.force
            and prior.get("bytes") == str(cl)
            and prior.get("http_last_modified") == (lm or "")
        )
        if unchanged:
            actions.append(("skip-unchanged", item))
            continue

        item["_content_length"] = cl
        item["_last_modified"] = lm or ""
        actions.append(("download", item))

    n_download = sum(1 for a, _ in actions if a == "download")
    n_skip = sum(1 for a, _ in actions if a == "skip-unchanged")
    n_err = sum(1 for a, _ in actions if a == "skip-head-error")
    print(f"      download={n_download}  skip-unchanged={n_skip}  head-error={n_err}")

    if args.dry_run:
        print("[5/5] DRY-RUN — exiting without download/upload")
        for action, item in actions:
            if action == "download":
                print(f"      WOULD download+upload {item['source_url']} -> s3://{args.bucket}/{s3_key_for(item)}")
        return 0

    print(f"[5/5] Downloading and uploading to s3://{args.bucket}/")
    new_rows: list[dict] = []
    for action, item in actions:
        key = (item["year"], item["form"], item["kind"])
        if action != "download":
            if manifest.get(key):
                new_rows.append(manifest[key])
            continue

        print(f"      -> {item['filename']} ({item['year']} {item['form']} {item['kind']})")
        try:
            body = download_to_memory(item["source_url"])
        except Exception as e:
            print(f"         ! download failed: {e}")
            if manifest.get(key):
                new_rows.append(manifest[key])
            continue

        sha = hashlib.sha256(body).hexdigest()
        s3_key = s3_key_for(item)
        try:
            upload(s3, args.bucket, s3_key, body)
        except _client_error() as e:
            print(f"         ! upload failed: {e}")
            if manifest.get(key):
                new_rows.append(manifest[key])
            continue

        new_rows.append({
            "year": item["year"],
            "form": item["form"],
            "kind": item["kind"],
            "source_url": item["source_url"],
            "filename": item["filename"],
            "s3_key": s3_key,
            "sha256": sha,
            "bytes": str(len(body)),
            "http_last_modified": item.get("_last_modified", ""),
            "fetched_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        })

    # Carry over any prior rows for items not seen this run (e.g., year filter).
    seen_keys = {(int(r["year"]), r["form"], r["kind"]) for r in new_rows}
    for k, row in manifest.items():
        if k not in seen_keys:
            new_rows.append(row)

    write_manifest(LOCAL_MANIFEST, new_rows)
    print(f"      Wrote local manifest: {LOCAL_MANIFEST}")

    # Upload manifest to S3 alongside the data.
    with LOCAL_MANIFEST.open("rb") as f:
        upload(s3, args.bucket, f"{S3_PREFIX}/manifest.csv", f.read())
    print(f"      Wrote s3://{args.bucket}/{S3_PREFIX}/manifest.csv")

    return 0


if __name__ == "__main__":
    sys.exit(main())
