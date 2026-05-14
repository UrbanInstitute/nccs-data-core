"""
Scrape NCCS legacy CORE data dictionaries (PZ + PF, 1989-2011).

Pages: https://urbaninstitute.github.io/nccs-legacy/dictionary/core/core_archive_html/
       CORE-{YYYY}-{SUBSECTION_CLASS}-{SCOPE}.html

The set of pages to fetch is driven by the inventory TSV produced by
scripts/inventory_legacy.R, so we never scrape pages that don't correspond to
an actual S3 file.

Outputs:
  - data/raw/legacy_dictionaries/html/<id>.html  (mirrored HTML)
  - data/raw/legacy_dictionaries/parsed/<id>.csv (one CSV per dictionary)
  - data/raw/legacy_dictionaries/inventory.csv   (long-format union, joinable to crosswalks)
  - data/raw/legacy_dictionaries/index.csv       (one row per dictionary, with fetch status)

Note: many CORE pages have an H1 that says "Core YYYY PC" even when the URL
slug is -PZ. The URL slug is authoritative (confirmed per project_data_sources
memory entry); the parser does not depend on the H1 for identification.
"""
import csv
import re
import time
import urllib.request
from html import unescape
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
INVENTORY_TSV = ROOT / "data/raw/legacy_inventory/headers_by_file.tsv"
OUT_ROOT = ROOT / "data/raw/legacy_dictionaries"
HTML_DIR = OUT_ROOT / "html"
PARSED_DIR = OUT_ROOT / "parsed"
INVENTORY_PATH = OUT_ROOT / "inventory.csv"
INDEX_PATH = OUT_ROOT / "index.csv"

BASE_URL = "https://urbaninstitute.github.io/nccs-legacy/dictionary/core/core_archive_html"

# Regexes lifted from scripts/scrape_legacy_dictionaries.py — same site, same template.
FIELD_RE = re.compile(
    r'<td class="Data" valign="top">'
    r'<b>([^<]+)</b>'
    r'(?:<br>([A-Za-z]+))?'           # type
    r'(?:<br>\(([^)]+)\))?'           # length
    r'</td>'
    r'<td class="Data">'
    r'<b>([^<]*)</b>'                  # label
    r'<br>(.*?)'                       # description (lazy)
    r'(?:<table[^>]*class="Data"[^>]*>|</td>)',
    re.DOTALL,
)
SECTION_RE = re.compile(r'<th[^>]*class="PlaqueHeader"[^>]*>\s*<b[^>]*>([^<]+)</b>\s*</th>')
TITLE_RE = re.compile(r'<h1[^>]*>([^<]+)</h1>')
VARS_RE = re.compile(r'Number of variables</th><td[^>]*>\s*(\d+)\s*</td>')
RECS_RE = re.compile(r'Number of records</th><td[^>]*>\s*(\d+)\s*</td>')


def load_dictionary_ids() -> list[tuple[int, str, str]]:
    """Return distinct (tax_year, subsection_class, scope) tuples from inventory."""
    if not INVENTORY_TSV.exists():
        raise SystemExit(
            f"Missing {INVENTORY_TSV}. Run scripts/inventory_legacy.R first."
        )
    seen: set[tuple[int, str, str]] = set()
    with INVENTORY_TSV.open() as f:
        r = csv.DictReader(f, delimiter="\t")
        for row in r:
            seen.add((int(row["tax_year"]), row["subsection_class"], row["scope"]))
    return sorted(seen)


def fetch(url: str, dest: Path) -> None:
    if dest.exists() and dest.stat().st_size > 0:
        return
    req = urllib.request.Request(url, headers={"User-Agent": "nccs-data-core/1.0"})
    with urllib.request.urlopen(req, timeout=60) as resp:
        dest.write_bytes(resp.read())


def parse(html: str) -> tuple[dict, list[dict]]:
    title = TITLE_RE.search(html)
    nvars = VARS_RE.search(html)
    nrecs = RECS_RE.search(html)
    meta = {
        "title": title.group(1).strip() if title else "",
        "n_variables_declared": int(nvars.group(1)) if nvars else None,
        "n_records": int(nrecs.group(1)) if nrecs else None,
    }
    pos = 0
    current_section = ""
    fields: list[dict] = []
    while True:
        m_section = SECTION_RE.search(html, pos)
        m_field = FIELD_RE.search(html, pos)
        if not m_field:
            break
        if m_section and m_section.start() < m_field.start():
            current_section = m_section.group(1).strip()
            pos = m_section.end()
            continue
        name = m_field.group(1).strip()
        dtype = (m_field.group(2) or "").strip()
        length = (m_field.group(3) or "").strip()
        label = unescape(m_field.group(4)).strip()
        desc_html = m_field.group(5)
        desc = re.sub(r"<[^>]+>", " ", desc_html)
        desc = unescape(desc)
        desc = re.sub(r"\s+", " ", desc).strip()
        fields.append({
            "section": current_section,
            "name": name,
            "name_upper": name.upper(),
            "dtype": dtype,
            "length": length,
            "label": label,
            "description": desc,
        })
        pos = m_field.end()
    return meta, fields


def main() -> None:
    HTML_DIR.mkdir(parents=True, exist_ok=True)
    PARSED_DIR.mkdir(parents=True, exist_ok=True)

    pages = load_dictionary_ids()
    print(f"Pages to fetch: {len(pages)}")

    index_rows = []
    inventory_rows = []

    for tax_year, subclass, scope in pages:
        dict_id = f"CORE-{tax_year}-{subclass}-{scope}"
        url = f"{BASE_URL}/{dict_id}.html"
        html_path = HTML_DIR / f"{dict_id}.html"

        try:
            fetch(url, html_path)
        except Exception as e:
            print(f"  [fetch-error] {dict_id}: {e}")
            index_rows.append({
                "dictionary_id": dict_id, "tax_year": tax_year,
                "subsection_class": subclass, "scope": scope, "url": url,
                "status": f"fetch_error:{e}", "title": "",
                "n_variables_declared": "", "n_variables_parsed": 0, "n_records": "",
            })
            continue
        time.sleep(0.3)  # polite

        html = html_path.read_text(encoding="utf-8", errors="replace")
        meta, fields = parse(html)

        csv_path = PARSED_DIR / f"{dict_id}.csv"
        with csv_path.open("w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=["section", "name", "name_upper", "dtype",
                                              "length", "label", "description"])
            w.writeheader()
            w.writerows(fields)

        for fld in fields:
            inventory_rows.append({
                "tax_year": tax_year,
                "subsection_class": subclass,
                "scope": scope,
                "dictionary_id": dict_id,
                "section": fld["section"],
                "column_name": fld["name"],
                "column_name_upper": fld["name_upper"],
                "dtype": fld["dtype"],
                "length": fld["length"],
                "label": fld["label"],
                "description": fld["description"],
            })

        index_rows.append({
            "dictionary_id": dict_id,
            "tax_year": tax_year,
            "subsection_class": subclass,
            "scope": scope,
            "url": url,
            "status": "ok",
            "title": meta["title"],
            "n_variables_declared": meta["n_variables_declared"] or "",
            "n_variables_parsed": len(fields),
            "n_records": meta["n_records"] or "",
        })

        ok = (meta["n_variables_declared"] is None) or (meta["n_variables_declared"] == len(fields))
        flag = "OK      " if ok else "MISMATCH"
        print(f"  [{flag}] {dict_id}  declared={meta['n_variables_declared']} parsed={len(fields)}")

    with INDEX_PATH.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["dictionary_id", "tax_year", "subsection_class",
                                          "scope", "url", "status", "title",
                                          "n_variables_declared", "n_variables_parsed", "n_records"])
        w.writeheader()
        w.writerows(index_rows)

    with INVENTORY_PATH.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["tax_year", "subsection_class", "scope",
                                          "dictionary_id", "section",
                                          "column_name", "column_name_upper", "dtype",
                                          "length", "label", "description"])
        w.writeheader()
        w.writerows(inventory_rows)

    n_ok = sum(1 for r in index_rows if r["status"] == "ok")
    n_err = sum(1 for r in index_rows if r["status"].startswith("fetch_error"))
    print()
    print(f"Pages: {len(pages)} listed, {n_ok} fetched OK, {n_err} fetch errors")
    print(f"Total field-rows in inventory: {len(inventory_rows)}")
    print(f"Distinct column_name (case-sensitive): "
          f"{len({r['column_name'] for r in inventory_rows})}")
    print(f"Distinct column_name_upper:            "
          f"{len({r['column_name_upper'] for r in inventory_rows})}")


if __name__ == "__main__":
    main()
