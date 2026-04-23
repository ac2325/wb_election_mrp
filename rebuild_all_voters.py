# -*- coding: utf-8 -*-
"""
Rebuild all_voters.csv
======================
Scans all booth folders in extracted_data_from_booths/,
reads each voters.csv + sibling header.txt, and builds
a clean all_voters.csv with only the required columns.

Also merges constituency name and reservation type from
west_bengal_assembly_constituencies_2026.csv using the
eroll number (eroll-6 → Number 6).

Output columns:
  serial, name, father_husband_name, relation_type,
  age, gender, eroll_folder, booth_number, page_no,
  total_individuals, constituency_name, reservation

Run:
    python rebuild_all_voters.py
"""

import os
import re
import pandas as pd

# ─────────────────────────────────────────────
# CONFIGURATION  ← Edit if needed
# ─────────────────────────────────────────────
OUTPUT_ROOT        = r"/Users/akankshachatterjee/downloads/ecroll_reader/extracted_data_from_booths"
CONSTITUENCIES_CSV = r"/Users/akankshachatterjee/downloads/ecroll_reader/west_bengal_assembly_constituencies_2026.csv"
# ─────────────────────────────────────────────

BENGALI_DIGITS = str.maketrans('০১২৩৪৫৬৭৮৯', '0123456789')
EXCLUDE_YEARS  = {2025, 2026}

KEEP_COLUMNS = [
    "serial", "name", "father_husband_name", "relation_type",
    "age", "gender", "eroll_folder", "booth_number", "page_no",
    "total_individuals", "constituency_name", "reservation",
]


def extract_total_individuals(header_txt: str) -> str:
    normalised = header_txt.translate(BENGALI_DIGITS)
    numbers    = [int(m) for m in re.findall(r'\b(\d+)\b', normalised)]
    candidates = [n for n in numbers if 0 < n < 99999 and n not in EXCLUDE_YEARS]
    return str(max(candidates)) if candidates else ""


def extract_booth_number(source_file: str) -> str:
    m = re.search(r'BEN-(\d+)-WI', str(source_file), re.IGNORECASE)
    return m.group(1) if m else ""


def main():
    print("\n" + "="*60)
    print("📊 REBUILD all_voters.csv")
    print(f"   Source : {OUTPUT_ROOT}")
    print("="*60)

    # ── Load constituencies lookup ────────────────────────────────────────────
    const_lookup = {}
    if os.path.isfile(CONSTITUENCIES_CSV):
        try:
            const_df = pd.read_csv(CONSTITUENCIES_CSV, dtype=str)
            # Normalise column names (strip whitespace)
            const_df.columns = [c.strip() for c in const_df.columns]
            for _, row in const_df.iterrows():
                num         = str(row.get("Number", "")).strip()
                name        = str(row.get("Constituency Name", "")).strip()
                reservation = str(row.get("Reserved for (SC/ST/None)", "")).strip()
                if num:
                    const_lookup[num] = (name, reservation)
            print(f"  Loaded {len(const_lookup)} constituencies from CSV\n")
        except Exception as e:
            print(f"  ⚠️  Could not load constituencies CSV: {e}\n")
    else:
        print(f"  ⚠️  Constituencies CSV not found: {CONSTITUENCIES_CSV}\n")

    all_dfs     = []
    booth_count = 0
    missing_hdr = 0

    for eroll_label in sorted(os.listdir(OUTPUT_ROOT)):
        ep = os.path.join(OUTPUT_ROOT, eroll_label)
        if not os.path.isdir(ep) or not eroll_label.startswith("eroll-"):
            continue

        # Extract eroll number → look up constituency
        eroll_num_m   = re.search(r'eroll-(\d+)', eroll_label)
        eroll_num_str = eroll_num_m.group(1) if eroll_num_m else ""
        constituency_name, reservation = const_lookup.get(eroll_num_str, ("", ""))

        for booth_folder in sorted(os.listdir(ep)):
            booth_path = os.path.join(ep, booth_folder)
            if not os.path.isdir(booth_path):
                continue

            csv_path = os.path.join(booth_path, "voters.csv")
            hdr_path = os.path.join(booth_path, "header.txt")

            if not os.path.isfile(csv_path):
                continue

            # ── Read voters.csv ───────────────────────────────────────────────
            try:
                df = pd.read_csv(csv_path, encoding="utf-8-sig", dtype=str)
            except Exception as e:
                print(f"  ⚠️  Could not read {csv_path}: {e}")
                continue

            # ── booth_number from source_file column ──────────────────────────
            if "source_file" in df.columns:
                df["booth_number"] = df["source_file"].apply(extract_booth_number)
            else:
                df["booth_number"] = extract_booth_number(booth_folder)

            # ── total_individuals from header.txt ─────────────────────────────
            total_individuals = ""
            if os.path.isfile(hdr_path):
                with open(hdr_path, "r", encoding="utf-8") as f:
                    hdr_text = f.read()
                total_individuals = extract_total_individuals(hdr_text)
            else:
                missing_hdr += 1

            df["total_individuals"] = total_individuals

            # ── Constituency info from lookup ─────────────────────────────────
            df["constituency_name"] = constituency_name
            df["reservation"]       = reservation

            # ── Keep only required columns ────────────────────────────────────
            for col in KEEP_COLUMNS:
                if col not in df.columns:
                    df[col] = ""
            df = df[KEEP_COLUMNS]

            all_dfs.append(df)
            booth_count += 1

            sample_booth = df["booth_number"].iloc[0] if len(df) else "?"
            print(f"  ✅ {eroll_label}/{booth_folder} — "
                  f"{len(df)} voters | booth={sample_booth} | "
                  f"total={total_individuals or '?'} | "
                  f"constituency={constituency_name or '?'} ({reservation or '?'})")

    if not all_dfs:
        print("\n⚠️  No voters.csv files found.")
        return

    combined = pd.concat(all_dfs, ignore_index=True)
    out_path  = os.path.join(OUTPUT_ROOT, "all_voters.csv")
    combined.to_csv(out_path, index=False, encoding="utf-8-sig")

    print("\n" + "="*60)
    print(f"✅ Done!")
    print(f"   Booths processed   : {booth_count}")
    print(f"   Missing header.txt : {missing_hdr}")
    print(f"   Total voters       : {len(combined)}")
    print(f"   Output             : {out_path}")
    print("="*60)


if __name__ == "__main__":
    main()