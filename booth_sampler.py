# -*- coding: utf-8 -*-
"""
Created on Wed Mar 11 09:50:38 2026

@author: akchatte
"""

# -*- coding: utf-8 -*-
"""
Electoral Roll Booth Sampler
============================
Picks 3% of PDF files (rounded up) from each eroll folder in a given range,
copies them into selected_booths/eroll-N/ folders, and saves a summary JSON.

USAGE:
    python booth_sampler.py

Edit EROLL_START, EROLL_END, BASE_FOLDER at the top to change range/paths.
"""

import os
import re
import math
import json
import random
import shutil
from datetime import datetime

# ─────────────────────────────────────────────
# CONFIGURATION  ← Edit these
# ─────────────────────────────────────────────
BASE_FOLDER  = r"/Users/akankshachatterjee/downloads/ecroll_reader"
EROLL_START  = 110      # inclusive
EROLL_END    = 111     # inclusive  ← change to 21-50, 1-200 etc.
SAMPLE_PCT   = 0.05   # 3%
# ─────────────────────────────────────────────

OUTPUT_ROOT  = os.path.join(BASE_FOLDER, "selected_booths")
JSON_PATH    = os.path.join(OUTPUT_ROOT, "selected_booths.json")


def find_pdfs_in_eroll(eroll_name: str) -> list:
    """Return sorted list of all PDF paths in an eroll folder."""
    folder = os.path.join(BASE_FOLDER, eroll_name)
    if not os.path.isdir(folder):
        return []

    pdfs = [
        os.path.join(folder, f)
        for f in os.listdir(folder)
        if f.lower().endswith(".pdf")
    ]

    def booth_num(fp):
        m = re.search(r'BEN-(\d+)-WI\.pdf$', fp, re.IGNORECASE)
        return int(m.group(1)) if m else 0

    return sorted(pdfs, key=booth_num)


def main():
    print("\n" + "="*60)
    print("🗳️  ELECTORAL ROLL BOOTH SAMPLER")
    print(f"   Range  : eroll-{EROLL_START} → eroll-{EROLL_END}")
    print(f"   Sample : {int(SAMPLE_PCT*100)}% per folder (rounded up, min 1)")
    print(f"   Output : {OUTPUT_ROOT}")
    print("="*60 + "\n")

    os.makedirs(OUTPUT_ROOT, exist_ok=True)

    summary = {
        "generated_at":  datetime.now().isoformat(),
        "base_folder":   BASE_FOLDER,
        "eroll_range":   f"{EROLL_START}-{EROLL_END}",
        "sample_pct":    f"{int(SAMPLE_PCT*100)}%",
        "total_pdfs":    0,
        "total_selected": 0,
        "erolls": {}
    }

    grand_total    = 0
    grand_selected = 0

    for n in range(EROLL_START, EROLL_END + 1):
        eroll_name  = f"{n}-eroll"
        eroll_label = f"eroll-{n}"
        all_pdfs    = find_pdfs_in_eroll(eroll_name)

        if not all_pdfs:
            print(f"  ⚠️  {eroll_name} — folder not found or empty, skipping")
            continue

        # ── Sample ────────────────────────────────────────────────────────────
        n_sample = max(1, math.ceil(len(all_pdfs) * SAMPLE_PCT))
        n_sample = min(n_sample, len(all_pdfs))
        selected = sorted(random.sample(all_pdfs, n_sample),
                          key=lambda fp: int(re.search(r'BEN-(\d+)-WI', fp).group(1))
                          if re.search(r'BEN-(\d+)-WI', fp) else 0)

        # ── Copy files ────────────────────────────────────────────────────────
        dest_dir = os.path.join(OUTPUT_ROOT, eroll_label)
        os.makedirs(dest_dir, exist_ok=True)

        copied_files = []
        for src in selected:
            fname = os.path.basename(src)
            dst   = os.path.join(dest_dir, fname)
            shutil.copy2(src, dst)
            copied_files.append(fname)
            print(f"  ✅ {eroll_label}/{fname}")

        # ── Build summary entry ───────────────────────────────────────────────
        booth_nums = [
            int(re.search(r'BEN-(\d+)-WI', f).group(1))
            for f in copied_files
            if re.search(r'BEN-(\d+)-WI', f)
        ]

        summary["erolls"][eroll_label] = {
            "eroll_folder":     eroll_name,
            "source_path":      os.path.join(BASE_FOLDER, eroll_name),
            "dest_path":        dest_dir,
            "total_booths":     len(all_pdfs),
            "selected_count":   len(selected),
            "booth_numbers":    booth_nums,
            "files": [
                {
                    "filename":    os.path.basename(src),
                    "booth_no":    int(re.search(r'BEN-(\d+)-WI', src).group(1))
                                   if re.search(r'BEN-(\d+)-WI', src) else None,
                    "source_path": src,
                    "dest_path":   os.path.join(dest_dir, os.path.basename(src)),
                }
                for src in selected
            ]
        }

        grand_total    += len(all_pdfs)
        grand_selected += len(selected)

        print(f"  📁 {eroll_label}: {len(all_pdfs)} booths → "
              f"selected {len(selected)} ({booth_nums})\n")

    # ── Finalise summary ──────────────────────────────────────────────────────
    summary["total_pdfs"]     = grand_total
    summary["total_selected"] = grand_selected

    with open(JSON_PATH, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)

    print("="*60)
    print(f"✅ Done!")
    print(f"   Total PDFs scanned : {grand_total}")
    print(f"   Total selected     : {grand_selected}")
    print(f"   Copied to          : {OUTPUT_ROOT}")
    print(f"   JSON summary       : {JSON_PATH}")
    print("="*60)
    print("""
Output structure:
  selected_booths/
    eroll-1/
      2026-EROLLGEN-S25-1-SIR-...-BEN-12-WI.pdf
      2026-EROLLGEN-S25-1-SIR-...-BEN-87-WI.pdf
      ...
    eroll-2/
      ...
    selected_booths.json
""")


if __name__ == "__main__":
    main()