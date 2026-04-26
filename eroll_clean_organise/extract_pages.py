# -*- coding: utf-8 -*-
"""
Electoral Roll Page Extractor
==============================
Reads selected_booths.json, extracts pages from each selected PDF:
  - Page 1  → header.jpg
  - 20 random pages (excluding first and last 2) → voter_p###.jpg

Output structure:
  extracted_pages_selected_booths/
    eroll-1/
      2026-EROLLGEN-S25-1-SIR-...-BEN-12-WI/
        header.jpg
        voter_p005.jpg
        voter_p011.jpg
        ...
    eroll-2/
      ...

SETUP:
    pip install pymupdf
"""

import os
import re
import json
import random
import fitz   # pymupdf

# ─────────────────────────────────────────────
# CONFIGURATION  ← Edit these
# ─────────────────────────────────────────────
# WHEN FIRST TIME EXTRACTION:
JSON_PATH     = r"/Users/akankshachatterjee/downloads/ecroll_reader/selected_booths/selected_booths.json"

# WHEN EXTRACTION FROM EXCESS FOLDER:
# JSON_PATH   = r"/Users/akankshachatterjee/downloads/ecroll_reader/selected_booths_excess/selected_booths_excess.json"
OUTPUT_ROOT   = r"/Users/akankshachatterjee/downloads/ecroll_reader/extracted_pages_selected_booths"

PAGES_TO_PICK = 15    # random voter pages per PDF (excluding page 1 and last 2)
DPI           = 200   # image resolution
# ─────────────────────────────────────────────

os.makedirs(OUTPUT_ROOT, exist_ok=True)


def main():
    print("\n" + "="*60)
    print("📄 ELECTORAL ROLL PAGE EXTRACTOR")
    print(f"   JSON   : {JSON_PATH}")
    print(f"   Output : {OUTPUT_ROOT}")
    print(f"   Pages  : header + up to {PAGES_TO_PICK} random voter pages")
    print("="*60)

    # ── Load selected_booths.json ─────────────────────────────────────────────
    with open(JSON_PATH, 'r', encoding='utf-8') as f:
        data = json.load(f)

    erolls = data.get("erolls", {})
    print(f"\n  Eroll folders in JSON : {len(erolls)}")

    total_pdfs   = sum(e["selected_count"] for e in erolls.values())
    total_images = 0
    print(f"  Total PDFs to process : {total_pdfs}\n")

    # ── Process each eroll ────────────────────────────────────────────────────
    for eroll_label, eroll_info in sorted(erolls.items()):
        files = eroll_info.get("files", [])
        print(f"\n📁 {eroll_label}  ({len(files)} PDFs)")

        eroll_out = os.path.join(OUTPUT_ROOT, eroll_label)
        os.makedirs(eroll_out, exist_ok=True)

        for file_entry in files:
            src_path = file_entry["source_path"]   # original PDF location
            filename = file_entry["filename"]
            folder_name = filename.replace(".pdf", "").replace(".PDF", "")

            print(f"\n  📄 {filename}")

            if not os.path.isfile(src_path):
                # Also try the dest_path (copied file)
                src_path = file_entry.get("dest_path", src_path)
                if not os.path.isfile(src_path):
                    print(f"  ⚠️  File not found at source or dest, skipping")
                    continue

            try:
                doc   = fitz.open(src_path)
                total = len(doc)
                print(f"     Total pages: {total}")

                # ── Select pages ──────────────────────────────────────────────
                # Header: always page index 0 (page 1)
                # Voter : random PAGES_TO_PICK from index 1 to total-3 (skip last 2)
                eligible = list(range(2, total - 2))   # exclude p1 and last 2

                n_pick   = min(PAGES_TO_PICK, len(eligible))
                sampled  = sorted(random.sample(eligible, n_pick))

                pages_to_extract = [0] + sampled

                # ── Create output subfolder ───────────────────────────────────
                pdf_out = os.path.join(eroll_out, folder_name)
                os.makedirs(pdf_out, exist_ok=True)

                # ── Extract and save images ───────────────────────────────────
                for page_idx in pages_to_extract:
                    page  = doc[page_idx]
                    pix   = page.get_pixmap(dpi=DPI)

                    if page_idx == 0:
                        label = "header"
                    else:
                        label = f"voter_p{page_idx + 1:03d}"

                    out_path = os.path.join(pdf_out, f"{label}.jpg")
                    pix.save(out_path)
                    print(f"     ✅ page {page_idx+1:3d} → {label}.jpg")
                    total_images += 1

                doc.close()

            except Exception as e:
                print(f"  ❌ Failed: {e}")

    print("\n" + "="*60)
    print(f"✅ Done!")
    print(f"   Images saved : {total_images}")
    print(f"   Output root  : {OUTPUT_ROOT}")
    print("="*60)


if __name__ == "__main__":
    main()