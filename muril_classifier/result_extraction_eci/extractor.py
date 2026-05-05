"""
Scrape West Bengal 2026 Assembly Election results from the ECI site.

Output: wb_2026_results.csv
  - one row per constituency
  - three columns per party found anywhere across the state:
      "<party>_candidate", "<party>_total_votes", "<party>_vote_share"
  - new parties seen in any constituency add new columns automatically

Usage:
    pip install selenium beautifulsoup4
    # Make sure the chromedriver matching your Chrome version is on PATH.
    python scrape_wb_2026.py
"""

import csv
import json
import time
from pathlib import Path

from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select, WebDriverWait

# ---------------------------------------------------------------------------
# CONFIG  -- point BASE_URL at the directory that hosts ConstituencywiseS*.htm
# ---------------------------------------------------------------------------
BASE_URL    = "https://results.eci.gov.in/AcResultGenMay2026/"
INDEX_URL   = BASE_URL + "index.htm"        # page that has the dropdown
OUTPUT_CSV  = "wb_2026_results.csv"
CHECKPOINT  = "wb_2026_checkpoint.json"     # resumable progress
DELAY_S     = 0.4                            # politeness delay between pages
HEADLESS    = True

DROPDOWN_ID = "ctl00_ContentPlaceHolder1_Result1_ddlState"


# ---------------------------------------------------------------------------
# Selenium plumbing
# ---------------------------------------------------------------------------
def make_driver():
    opts = Options()
    if HEADLESS:
        opts.add_argument("--headless=new")
    opts.add_argument("--disable-gpu")
    opts.add_argument("--no-sandbox")
    opts.add_argument("--window-size=1400,1000")
    # less console spam
    opts.add_experimental_option("excludeSwitches", ["enable-logging"])
    return webdriver.Chrome(options=opts)


# ---------------------------------------------------------------------------
# Step 1 -- read every constituency code from the dropdown on the index page
# ---------------------------------------------------------------------------
def get_constituencies(driver):
    driver.get(INDEX_URL)
    WebDriverWait(driver, 20).until(
        EC.presence_of_element_located((By.ID, DROPDOWN_ID))
    )
    sel = Select(driver.find_element(By.ID, DROPDOWN_ID))
    out = []
    for opt in sel.options:
        val = (opt.get_attribute("value") or "").strip()
        txt = opt.text.strip()
        if val:                                       # skip "Select Constituency"
            out.append((val, txt))
    return out


# ---------------------------------------------------------------------------
# Step 2 -- per constituency, hit the table view and parse the rows
# ---------------------------------------------------------------------------
def scrape_constituency(driver, code):
    """
    Fetch ConstituencywiseS<code>.htm. The dropdown values already start with
    'S', e.g. 'S2512' -> ConstituencywiseS2512.htm.
    """
    url = f"{BASE_URL}Constituencywise{code}.htm"
    driver.get(url)
    WebDriverWait(driver, 20).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, "table.table tbody tr"))
    )
    soup = BeautifulSoup(driver.page_source, "html.parser")
    table = soup.select_one("table.table")
    rows = []
    for tr in table.select("tbody tr"):
        tds = tr.find_all("td")
        if len(tds) < 7:
            continue
        rows.append({
            "candidate":   tds[1].get_text(strip=True),
            "party":       tds[2].get_text(strip=True),
            "total_votes": tds[5].get_text(strip=True),
            "vote_share":  tds[6].get_text(strip=True),
        })
    return rows


def collapse_to_party(rows):
    """
    Some constituencies have multiple candidates under the same party label
    (Independents especially). Keep the highest-voted one per party so the
    flat 'party -> 3 columns' mapping stays unambiguous.
    """
    best = {}
    for r in rows:
        try:
            v = int(r["total_votes"].replace(",", ""))
        except ValueError:
            v = 0
        cur = best.get(r["party"])
        if cur is None or v > cur["_v"]:
            best[r["party"]] = {**r, "_v": v}
    for d in best.values():
        d.pop("_v", None)
    return best


# ---------------------------------------------------------------------------
# Step 3 -- write the wide CSV
# ---------------------------------------------------------------------------
def write_csv(all_data, all_parties, path):
    parties_sorted = sorted(all_parties)
    fieldnames = ["constituency"]
    for p in parties_sorted:
        fieldnames += [f"{p}_candidate", f"{p}_total_votes", f"{p}_vote_share"]

    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        for cons_name, party_map in all_data.items():
            row = {"constituency": cons_name}
            for p in parties_sorted:
                if p in party_map:
                    row[f"{p}_candidate"]   = party_map[p]["candidate"]
                    row[f"{p}_total_votes"] = party_map[p]["total_votes"]
                    row[f"{p}_vote_share"]  = party_map[p]["vote_share"]
            w.writerow(row)


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------
def main():
    # resume from checkpoint if present
    all_data = {}
    if Path(CHECKPOINT).exists():
        all_data = json.loads(Path(CHECKPOINT).read_text(encoding="utf-8"))
        print(f"Resuming with {len(all_data)} constituencies already scraped.")

    driver = make_driver()
    try:
        constituencies = get_constituencies(driver)
        print(f"Discovered {len(constituencies)} constituencies in the dropdown.")

        for i, (code, name) in enumerate(constituencies, 1):
            if name in all_data and all_data[name]:    # already done
                continue
            print(f"[{i:3d}/{len(constituencies)}] {name} ({code})")
            try:
                rows = scrape_constituency(driver, code)
                all_data[name] = collapse_to_party(rows)
            except Exception as e:
                print(f"   ! failed: {e}")
                all_data[name] = {}
            # checkpoint every page
            Path(CHECKPOINT).write_text(json.dumps(all_data, ensure_ascii=False))
            time.sleep(DELAY_S)
    finally:
        driver.quit()

    # union of every party seen in any constituency
    all_parties = {p for d in all_data.values() for p in d}
    write_csv(all_data, all_parties, OUTPUT_CSV)
    print(f"\nWrote {len(all_data)} rows x {len(all_parties)} parties -> {OUTPUT_CSV}")


if __name__ == "__main__":
    main()