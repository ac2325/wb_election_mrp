# -*- coding: utf-8 -*-
"""
Bengali Electoral Roll — Google Lens Scraper (Selected Booths)
==============================================================
Reads from: extracted_pages_selected_booths/
  eroll-1/
    2026-EROLLGEN-S25-1-SIR-.../
      header.jpg
      voter_p005.jpg
      ...

Saves to: extracted_data_from_booths/
  eroll-1/
    2026-EROLLGEN-S25-1-SIR-.../
      header.txt              ← Bengali text
      voter_p005.txt          ← English translation of that page
      ...
      voters.csv              ← all voters from this booth combined

  all_voters.csv              ← everything combined

SETUP:
    pip install selenium webdriver-manager pandas pyperclip

Run:
    python lens_scraper_booths.py
"""

import os
import re
import time
import json
import base64
import pyperclip
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager

# ─────────────────────────────────────────────
# CONFIGURATION  ← Edit these
# ─────────────────────────────────────────────
EXTRACTED_ROOT  = r"/Users/akankshachatterjee/downloads/ecroll_reader/extracted_pages_selected_booths"
OUTPUT_ROOT     = r"/Users/akankshachatterjee/downloads/ecroll_reader/extracted_data_from_booths"
CHECKPOINT_FILE = r"/Users/akankshachatterjee/downloads/ecroll_reader/booths_checkpoint.json"
WAIT_TIMEOUT    = 40
DELAY_BETWEEN   = 4
# ─────────────────────────────────────────────

os.makedirs(OUTPUT_ROOT, exist_ok=True)

COLUMN_ORDER = [
    "serial", "name", "father_husband_name", "relation_type",
    "house_no", "age", "gender", "voter_id",
    "constituency_no", "constituency_name", "part_no",
    "bhag_no", "bhag_name", "polling_station_no", "polling_station_name",
    "thana", "gram_panchayat", "block", "sub_division", "district", "pin",
    "eroll_folder", "source_file", "page_no",
]

# Language name variants confirmed from both DE and EN UIs
BENGALI_NAMES = {'Bangla', 'Bengali', 'Bengalisch', 'Bengalî', 'বাংলা'}
ENGLISH_NAMES = {'English', 'Englisch', 'Anglais'}

# =============================================================================
# CHECKPOINT
# =============================================================================

def load_checkpoint() -> set:
    if os.path.exists(CHECKPOINT_FILE):
        try:
            with open(CHECKPOINT_FILE, 'r', encoding='utf-8') as f:
                return set(json.load(f).get("done", []))
        except Exception:
            pass
    return set()

def save_checkpoint(done: set):
    with open(CHECKPOINT_FILE, 'w', encoding='utf-8') as f:
        json.dump({"done": list(done),
                   "last_run": time.strftime("%Y-%m-%d %H:%M:%S")},
                  f, indent=2, ensure_ascii=False)

# =============================================================================
# SELENIUM DRIVER
# =============================================================================

def make_driver() -> webdriver.Chrome:
    options = webdriver.ChromeOptions()
    options.add_argument("--start-maximized")
    options.add_argument("--disable-blink-features=AutomationControlled")
    options.add_experimental_option("excludeSwitches", ["enable-automation"])
    options.add_experimental_option("useAutomationExtension", False)
    options.add_experimental_option("prefs", {
        "profile.content_settings.exceptions.clipboard": {
            "[*.]google.com,*": {"last_modified": "0", "setting": 1}
        }
    })
    driver = webdriver.Chrome(
        service=Service(ChromeDriverManager().install()),
        options=options
    )
    driver.execute_script(
        "Object.defineProperty(navigator, 'webdriver', {get: () => undefined})"
    )
    return driver

# =============================================================================
# IMAGE UPLOAD
# Strategy: inject the file as a real File object via DataTransfer directly
# onto the upload controller (jscontroller="QrpsMc"), then fire all the events
# Google's handler listens to. This bypasses the OS dialog entirely and
# reliably triggers navigation to the results page.
# =============================================================================

def upload_image(driver: webdriver.Chrome, img_path: str) -> bool:
    """
    Injects the image file directly into Google's upload controller using
    DataTransfer, then fires the exact event sequence Google's jsaction
    handler expects: 'change' on the input, which triggers the upload
    and navigates to the results page.

    No OS file dialog is opened at any point.
    """
    wait = WebDriverWait(driver, WAIT_TIMEOUT)
    name = os.path.basename(img_path)
    ext  = os.path.splitext(img_path)[1].lower().lstrip(".")
    mime = {"jpg": "image/jpeg", "jpeg": "image/jpeg",
            "png": "image/png",  "webp": "image/webp",
            "bmp": "image/bmp"}.get(ext, "image/jpeg")

    # Read and encode the file in Python — avoids JS string size limits
    with open(img_path, "rb") as f:
        b64 = base64.b64encode(f.read()).decode()

    # Wait for upload page to be ready
    try:
        wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, "[jsname='tAPGc']")))
    except Exception as e:
        print(f"    ❌ upload page not ready: {e}")
        return False

    # Inject the file via DataTransfer and fire Google's upload event chain.
    # jscontroller="QrpsMc" is Google's upload controller (confirmed in HTML).
    # Its jsaction listens for: "rcuQ6b:npT2md" (change) and "AMruCe:Zpug7c"
    # We fire 'change' on the hidden input which is what the real file picker
    # would do after the user selects a file.
    result = driver.execute_script("""
        var b64  = arguments[0];
        var mime = arguments[1];
        var name = arguments[2];

        // Build File object from base64
        var byteChars = atob(b64);
        var byteNums  = new Array(byteChars.length);
        for (var i = 0; i < byteChars.length; i++) {
            byteNums[i] = byteChars.charCodeAt(i);
        }
        var byteArr = new Uint8Array(byteNums);
        var blob    = new Blob([byteArr], {type: mime});
        var file    = new File([blob], name, {type: mime, lastModified: Date.now()});

        var dt = new DataTransfer();
        dt.items.add(file);

        // Find the upload controller by jscontroller (confirmed in HTML)
        var controller = document.querySelector('[jscontroller="QrpsMc"]');

        // Strategy 1: set files on every input[type=file] and fire change
        var inputs = document.querySelectorAll('input[type=file]');
        if (inputs.length > 0) {
            for (var i = 0; i < inputs.length; i++) {
                try {
                    // Suppress the OS dialog just in case
                    inputs[i].click = function() {};
                    inputs[i].files = dt.files;
                    // Fire the full event sequence
                    inputs[i].dispatchEvent(new Event('input',  {bubbles: true}));
                    inputs[i].dispatchEvent(new Event('change', {bubbles: true}));
                } catch(e) {}
            }
            return 'input_change:' + inputs.length;
        }

        // Strategy 2: drop on the upload controller div
        if (controller) {
            controller.dispatchEvent(new DragEvent('dragenter', {bubbles:true, cancelable:true, dataTransfer:dt}));
            controller.dispatchEvent(new DragEvent('dragover',  {bubbles:true, cancelable:true, dataTransfer:dt}));
            controller.dispatchEvent(new DragEvent('drop',      {bubbles:true, cancelable:true, dataTransfer:dt}));
            return 'drop:controller';
        }

        // Strategy 3: drop on the dropzone (jsname="QdEQIc")
        var zone = document.querySelector('[jsname="QdEQIc"]');
        if (zone) {
            zone.dispatchEvent(new DragEvent('dragenter', {bubbles:true, cancelable:true, dataTransfer:dt}));
            zone.dispatchEvent(new DragEvent('dragover',  {bubbles:true, cancelable:true, dataTransfer:dt}));
            zone.dispatchEvent(new DragEvent('drop',      {bubbles:true, cancelable:true, dataTransfer:dt}));
            return 'drop:zone';
        }

        return 'no_target';
    """, b64, mime, name)

    print(f"    📤 {name} [JS:{result}]", end="", flush=True)

    if result and result != 'no_target':
        return True

    # Last resort: click tAPGc, wait for input, unhide + send_keys
    # The send_keys path is unreliable for triggering navigation but worth trying
    print(f" [send_keys fallback]", end="", flush=True)
    try:
        upload_span = driver.find_element(By.CSS_SELECTOR, "[jsname='tAPGc']")

        # Plant a MutationObserver to intercept any dynamically injected input
        driver.execute_script("""
            window.__obs = new MutationObserver(function(muts) {
                muts.forEach(function(m) {
                    m.addedNodes.forEach(function(n) {
                        var inputs = [];
                        if (n.type === 'file') inputs.push(n);
                        else if (n.querySelectorAll) {
                            n.querySelectorAll('input[type=file]').forEach(function(i){ inputs.push(i); });
                        }
                        inputs.forEach(function(inp) {
                            inp.click = function(){};
                            inp.style.cssText = 'display:block!important;visibility:visible!important;'
                                + 'opacity:1!important;position:fixed;top:0;left:0;width:1px;height:1px;z-index:9999;';
                        });
                    });
                });
            });
            window.__obs.observe(document.body, {childList:true, subtree:true});
            // Also suppress existing inputs
            document.querySelectorAll('input[type=file]').forEach(function(inp){
                inp.click = function(){};
                inp.style.cssText = 'display:block!important;visibility:visible!important;'
                    + 'opacity:1!important;position:fixed;top:0;left:0;width:1px;height:1px;z-index:9999;';
            });
        """)

        upload_span.click()
        time.sleep(1)

        driver.execute_script("if(window.__obs){window.__obs.disconnect();}")

        # Unhide + send_keys + fire events
        driver.execute_script("""
            document.querySelectorAll('input[type=file]').forEach(function(inp){
                inp.click = function(){};
                inp.style.cssText = 'display:block!important;visibility:visible!important;'
                    + 'opacity:1!important;position:fixed;top:0;left:0;width:1px;height:1px;z-index:9999;';
            });
        """)
        time.sleep(0.3)

        for inp in driver.find_elements(By.CSS_SELECTOR, "input[type='file']"):
            try:
                inp.send_keys(img_path)
                driver.execute_script("""
                    var inp = arguments[0];
                    inp.dispatchEvent(new Event('input',  {bubbles:true}));
                    inp.dispatchEvent(new Event('change', {bubbles:true}));
                """, inp)
                print(f" [send_keys+events✓]", end="", flush=True)
                return True
            except Exception:
                pass
    except Exception as e:
        print(f" ❌ fallback failed: {e}")

    print(f" ❌ all strategies failed for {name}")
    return False

# =============================================================================
# WAIT FOR RESULTS PAGE
# After upload we must confirm Lens has actually navigated away from the
# upload page to the results page before looking for tabs/buttons.
# =============================================================================

def wait_for_results(driver: webdriver.Chrome) -> bool:
    """
    Wait until Lens has navigated to the results page.
    Signs of the results page:
      - URL changes away from /upload
      - The translate button (jsname="TtaS0d") appears
      - OR at least one [role='tab'] appears
    Returns True if results page detected, False on timeout.
    """
    print(" [waiting for results]", end="", flush=True)
    for _ in range(30):
        time.sleep(1)
        try:
            url = driver.current_url
            # Results page URL contains /search or query params
            if '/upload' not in url and 'lens.google.com' in url:
                print(f" [url:ok]", end="", flush=True)
                time.sleep(2)  # let the page settle
                return True
        except Exception:
            pass
        try:
            btn = driver.find_element(By.CSS_SELECTOR, "button[jsname='TtaS0d']")
            if btn.is_displayed():
                print(f" [btn:ok]", end="", flush=True)
                return True
        except Exception:
            pass
        try:
            tabs = driver.find_elements(By.CSS_SELECTOR, "[role='tab']")
            if len(tabs) >= 2:
                print(f" [tabs:{len(tabs)}]", end="", flush=True)
                return True
        except Exception:
            pass

    print(f" [⚠️ results page timeout]", end="", flush=True)
    return False


# =============================================================================
# TRANSLATE BUTTON
# jsname="TtaS0d" — identical in both DE and EN UIs
# =============================================================================

def click_translate_button(driver: webdriver.Chrome) -> bool:
    wait = WebDriverWait(driver, 15)

    try:
        btn = wait.until(
            EC.element_to_be_clickable((By.CSS_SELECTOR, "button[jsname='TtaS0d']"))
        )
        label = btn.text.strip()
        btn.click()
        print(f" ['{label}'✓]", end="", flush=True)
        time.sleep(3)
        return True
    except Exception:
        pass

    for aria in ['Translate image', 'Bild übersetzen']:
        try:
            btn = driver.find_element(By.CSS_SELECTOR, f"button[aria-label='{aria}']")
            btn.click()
            print(f" [aria '{aria}'✓]", end="", flush=True)
            time.sleep(3)
            return True
        except Exception:
            pass

    try:
        span = driver.find_element(By.CSS_SELECTOR, "span[jsname='neGBhe']")
        btn  = span.find_element(By.XPATH, "./ancestor::button")
        btn.click()
        print(f" [neGBhe '{span.text}'✓]", end="", flush=True)
        time.sleep(3)
        return True
    except Exception:
        pass

    # Last resort: tab index fallback
    try:
        tabs = driver.find_elements(By.CSS_SELECTOR, "[role='tab']")
        print(f" [tab fallback, {len(tabs)} tabs]", end="", flush=True)
        idx = 2 if len(tabs) >= 3 else (1 if len(tabs) == 2 else -1)
        if idx >= 0:
            tabs[idx].click()
            time.sleep(3)
            return True
    except Exception:
        pass

    print(" [⚠️ translate btn not found]", end="", flush=True)
    return False


# =============================================================================
# LANGUAGE SELECTOR
# =============================================================================

def set_target_language(driver: webdriver.Chrome, language: str):
    target_names = BENGALI_NAMES if language == "Bengali" else ENGLISH_NAMES
    search_term  = "Bangla" if language == "Bengali" else "English"

    lang_btn = None
    for css in ["div.niO4u.VDgVie.SlP8xc", "div.niO4u.VDgVie", "div.niO4u"]:
        try:
            btn = driver.find_element(By.CSS_SELECTOR, css)
            if btn.is_displayed():
                lang_btn = btn
                break
        except Exception:
            pass

    if not lang_btn:
        print(f" [⚠️ lang btn not found]", end="", flush=True)
        return

    try:
        current = lang_btn.find_element(By.CSS_SELECTOR, "span[jsname='rUg5qe']").text.strip()
    except Exception:
        current = lang_btn.text.strip()
    print(f" [lang={current}]", end="", flush=True)

    if any(v in current for v in target_names):
        print(f" [already {language}]", end="", flush=True)
        return

    lang_btn.click()
    time.sleep(2)

    for css in ["input[type='search']", "input[type='text']", "input[placeholder]"]:
        try:
            box = WebDriverWait(driver, 5).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, css))
            )
            box.clear()
            box.send_keys(search_term)
            time.sleep(1.5)
            break
        except Exception:
            pass

    candidates = (
        ['Bangla', 'Bengalisch', 'Bengali', 'Bengalî', 'বাংলা']
        if language == "Bengali"
        else ['English', 'Englisch', 'Anglais']
    )
    for name in candidates:
        for xpath in [f"//*[text()='{name}']", f"//*[contains(text(),'{name}')]"]:
            try:
                opt = driver.find_element(By.XPATH, xpath)
                if opt.is_displayed():
                    opt.click()
                    print(f" [→{language} '{name}'✓]", end="", flush=True)
                    time.sleep(2)
                    return
            except Exception:
                pass

    print(f" [⚠️ could not select {language}]", end="", flush=True)


# =============================================================================
# COPY TEXT BUTTON
# =============================================================================

def click_copy_button(driver: webdriver.Chrome) -> bool:
    pyperclip.copy("")
    time.sleep(0.3)

    for css in [
        "div[jscontroller='icx32b']",
        "div.niO4u.VDgVie.SlP8xc",
        "div.niO4u.VDgVie",
        "div.niO4u",
    ]:
        try:
            btns = driver.find_elements(By.CSS_SELECTOR, css)
            for btn in btns:
                if btn.is_displayed():
                    btn.click()
                    print(" [copy✓]", end="", flush=True)
                    return True
        except Exception:
            pass

    for label in ['Text kopieren', 'Copy text', 'Copier le texte', 'টেক্সট কপি']:
        for xpath in [
            f"//*[@role='button'][contains(.,'{label}')]",
            f"//div[contains(@class,'niO4u')][contains(.,'{label}')]",
        ]:
            try:
                btn = driver.find_element(By.XPATH, xpath)
                if btn.is_displayed():
                    btn.click()
                    print(f" [copy '{label}'✓]", end="", flush=True)
                    return True
            except Exception:
                pass

    print(" [⚠️ copy btn not found]", end="", flush=True)
    return False


# =============================================================================
# GOOGLE LENS — Full flow
# =============================================================================

def upload_and_get_text(driver: webdriver.Chrome,
                        img_path: str,
                        target_language: str = "English") -> str:

    # Step 1: Load Lens upload page
    driver.get("https://lens.google.com/upload")
    time.sleep(3)

    # Step 2: Upload image
    if not upload_image(driver, img_path):
        return ""

    # Step 3: Wait for Lens to navigate to results page
    if not wait_for_results(driver):
        # Page didn't navigate — try once more with a fresh load
        print(" [retry upload]", end="", flush=True)
        driver.get("https://lens.google.com/upload")
        time.sleep(3)
        if not upload_image(driver, img_path):
            return ""
        if not wait_for_results(driver):
            print(" ❌ results page never loaded")
            return ""

    # Step 4: Click Translate
    print(" [translate]", end="", flush=True)
    click_translate_button(driver)

    # Step 5: Set language
    set_target_language(driver, target_language)
    time.sleep(2)

    # Step 6: Copy
    copy_ok = click_copy_button(driver)

    # Step 7: Poll clipboard
    if copy_ok:
        for _ in range(20):
            time.sleep(0.5)
            try:
                text = pyperclip.paste()
                if text and len(text) > 20:
                    print(f" ✅ {len(text)} chars")
                    return text
            except Exception:
                pass
        print(" [clipboard empty→DOM]", end="", flush=True)

    # Step 8: DOM fallback
    try:
        collected, seen = [], set()
        for css in [
            "div.ob9lvb", "div[jsname='r4nke']", "div.dDoNo",
            "div[data-language]", "span[lang]", "div.fLtMuf",
            "div.I6TXqe", "div.fihFlb", "div[jscontroller] span",
        ]:
            try:
                for el in driver.find_elements(By.CSS_SELECTOR, css):
                    t = el.text.strip()
                    if t and t not in seen and len(t) > 3:
                        seen.add(t)
                        collected.append(t)
            except Exception:
                pass
        text = "\n".join(collected)
        print(f" {'✅' if text else '❌'} {len(text)} chars (DOM)")
        return text
    except Exception as e:
        print(f" ❌ {e}")
        return ""


# =============================================================================
# PARSERS
# =============================================================================

def parse_header(text: str) -> dict:
    info = {
        "constituency_no": "", "constituency_name": "", "part_no": "",
        "bhag_no": "", "bhag_name": "", "polling_station_no": "",
        "polling_station_name": "", "thana": "", "gram_panchayat": "",
        "block": "", "sub_division": "", "district": "", "pin": "",
    }
    m = re.search(r'নং[,،]\s*নাম\s*[:]\s*(\d+)[–\-](.+?)(?:\n|ভাগ|অংশ)', text)
    if m:
        info["constituency_no"]   = m.group(1).strip()
        info["constituency_name"] = m.group(2).strip()
    m = re.search(r'ভাগ নং[,،]\s*নাম\s*[:]\s*(\d+)[–\-](.+?)(?:\n|অংশ)', text)
    if m:
        info["bhag_no"]   = m.group(1).strip()
        info["bhag_name"] = m.group(2).strip()
    m = re.search(r'অংশ নং\s*[:।]\s*(\d+)', text)
    if m: info["part_no"] = m.group(1)
    for key, pats in [
        ("thana",                [r'থানা\s*[:।]\s*(.+)']),
        ("gram_panchayat",       [r'গ্রাম পঞ্চায়েত\s*[:।]\s*(.+)', r'ওয়ার্ড\s*[:।]\s*(.+)']),
        ("block",                [r'ব্লক\s*[:।]\s*(.+)']),
        ("sub_division",         [r'মহকুমা\s*[:।]\s*(.+)']),
        ("district",             [r'জেলা\s*[:।]\s*(.+)']),
        ("pin",                  [r'পিন\s*[:।]\s*(\d+)']),
        ("polling_station_no",   [r'ভোট গ্রহন কেন্দ্রের নং\s*[:।]\s*(\d+)']),
        ("polling_station_name", [r'ভোট গ্রহন কেন্দ্রের নাম\s*[:।]\s*(.+)']),
    ]:
        for pat in pats:
            m = re.search(pat, text)
            if m:
                info[key] = m.group(1).strip()
                break
    return info


def parse_voters_english(text: str, source_file: str, page_no: int,
                         eroll_folder: str, header_info: dict) -> list:
    rows  = []
    text  = re.sub(r'Photo available', '', text, flags=re.IGNORECASE)
    text  = re.sub(r'Publication date.*', '', text, flags=re.IGNORECASE)
    text  = re.sub(r'Total pages.*', '', text, flags=re.IGNORECASE)
    text  = re.sub(r'\n{2,}', '\n', text)
    lines = [l.strip() for l in text.split('\n') if l.strip()]

    VID_RE     = re.compile(r'\b([A-Z]{2,4}[/\d]{5,})\b')
    serial_idx = [i for i, l in enumerate(lines) if re.match(r'^\d{3,4}$', l)]

    for n, start in enumerate(serial_idx):
        end   = serial_idx[n + 1] if n + 1 < len(serial_idx) else len(lines)
        block = lines[start:end]
        bt    = " ".join(block)

        voter = {
            "serial": "", "name": "", "father_husband_name": "",
            "relation_type": "", "house_no": "", "age": "",
            "gender": "", "voter_id": "",
        }
        voter["serial"] = block[0].strip()

        vid = VID_RE.search(bt)
        if vid: voter["voter_id"] = vid.group(1)

        for pat, field in [
            (r"Name\s*:\s*(.+?)(?=\s+(?:Father|Husband|Mother)'s name|\s+House|\s+Age|$)", "name"),
            (r"Father's name\s*:\s*(.+?)(?=\s+House|\s+Age|$)",  "father_husband_name"),
            (r"Husband's name\s*:\s*(.+?)(?=\s+House|\s+Age|$)", "father_husband_name"),
            (r"Mother's name\s*:\s*(.+?)(?=\s+House|\s+Age|$)",  "father_husband_name"),
            (r"House\s*(?:No|Number)\s*[:#.]\s*(\S+)",           "house_no"),
            (r"Age\s*:\s*(\d+)",                                  "age"),
        ]:
            m = re.search(pat, bt, re.IGNORECASE)
            if m and not voter[field]:
                voter[field] = m.group(1).strip()

        if re.search(r"Father's name",   bt, re.IGNORECASE): voter["relation_type"] = "father"
        elif re.search(r"Husband's name", bt, re.IGNORECASE): voter["relation_type"] = "husband"
        elif re.search(r"Mother's name",  bt, re.IGNORECASE): voter["relation_type"] = "mother"

        gm = re.search(r'Gender\s*:\s*(\S+)', bt, re.IGNORECASE)
        if gm:
            g = gm.group(1).lower()
            voter["gender"] = "Female" if 'female' in g else \
                              "Male"   if 'male'   in g else gm.group(1)

        if voter["name"]:
            rows.append({**voter, **header_info,
                         "source_file":  source_file,
                         "page_no":      page_no,
                         "eroll_folder": eroll_folder})
    return rows


# =============================================================================
# WALK INPUT FOLDERS
# =============================================================================

def get_all_tasks(root: str) -> list:
    tasks = []
    for eroll_label in sorted(os.listdir(root)):
        ep = os.path.join(root, eroll_label)
        if not os.path.isdir(ep) or not eroll_label.startswith("eroll-"):
            continue
        for booth_folder in sorted(os.listdir(ep)):
            bp = os.path.join(ep, booth_folder)
            if not os.path.isdir(bp):
                continue
            jpgs = sorted([
                os.path.join(bp, f)
                for f in os.listdir(bp) if f.lower().endswith(".jpg")
            ])
            header_jpg = next(
                (j for j in jpgs if os.path.basename(j).lower() == "header.jpg"), None
            )
            voter_jpgs = sorted(
                [j for j in jpgs
                 if re.match(r'voter_p\d+\.jpg', os.path.basename(j).lower())],
                key=lambda j: int(re.search(r'p(\d+)', os.path.basename(j)).group(1))
                              if re.search(r'p(\d+)', os.path.basename(j)) else 0
            )
            if voter_jpgs or header_jpg:
                tasks.append({
                    "eroll_label":  eroll_label,
                    "booth_folder": booth_folder,
                    "header_jpg":   header_jpg,
                    "voter_jpgs":   voter_jpgs,
                })
    return tasks


# =============================================================================
# MAIN
# =============================================================================

def main():
    print("\n" + "="*65)
    print("🗳️  ELECTORAL ROLL — GOOGLE LENS SCRAPER (Selected Booths)")
    print("    Header: Bengali  |  Voter pages: English (per-page .txt)")
    print("="*65)

    done  = load_checkpoint()
    tasks = get_all_tasks(EXTRACTED_ROOT)

    pending = [
        t for t in tasks
        if (
            (t["header_jpg"] and
             f"{t['eroll_label']}/{t['booth_folder']}/header.jpg" not in done)
            or
            any(
                f"{t['eroll_label']}/{t['booth_folder']}/{os.path.basename(j)}"
                not in done
                for j in t["voter_jpgs"]
            )
        )
    ]

    print(f"\n  Booth folders total : {len(tasks)}")
    print(f"  Pending this run    : {len(pending)}")

    if not pending:
        print("\n✅ Nothing to do.")
        return

    confirm = input(f"\n▶️  Start? (y/n): ")
    if confirm.lower() != 'y':
        return

    driver   = make_driver()
    all_rows = []

    # One-time: open Lens and dismiss consent/cookie banners
    print("\n  🌐 Opening Lens — dismissing consent banners if any...")
    driver.get("https://lens.google.com/upload")
    time.sleep(4)
    for xpath in [
        "//button[contains(.,'Alle akzeptieren')]",
        "//button[contains(.,'Akzeptieren')]",
        "//button[contains(.,'Ich stimme zu')]",
        "//button[contains(.,'Accept all')]",
        "//button[contains(.,'Accept')]",
        "//button[contains(.,'Agree')]",
        "//button[contains(.,'Tout accepter')]",
        "//button[contains(.,'Accepter')]",
        "//*[@aria-label='Alle akzeptieren']",
        "//*[@aria-label='Accept all']",
    ]:
        try:
            btn = driver.find_element(By.XPATH, xpath)
            if btn.is_displayed():
                btn.click()
                print("  ✅ Dismissed consent banner")
                time.sleep(2)
                break
        except Exception:
            pass

    try:
        for i, task in enumerate(pending, 1):
            eroll_label  = task["eroll_label"]
            booth_folder = task["booth_folder"]
            header_jpg   = task["header_jpg"]
            voter_jpgs   = task["voter_jpgs"]

            print(f"\n[{i}/{len(pending)}] {eroll_label} / {booth_folder}")

            out_dir = os.path.join(OUTPUT_ROOT, eroll_label, booth_folder)
            os.makedirs(out_dir, exist_ok=True)

            # ── HEADER: Bengali → Bengali ──────────────────────────────────────
            header_info = {}
            header_key  = f"{eroll_label}/{booth_folder}/header.jpg"

            if header_jpg and header_key not in done:
                print("  🏛️  Header (Bengali→Bengali)...")
                htext = upload_and_get_text(driver, header_jpg,
                                            target_language="Bengali")
                if htext:
                    hdr_out = os.path.join(out_dir, "header.txt")
                    with open(hdr_out, "w", encoding="utf-8") as f:
                        f.write(htext)
                    print(f"     → header.txt saved")
                    header_info = parse_header(htext)
                    print(f"     constituency = {header_info.get('constituency_name','')}")
                    print(f"     part         = {header_info.get('part_no','')}")
                    print(f"     district     = {header_info.get('district','')}")
                else:
                    print("     ⚠️  No header text extracted")

                done.add(header_key)
                save_checkpoint(done)
                time.sleep(DELAY_BETWEEN)

            elif header_jpg:
                hdr_path = os.path.join(out_dir, "header.txt")
                if os.path.isfile(hdr_path):
                    with open(hdr_path, "r", encoding="utf-8") as f:
                        header_info = parse_header(f.read())
                print(f"  ⏭️  header.jpg (already done)")

            # ── VOTER PAGES: Bengali → English ────────────────────────────────
            booth_rows = []

            for jpg in voter_jpgs:
                jname    = os.path.basename(jpg)
                task_key = f"{eroll_label}/{booth_folder}/{jname}"

                if task_key in done:
                    print(f"  ⏭️  {jname}")
                    txt_path = os.path.join(out_dir, jname.replace(".jpg", ".txt"))
                    if os.path.isfile(txt_path):
                        with open(txt_path, "r", encoding="utf-8") as f:
                            existing = f.read()
                        pm      = re.search(r'p(\d+)', jname)
                        page_no = int(pm.group(1)) if pm else 0
                        rows    = parse_voters_english(
                            existing, booth_folder, page_no, eroll_label, header_info
                        )
                        booth_rows.extend(rows)
                    continue

                pm      = re.search(r'p(\d+)', jname)
                page_no = int(pm.group(1)) if pm else 0

                print(f"  🗳️  {jname} (→ English)")
                text = upload_and_get_text(driver, jpg, target_language="English")

                if text:
                    txt_out = os.path.join(out_dir, jname.replace(".jpg", ".txt"))
                    with open(txt_out, "w", encoding="utf-8") as f:
                        f.write(text)
                    print(f"     → {jname.replace('.jpg','.txt')} saved")
                    rows = parse_voters_english(
                        text, booth_folder, page_no, eroll_label, header_info
                    )
                    print(f"     → {len(rows)} voters parsed")
                    booth_rows.extend(rows)
                else:
                    print("     ⚠️  No text extracted")

                done.add(task_key)
                save_checkpoint(done)
                time.sleep(DELAY_BETWEEN)

            # ── Per-booth CSV ──────────────────────────────────────────────────
            if booth_rows:
                df = pd.DataFrame(booth_rows)
                for col in COLUMN_ORDER:
                    if col not in df.columns:
                        df[col] = ""
                df[COLUMN_ORDER].to_csv(
                    os.path.join(out_dir, "voters.csv"),
                    index=False, encoding="utf-8-sig"
                )
                print(f"  ✅ {len(booth_rows)} voters → voters.csv")
                all_rows.extend(booth_rows)

    except KeyboardInterrupt:
        print("\n⚠️  Stopped — progress saved.")
    finally:
        driver.quit()

    # ── Combined CSV ───────────────────────────────────────────────────────────
    if all_rows:
        df_all = pd.DataFrame(all_rows)
        for col in COLUMN_ORDER:
            if col not in df_all.columns:
                df_all[col] = ""
        combined_path = os.path.join(OUTPUT_ROOT, "all_voters.csv")
        df_all[COLUMN_ORDER].to_csv(combined_path, index=False, encoding="utf-8-sig")
        print(f"\n✅ all_voters.csv — {len(df_all)} total voters")

    print(f"\n✅ Done! Output: {OUTPUT_ROOT}")
    print("""
Output structure:
  extracted_data_from_booths/
    eroll-1/
      2026-EROLLGEN-S25-1-SIR-.../
        header.txt
        voter_p005.txt  ...
        voters.csv
    all_voters.csv
""")


if __name__ == "__main__":
    main()