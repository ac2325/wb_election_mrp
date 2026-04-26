# wb_election_mrp

This repository contains an end-to-end pipeline that forecasts all 294 West Bengal assembly constituencies using LLM-generated synthetic voters, electoral roll processing and multilevel regression and poststratification.

## Overall Data Flow

Stratification frame (EC voter rolls)  
↓  
`/n stratification_frame_cleaned.csv` (66,670 demographic strata)  
↓  
`sample_create.ipynb` → `sample_20k_sf.csv` → `sample_1–5.csv`  
↓  
LLM API (Sarvam) — 5 batches of synthetic survey respondents  
↓  
`merge_samples.ipynb` → `merged_sample_v4.csv` (25,025 respondents)  
↓  
`AI_approach_vglmer.R` (3-stage MRP models)  

- Stage 0: Turnout → `fits/stage0_turnout.rds`  
- Stage 1: P(AITC | votes) → `fits/stage1_aitc.rds`  
- Stage 2: P(NDA | not AITC) → `fits/stage2_nda.rds`  

↓  
Post-stratification using `stratification_frame_cleaned.csv`  
↓  
`projections/WB_wide.csv` (seat-level forecast)  
`projections/WB_long.csv` (long format)  
`projections/AITC/NDA/OTHER_proj.csv` (500 posterior draws each)  
↓  
`max_prob.ipynb` / `mrp_results_viz.ipynb` → plots/
