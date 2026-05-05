import pandas as pd
import numpy as np

# ── LOAD: keep only columns we actually need ──────────────────────────
COLS = [
    'eroll_no', 'constituency_name', 'booth_no',
    'total_individuals', 'area_type',
    'gender', 'age', 'R', 'G', 'SG'
]

df = pd.read_csv(
    "final_pred_updated.csv",
    usecols=COLS,
    low_memory=False,
    dtype={'eroll_no': 'int32', 'booth_no': 'float32',
           'total_individuals': 'float32'}
)

# ── STEP 1: AGE BANDS ────────────────────────────────────────────────
df['age_band'] = pd.cut(
    df['age'],
    bins=[18, 25, 35, 45, 60, np.inf],
    labels=['18-25', '26-35', '36-45', '46-60', '60+'],
    right=True
)
df = df.drop(columns=['age'])  # drop raw age immediately

# ── STEP 2: FILL missing area_type from booth majority ───────────────
# Work only on the small booth-level table, not the full df
booth_area = (
    df.dropna(subset=['area_type'])
      .groupby(['eroll_no', 'booth_no'])['area_type']
      .agg(lambda x: x.mode()[0])
      .rename('area_filled')
)

# Map back without a merge — use index lookup
missing_mask = df['area_type'].isna()
df.loc[missing_mask, 'area_type'] = (
    df.loc[missing_mask, ['eroll_no', 'booth_no']]
      .apply(lambda r: booth_area.get((r['eroll_no'], r['booth_no'])), axis=1)
)
del booth_area

# ── STEP 3: BOOTH WEIGHTS — computed entirely at booth level ──────────
# One row per booth
booth_tbl = (
    df.groupby(['eroll_no', 'booth_no'])['total_individuals']
      .first()
      .reset_index()
)

# Constituency total = sum of booth totals
booth_tbl['const_total'] = booth_tbl.groupby('eroll_no')['total_individuals'].transform('sum')
booth_tbl['booth_weight'] = booth_tbl['total_individuals'] / booth_tbl['const_total']
booth_tbl = booth_tbl[['eroll_no', 'booth_no', 'booth_weight']]

# Join weight onto df using a small map — avoids duplicate columns
weight_map = booth_tbl.set_index(['eroll_no', 'booth_no'])['booth_weight']
df['booth_weight'] = pd.array(
    [weight_map.get((e, b), np.nan)
     for e, b in zip(df['eroll_no'], df['booth_no'])],
    dtype='float32'
)
del booth_tbl, weight_map
df = df.drop(columns=['total_individuals', 'booth_no'])  # no longer needed

# ── STEP 4: STRATIFICATION FRAME ─────────────────────────────────────
strata_cols = [
    'eroll_no', 'constituency_name',
    'gender', 'age_band', 'area_type',
    'R', 'G', 'SG'
]

strat_frame = (
    df.groupby(strata_cols, dropna=False, observed=True)
      .agg(
          n_individuals  = ('booth_weight', 'count'),
          weighted_n     = ('booth_weight', 'sum')
      )
      .reset_index()
)
del df  # free the big frame

# ── STEP 5: STRATUM SHARE ─────────────────────────────────────────────
strat_frame['const_weighted_total'] = strat_frame.groupby('eroll_no')['weighted_n'].transform('sum')
strat_frame['stratum_share'] = strat_frame['weighted_n'] / strat_frame['const_weighted_total']
strat_frame = strat_frame.drop(columns=['const_weighted_total'])

strat_frame = strat_frame.sort_values(
    ['eroll_no', 'stratum_share'], ascending=[True, False]
).reset_index(drop=True)

print(strat_frame.shape)
print(strat_frame.head(10))
strat_frame.to_csv("stratification_frame_cleaned.csv", index=False)