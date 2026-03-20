# ══════════════════════════════════════════════════════════════
# Demonstrating step_unknown(), step_novel(), and step_other()
# Using a controlled subset of the cars dataset
# ══════════════════════════════════════════════════════════════
#
# KEY CONTEXT: Modern tidymodels (2024+) no longer crashes when
# it encounters NAs or unseen factor levels. Instead, it WARNS
# and handles things gracefully — but "gracefully" can mean
# SILENTLY WRONG if you're not paying attention.
#
# This demo shows:
#   1. What happens WITHOUT these steps (silent corruption)
#   2. What each step does (incremental build)
#   3. Why the ORDER of steps matters (tidymodels tells you!)
# ══════════════════════════════════════════════════════════════

library(tidyverse)
library(tidymodels)

cars <- read_csv('https://www.dropbox.com/scl/fi/xavej23qpauvx3xfdq7zh/car_sales.csv?rlkey=4mfp6tpia0uqkcoiqf9jleau3&dl=1')

# ── Build a controlled train / test split ─────────────────────
#
# We need a scenario where:
#   (a) NAs exist → step_unknown() has something to fix
#   (b) Some makes are rare → step_other() collapses them
#   (c) A make appears in test that NEVER appeared in training
#       → step_novel() catches it
#   (d) The "missing" level is frequent enough to SURVIVE
#       step_other(), so we can actually see it in the dummies

set.seed(42)
cars_split <- initial_split(cars, prop = 0.8)
cars_training <- training(cars_split)
cars_testing  <- testing(cars_split)

set.seed(42)

cars_train_demo <- bind_rows(
  cars_training |> filter(make == "Ford")      |> slice_sample(n = 500),
  cars_training |> filter(make == "Chevrolet") |> slice_sample(n = 400),
  cars_training |> filter(make == "Toyota")    |> slice_sample(n = 200),
  cars_training |> filter(make == "Nissan")    |> slice_sample(n = 50),   # rare
  cars_training |> filter(make == "Dodge")     |> slice_sample(n = 30),   # rare
  cars_training |> filter(is.na(make))         |> slice_sample(n = 80)    # NAs
)

cars_test_demo <- bind_rows(
  cars_testing |> filter(make == "Ford")      |> slice_sample(n = 120),
  cars_testing |> filter(make == "Chevrolet") |> slice_sample(n = 100),
  cars_testing |> filter(make == "Toyota")    |> slice_sample(n = 60),
  cars_testing |> filter(make == "BMW")       |> slice_sample(n = 40),   # UNSEEN
  cars_testing |> filter(is.na(make))         |> slice_sample(n = 25)    # NAs
)

# Quick check: what does each set look like?
cars_train_demo |> count(make, sort = TRUE)
# → Ford(500), Chev(400), Toyota(200), NA(80), Nissan(50), Dodge(30)

cars_test_demo |> count(make, sort = TRUE)
# → Ford(120), Chev(100), Toyota(60), BMW(40), NA(25)

# CRITICAL: BMW is NOT in training. NAs exist in both.


# ══════════════════════════════════════════════════════════════
# DEMO 1: What happens WITHOUT these steps?
#         (Spoiler: no crash, but silent problems)
# ══════════════════════════════════════════════════════════════

# -- 1a: Bare step_dummy() on data with NAs --
bare_rec <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_dummy(make)

bare_baked <- bare_rec |> prep() |> bake(cars_train_demo)
# ⚠ WARNING: "There are new levels in `make`: NA"
# ⚠ "Consider using step_unknown() before step_dummy()"

# But wait — it didn't crash! Let's look at what it DID do:
bare_baked |>
  select(starts_with("make_")) |>
  summarize(across(everything(), \(x) sum(x, na.rm = TRUE)))
# → The 80 NA rows get ALL-ZERO dummy columns.
#   That means they're absorbed into the REFERENCE LEVEL (Chevrolet).
#   Your model now thinks 80 cars with unknown make are Chevrolets. 😬

# How many rows have all-zero dummies? (= treated as reference level)
bare_baked |>
  select(starts_with("make_")) |>
  filter(if_all(everything(), ~ . == 0)) |>
  nrow()
# → 480 = 400 Chevrolets + 80 NAs. The NAs silently became Chevrolets.


# -- 1b: With step_unknown() + step_other() but NO step_novel() --
no_novel_rec <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_unknown(make, new_level = "missing") |>
  step_other(make, threshold = 0.05) |>
  step_dummy(make)

no_novel_baked <- no_novel_rec |> prep() |> bake(cars_test_demo)
# ⚠ WARNING: "There are new levels in `make`: BMW"
# ⚠ "Consider using step_novel() before step_unknown()"
# ⚠ "New levels will be coerced to NA by step_unknown()"

# Again — no crash! But what happened to BMW?
no_novel_baked |>
  select(starts_with("make_")) |>
  summarize(across(everything(), \(x) sum(x, na.rm = TRUE)))
# → make_missing shows 25 (our actual NAs), but where are the 40 BMWs?
#   BMW → coerced to NA → then step_unknown() converts NA to "missing"
#   So BMW is now counted as "missing" alongside real NAs!
#   You've lost the distinction between "I don't know the make" and
#   "this is a brand I've never seen before." Not great.

# KEY LESSON: Modern tidymodels won't crash. But "no crash" ≠ "correct."
# The data is silently corrupted. In production, this is WORSE than
# a crash — at least a crash tells you something is wrong.


# ══════════════════════════════════════════════════════════════
# DEMO 2: Build the recipe CORRECTLY — step by step
# ══════════════════════════════════════════════════════════════

# Step 0: Raw test data — our starting point
cars_test_demo |>
  count(make, sort = TRUE) |>
  mutate(pct = scales::percent(n / sum(n)))
# make          n pct
# Ford        120 34.8%
# Chevrolet   100 29.0%
# Toyota       60 17.4%
# BMW          40 11.6%    ← unseen in training
# NA           25  7.2%    ← missing


# ── STEP 1: step_novel() first ────────────────────────────────
# WHY FIRST? Because we need to catch unseen levels BEFORE
# step_unknown() turns them into NAs. Read the warnings above —
# tidymodels literally tells you: "use step_novel() BEFORE
# step_unknown()."

rec_step1 <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_novel(make, new_level = "brand_new")

rec_step1 |> prep() |> bake(cars_test_demo) |>
  count(make, sort = TRUE) |>
  mutate(pct = scales::percent(n / sum(n)))
# → BMW becomes "brand_new" (40 rows)  ← step_novel caught it!
# → NAs are still NA (25 rows)         ← not handled yet


# ── STEP 2: step_novel() + step_unknown() ─────────────────────
rec_step2 <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_novel(make, new_level = "brand_new") |>
  step_unknown(make, new_level = "missing")

rec_step2 |> prep() |> bake(cars_test_demo) |>
  count(make, sort = TRUE) |>
  mutate(pct = scales::percent(n / sum(n)))
# → "brand_new" = 40 rows (BMW, caught by step_novel)
# → "missing"   = 25 rows (NAs, caught by step_unknown)
# → No warnings! Both problems explicitly handled.
# COMPARE THIS to Demo 1b where BMW silently became "missing"


# ── STEP 3: + step_other() ───────────────────────────────────
rec_step3 <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_novel(make, new_level = "brand_new") |>
  step_unknown(make, new_level = "missing") |>
  step_other(make, threshold = 0.05)

rec_step3 |> prep() |> bake(cars_test_demo) |>
  count(make, sort = TRUE) |>
  mutate(pct = scales::percent(n / sum(n)))
# → Ford, Chevrolet, Toyota survive (each > 5% of training)
# → "missing" SURVIVES (80/1260 = 6.3% of training > 5% threshold)
# → Nissan (4.0%), Dodge (2.4%), "brand_new" (0%) → collapsed to "other"
# → No warnings!


# ── STEP 4: The full recipe with step_dummy() ────────────────
rec_full <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_novel(make, new_level = "brand_new") |>
  step_unknown(make, new_level = "missing") |>
  step_other(make, threshold = 0.05) |>
  step_dummy(make)

rec_full_prepped <- rec_full |> prep()

# What dummy columns did we get?
rec_full_prepped |>
  bake(cars_test_demo) |>
  select(starts_with("make_"))  
# → make_Ford, make_Toyota, make_missing, make_other
#   (Chevrolet is the reference level — gets all zeros)

# Count how many test rows land in each bucket
rec_full_prepped |>
  bake(cars_test_demo) |>
  select(starts_with("make_")) |>
  summarize(across(everything(), sum))
# → make_Ford = 120, make_Toyota = 60, make_missing = 25, make_other = 40
# → The 40 BMW rows land cleanly in "other" via brand_new → other
# → The 25 NA rows are explicitly "missing" — a real signal!
# → No warnings, no silent corruption. Everything is explicit.


# ══════════════════════════════════════════════════════════════
# DEMO 3: The ordering matters — wrong vs. right
# ══════════════════════════════════════════════════════════════

# ❌ WRONG ORDER: step_unknown() before step_novel()
rec_wrong_order <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_unknown(make, new_level = "missing") |>
  step_novel(make, new_level = "brand_new") |>
  step_other(make, threshold = 0.05) |>
  step_dummy(make)

wrong_baked <- rec_wrong_order |> prep() |> bake(cars_test_demo)
# ⚠ WARNING: new levels in make: BMW — coerced to NA by step_unknown()
# BMW → NA → "missing" ... lumped with real NAs. step_novel never fires.

wrong_baked |>
  select(starts_with("make_")) |>
  summarize(across(everything(), sum))
# → make_missing = 65 (25 real NAs + 40 BMWs all mixed together)
# → make_other = 0   (no one ended up here!)
# You can't tell a missing value from an unseen brand. Bad.


# ✅ RIGHT ORDER: step_novel() before step_unknown()
rec_right_order <- recipe(sellingprice_log ~ make, data = cars_train_demo) |>
  step_novel(make, new_level = "brand_new") |>
  step_unknown(make, new_level = "missing") |>
  step_other(make, threshold = 0.05) |>
  step_dummy(make)

right_baked <- rec_right_order |> prep() |> bake(cars_test_demo)
# → No warnings! Everything handled cleanly.

right_baked |>
  select(starts_with("make_")) |>
  summarize(across(everything(), sum))
# → make_missing = 25 (real NAs only)
# → make_other = 40   (BMWs, via brand_new → other)
# Clean separation. You know exactly what went where.


# ══════════════════════════════════════════════════════════════
# DEMO 4: Side-by-side comparison — why this matters for models
# ══════════════════════════════════════════════════════════════

# Let's see the actual difference row by row
comparison <- tibble(
  original_make = cars_test_demo$make,
  wrong_order = wrong_baked |>
    select(starts_with("make_")) |>
    mutate(bucket = case_when(
      make_Ford == 1    ~ "Ford",
      make_Toyota == 1  ~ "Toyota",
      make_missing == 1 ~ "missing",
      make_other == 1   ~ "other",
      TRUE              ~ "Chevrolet (ref)"
    )) |> pull(bucket),
  right_order = right_baked |>
    select(starts_with("make_")) |>
    mutate(bucket = case_when(
      make_Ford == 1    ~ "Ford",
      make_Toyota == 1  ~ "Toyota",
      make_missing == 1 ~ "missing",
      make_other == 1   ~ "other",
      TRUE              ~ "Chevrolet (ref)"
    )) |> pull(bucket)
)

comparison

# Where do they disagree?
comparison |>
  filter(wrong_order != right_order) |>
  count(original_make, wrong_order, right_order)
# → 40 BMW rows: wrong_order puts them in "missing",
#                 right_order puts them in "other"
# That's a material difference for your model!


# ══════════════════════════════════════════════════════════════
# SUMMARY: The correct recipe order (and why)
# ══════════════════════════════════════════════════════════════
#
#   1. step_novel()     unseen → "brand_new"   (catch new categories FIRST)
#   2. step_unknown()   NA → "missing"         (then handle NAs)
#   3. step_other()     rare → "other"         (collapse infrequent levels)
#   4. step_dummy()     factors → 0/1 columns  (numeric encoding)
#
# WHY THIS ORDER:
#   - step_novel() must come first so unseen levels don't get
#     silently coerced to NA by step_unknown()
#   - step_unknown() must come before step_dummy() so NAs get
#     an explicit level instead of all-zero dummies
#   - step_other() reduces dimensionality before dummifying
#   - step_dummy() goes last to create the numeric representation
#
# THE BIG TAKEAWAY:
#   Modern tidymodels won't crash on NAs or unseen levels.
#   It warns and handles them — but "handles" often means
#   "silently converts to NA" or "absorbs into the reference level."
#
#   Using these three steps in the right order gives you EXPLICIT
#   CONTROL over where every row ends up. No surprises in production.
#
# Think of it this way:
#   step_novel()   = handling unknown unknowns (new brands at inference)
#   step_unknown() = handling known unknowns   (we know NAs exist)
#   step_other()   = keeping the model lean    (too many small categories)
