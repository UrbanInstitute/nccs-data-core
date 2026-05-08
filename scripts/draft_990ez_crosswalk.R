suppressPackageStartupMessages({
  library(data.table)
})

# Load IRS dictionary entries for 990-EZ
v <- fread("data/raw/soi_dictionaries/_all_variables.csv")
vez <- v[form == "990EZ"]

# Range-collapse year list
collapse_years <- function(yrs) {
  yrs <- sort(unique(as.integer(yrs)))
  if (length(yrs) == 0) return("")
  breaks <- c(0L, which(diff(yrs) != 1L), length(yrs))
  parts <- character()
  for (i in seq_len(length(breaks) - 1L)) {
    run <- yrs[(breaks[i] + 1L):breaks[i + 1L]]
    parts[i] <- if (length(run) == 1L) as.character(run) else
                paste0(run[1], "-", run[length(run)])
  }
  paste(parts, collapse = ", ")
}

setorder(vez, variable_name, -year)
src <- vez[, .(years_present = collapse_years(year),
               description = first(description),
               location = first(location)),
           by = .(variable_name)]
setnames(src, "variable_name", "source_var")
setorder(src, source_var)

# ---- Step 1: import 990 harmonized names for shared source vars ----
f990 <- fread("data/crosswalks/soi_990_crosswalk_FINAL.csv")
# Build case-insensitive lookup: source_var_upper -> harmonized_name
f990[, key := toupper(source_var)]
f990_lookup <- f990[, .(harmonized_name_990 = first(harmonized_name)), by = key]

src[, key := toupper(source_var)]
src[f990_lookup, on = "key", harmonized_name := i.harmonized_name_990]
src[, key := NULL]

# Tag which rows are inherited from 990
src[, source := ifelse(!is.na(harmonized_name), "from_990", "ez_only")]

# ---- Step 2: algorithmic baseline for ez_only rows ----
# Reuse the same expansion/cleanup machinery as 990
expansions <- list(
  c("totcntrbs",        "total_contributions"),
  c("prgmservrev",      "program_service_revenue"),
  c("duesassesmnts",    "dues_assessments"),
  c("othrinvstinc",     "other_investment_income"),
  c("grsamtsalesastothr","gross_sales_other_assets"),
  c("basisalesexpnsothr","cost_basis_sales_expenses_other"),
  c("gnsaleofastothr",  "gain_loss_sales_other_assets"),
  c("grsrevnuefndrsng", "gross_revenue_fundraising"),
  c("grspublicrcpts",   "gross_public_receipts"),
  c("grsalesminusret",  "gross_sales_less_returns"),
  c("costgoodsold",     "cost_of_goods_sold"),
  c("grsprft",          "gross_profit"),
  c("othrevnue",        "other_revenue"),
  c("totrevnue",        "total_revenue"),
  c("totexpns",         "total_expenses"),
  c("totexcessyr",      "excess_for_year"),
  c("othrchgsnetassetfnd","other_changes_net_assets"),
  c("totnetassetsend",  "total_net_assets_eoy"),
  c("networthend",      "net_worth_eoy"),
  c("unrelbusincd",     "unrelated_business_income_cd"),
  c("initiationfee",    "initiation_fees"),
  c("gftgrntrcvd170",   "gifts_grants_received_sec170"),
  c("grsrcptsrelatd170","gross_receipts_related_activities_sec170"),
  c("grsrcptsadmiss509","gross_receipts_admissions_sec509"),
  c("actvtynotprevrptcd","activity_not_prev_reported_cd"),
  c("chngsinorgcd",     "changes_in_org_documents_cd"),
  c("contractioncd",    "contraction_cd"),
  c("politicalexpend",  "political_expenditures"),
  c("filedf1120polcd",  "filed_form_1120pol_cd"),
  c("loanstoofficerscd","loans_to_officers_cd"),
  c("loanstoofficers",  "loans_to_officers"),
  c("s4958excessbenefcd","s4958_excess_benefit_cd"),
  c("direxpns",         "direct_expenses"),
  # Cleanup
  c("end",              "_eoy"),
  c("cd",               "_cd"),
  c("EIN",              "ein"),
  c("ein",              "ein")
)

normalize <- function(x) {
  s <- tolower(x)
  for (e in expansions) s <- gsub(e[1], paste0("_", e[2], "_"), s, fixed = TRUE)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}

ez_only <- src$source == "ez_only"
src[ez_only, harmonized_name := vapply(source_var, normalize, character(1))]

# Apply IRC section fix to ez_only rows
fix_section <- function(s, n) {
  num <- as.character(n)
  already <- endsWith(s, paste0("sec", num))
  ends_in_num <- endsWith(s, num) & !already
  s[ends_in_num] <- sub(paste0("_*", num, "$"), "", s[ends_in_num])
  s[ends_in_num] <- paste0(s[ends_in_num], "_sec", num)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}
src[ez_only, harmonized_name := fix_section(harmonized_name, 170)]
src[ez_only, harmonized_name := fix_section(harmonized_name, 509)]

# ---- Step 3: form-text-verified + cross-form-aligned overrides ----
overrides <- data.table(
  source_var = c(
    # Group A — algorithmic bugs
    "excds1pct509",
    "excds2pct170",
    "politicalexpend",
    "taxpd",
    "totnoforgscnt",
    "grsrcptsactvts509",
    "netincunrelatd170",
    "netincunreltd509",
    "pubsupplesssub509",
    "totsupport170",
    # Group B — form-verified semantic fixes
    "actvtynotprevrptcd",   # line 33 — significant new activity
    "chngsinorgcd",          # line 34 — changes to governing docs
    "contractioncd",         # line 36 — liquidation/dissolution/termination
    "unrelbusincd",          # line 35a — UBI >= $1,000
    "s4958excessbenefcd",    # line 40b — excess benefit transaction (matches 990)
    "direxpns",              # line 6c — gaming+fundraising direct expenses
    # Group C — cross-form synonym alignment
    "othrinvstinc",          # line 4 — investment income (synonym of 990 invstmntinc)
    "grspublicrcpts",        # line 39b — gross receipts public use (synonym of 990 grsrcptspublicuse)
    "grsrevnuefndrsng",      # line 6b — gross income from fundraising events (synonym of 990 grsincfndrsng)
    "networthend",           # Pt II line 27B — total net assets EOY (synonym of totnetassetsend)
    "prgmservrev",           # line 2 — program service revenue (cross-form alignment)
    # Group D — form precision improvements
    "basisalesexpnsothr",    # line 5b
    "grsalesminusret",       # line 7a
    "grsprft",               # line 7c
    "totexcessyr",           # line 18
    "othrchgsnetassetfnd",   # line 20
    "loanstoofficers",       # line 38b (amount, vs cd indicator on 38a)
    "duesassesmnts"          # line 3
  ),
  harmonized_name = c(
    # Group A
    "excess_over_1pct_sec509",
    "excess_over_2pct_sec170",
    "political_expenditures",
    "tax_period",
    "n_orgs_supported",
    "gross_receipts_related_activities_sec509",
    "net_ubi_sec170",
    "net_ubi_excl_10b_sec509",
    "public_support_sec509",
    "total_support_sec170",
    # Group B
    "significant_new_activity_cd",
    "changes_to_governing_docs_cd",
    "liquidated_terminated_cd",
    "unrelated_business_income_1k_cd",
    "excess_benefit_transaction_cd",
    "gaming_fundraising_direct_expenses",
    # Group C
    "investment_income",
    "gross_receipts_public_use",
    "gross_income_fundraising_events",
    "total_net_assets_eoy",
    "program_service_revenue",
    # Group D
    "cost_basis_other_assets",
    "gross_sales_inventory_less_returns",
    "gross_profit_inventory_sales",
    "excess_or_deficit_for_year",
    "other_changes_net_assets_fund_balances",
    "loans_to_officers_amount",
    "membership_dues_assessments"
  )
)
stopifnot(length(overrides$source_var) == length(overrides$harmonized_name))
if (nrow(overrides) > 0) {
  src[overrides, on = "source_var", harmonized_name := i.harmonized_name]
}

# ---- Output ----
out <- src[, .(source_var, harmonized_name, description, location, years_present, source)]

baseline_path <- "data/crosswalks/soi_990ez_crosswalk_BASELINE.csv"
overrides_path <- "data/crosswalks/soi_990ez_crosswalk_OVERRIDES.csv"
final_path <- "data/crosswalks/soi_990ez_crosswalk_FINAL.csv"

fwrite(out, baseline_path)

if (!file.exists(overrides_path)) {
  fwrite(out, overrides_path)
  cat("Initialized OVERRIDES as a copy of BASELINE:", overrides_path, "\n")
} else {
  cat("Preserving existing OVERRIDES (script will NOT overwrite):", overrides_path, "\n")
  ov <- fread(overrides_path)
  new_in_baseline <- setdiff(out$source_var, ov$source_var)
  if (length(new_in_baseline) > 0) {
    cat("\n*** ATTENTION: source_vars in BASELINE but not OVERRIDES (",
        length(new_in_baseline), ") ***\n", sep = "")
    cat("  ", paste(new_in_baseline, collapse = ", "), "\n", sep = "")
  }
}

final <- fread(overrides_path)
fwrite(final, final_path)

cat("\n=== files written ===\n")
cat("  BASELINE:", baseline_path, "\n")
cat("  OVERRIDES:", overrides_path, "\n")
cat("  FINAL:    ", final_path, "\n")
cat("rows:", nrow(out), "\n\n")

cat("=== source breakdown ===\n")
print(out[, .N, by = source])

cat("\n=== ez_only rows (algorithmic-only, need form review) ===\n")
print(out[source == "ez_only", .(source_var, harmonized_name, description, location)], nrows=60)
