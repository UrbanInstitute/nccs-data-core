suppressPackageStartupMessages({
  library(data.table)
})

# Load IRS dictionary entries for 990-PF
v <- fread("data/raw/soi_dictionaries/_all_variables.csv")
vpf <- v[form == "990PF"]
# Drop dictionary footnote artifacts (rows that got parsed as variables)
vpf <- vpf[!startsWith(variable_name, "*")]

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

# Use most-recent year's description/location as canonical
setorder(vpf, variable_name, -year)
src <- vpf[, .(years_present = collapse_years(year),
               description = first(description),
               location = first(location)),
           by = .(variable_name)]
setnames(src, "variable_name", "source_var")
setorder(src, source_var)

# ---- Step 1: import 990 harmonized names for shared source vars (case-insensitive) ----
f990 <- fread("data/crosswalks/soi_990_crosswalk_FINAL.csv")
f990[, key := toupper(source_var)]
f990_lookup <- f990[, .(harmonized_name_990 = first(harmonized_name)), by = key]

src[, key := toupper(source_var)]
src[f990_lookup, on = "key", harmonized_name := i.harmonized_name_990]
src[, key := NULL]

# Tag rows
src[, source := ifelse(!is.na(harmonized_name), "from_990", "pf_only")]

# ---- Step 2: algorithmic baseline for pf_only rows ----
# PF source vars are ALL UPPERCASE in 2024 dictionary (and lowercase 2012-2016).
# Lowercase first, then apply expansions matching the lowercase tokens.
expansions <- list(
  # Tax / accounting / financial structure
  c("tax_prd",          "tax_period"),
  c("eostatus",         "eo_status"),
  c("operatingcd",      "operating_foundation_cd"),
  c("subcd",            "subsection_cd"),
  c("tax_yr",           "soi_year"),
  c("schedbind",        "sched_b_required_cd"),
  # Part I revenue/expense items (col-a per books)
  c("fairmrktvalamt",   "fair_mkt_value_total_assets"),
  c("grscontrgifts",    "gross_contributions_received"),
  c("intrstrvnue",      "interest_revenue"),
  c("dividndsamt",      "dividends"),
  c("grsrents",         "gross_rents"),
  c("grsslspramt",      "gross_sales_price_assets"),
  c("costsold",         "cost_of_goods_sold"),
  c("grsprofitbus",     "gross_profit"),
  c("otherincamt",      "other_income"),
  c("totrcptperbks",    "total_revenue"),
  c("compofficers",     "compensation_officers"),
  c("pensplemplbenf",   "pension_employee_benefits"),
  c("legalfeesamt",     "legal_fees"),
  c("accountingfees",   "accounting_fees"),
  c("interestamt",      "interest_expense"),
  c("depreciationamt",  "depreciation_depletion"),
  c("occupancyamt",     "occupancy"),
  c("travlconfmtngs",   "travel_conf_meetings"),
  c("printingpubl",     "printing_publications"),
  c("topradmnexpnsa",   "total_operating_admin_expenses_col_a"),
  c("contrpdpbks",      "contributions_paid"),
  c("totexpnspbks",     "total_expenses"),
  c("excessrcpts",      "excess_receipts"),
  c("totexpnsexempt",   "total_expenses_exempt_purpose"),
  c("netinvstinc",      "net_investment_income"),
  c("totaxpyr",         "tax_paid_for_year"),
  c("adjnetinc",        "adjusted_net_income"),
  c("totassetsend",     "total_assets_eoy"),
  c("invstgovtoblig",   "investments_govt_obligations"),
  c("invstcorpstk",     "investments_corp_stock"),
  c("invstcorpbnd",     "investments_corp_bonds"),
  c("totinvstsec",      "total_investments_securities"),
  c("totliabend",       "total_liabilities_eoy"),
  c("undistribincyr",   "undistributed_income_current_yr"),
  c("cmpmininvstret",   "minimum_investment_return"),
  c("sec4940notxcd",    "sec4940_no_tax_cd"),
  c("sec4940redtxcd",   "sec4940_reduced_tax_cd"),
  c("sec4940nopftxcd",  "sec4940_nonpf_tax_cd"),
  c("totl4940",         "total_section_4940_tax"),
  c("subaorbtaxinc",    "tax_on_investment_income"),
  c("noemplyeesw3",     "n_employees_w3"),
  c("nooforgssupp",     "n_orgs_supported"),
  c("totsupport",       "total_support_amount"),
  # General cleanup
  c("cd",               "_cd"),
  c("end",              "_eoy"),
  c("amt",              "_amount")
)

normalize <- function(x) {
  s <- tolower(x)
  for (e in expansions) s <- gsub(e[1], paste0("_", e[2], "_"), s, fixed = TRUE)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}

pf_only <- src$source == "pf_only"
src[pf_only, harmonized_name := vapply(source_var, normalize, character(1))]

# IRC section fix
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
src[pf_only, harmonized_name := fix_section(harmonized_name, 170)]
src[pf_only, harmonized_name := fix_section(harmonized_name, 509)]
src[pf_only, harmonized_name := fix_section(harmonized_name, 4940)]

# ---- Step 3: form-text-verified overrides ----
# Note: PF source_vars exist in BOTH lowercase (2012-2016) and UPPERCASE (2020-2024).
# Each override below covers BOTH casings via case-insensitive join further below.
overrides <- data.table(
  source_var = c(
    # Part I — column-suffix harmonization (fully symmetric col_a/b/c/d)
    "topradmnexpnsa",       # line 24 col (a)
    "topradmnexpnsb",       # line 24 col (b)
    "topradmnexpnsd",       # line 24 col (d)
    "totexpnspbks",         # line 26 col (a) — total expenses per books
    "totexpnsnetinc",       # line 26 col (b) — total expenses for net invest income calc
    "totexpnsadjnet",       # line 26 col (c) — total expenses for adj net income calc
    "totexpnsexempt",       # line 26 col (d) — total expenses (exempt-purpose disbursements)
    "totrcptperbks",        # line 12 col (a) — total revenue per books
    "totrcptnetinc",        # line 12 col (b) — total revenue for net invest income calc
    "trcptadjnetinc",       # line 12 col (c) — total revenue for adj net income calc
    "excessrcpts",          # line 27a — excess of revenue over expenses and disbursements
    # Part II — Balance Sheets (col (b) EOY book value unless noted)
    "fairmrktvaleoy",       # line 16 col (c) — total assets fair market value
    "fairmrktvalamt",       # Item I top-of-form (synonym of fairmrktvaleoy)
    "mrtgloans",            # line 12 col (b)
    "mrtgnotespay",         # line 21 col (b)
    "othrassetseoy",        # line 15 col (b)
    "othrcashamt",          # line 1 col (b) — non-interest-bearing cash
    "othrinvstend",         # line 13 col (b)
    "othrliabltseoy",       # line 22 col (b)
    "tfundnworth",          # line 31 col (b) — total fund net worth (PF for net assets)
    # Part VI-A — Statements Regarding Activities
    "infleg",                # 1a — influence legislation
    "actnotpr",              # 2 — activities not previously reported
    "chgnprvrptcd",          # 3 — changes to governing docs
    "contractncd",           # 5 — liquidation/termination/contraction
    "furnishcpycd",          # 8b — furnished copy to Attorney General
    "claimstatcd",           # 9 — claiming private operating foundation status
    "cntrbtrstxyrcd",        # 10 — new substantial contributors
    "acqdrindrintcd",        # 12 — distribution to DAF (var name lags form change)
    "orgcmplypubcd",         # 13 — complied with public inspection requirements
    "filedlf1041ind",        # 15 — 4947(a)(1) trust filing 990-PF in lieu of 1041
    # Part VI-B — Activities for Which Form 4720 May Be Required
    "propexchcd",            # 1a(1) — DQP property exchange
    "brwlndmnycd",           # 1a(2) — DQP borrow/lend money
    "furngoodscd",           # 1a(3) — DQP furnish goods/services
    "paidcmpncd",            # 1a(4) — DQP paid compensation
    "transfercd",            # 1a(5) — DQP transfer assets
    "agremkpaycd",           # 1a(6) — agreed to pay government official
    "exceptactsind",         # 1b — DQP acts failed to qualify
    "prioractvcd",           # 1c — DQP acts in prior year
    "undistrinccd",          # 2a — undistributed income
    "applyprovind",          # 2b — not applying 4942(a)(2) provisions
    "dirindirintcd",         # 3a — direct/indirect >2% interest
    "excesshldcd",           # 3b — excess business holdings
    "invstjexmptcd",         # 4a — jeopardizing investments
    "prevjexmptcd",          # 4b — prior year jeopardizing investments
    "propgndacd",            # 5a(1) — s4945 propaganda/legislation
    "ipubelectcd",           # 5a(2) — s4945 influence election
    "grntindivcd",           # 5a(3) — s4945 grant to individual
    "nchrtygrntcd",          # 5a(4) — s4945 grant to non-charity
    "nreligiouscd",          # 5a(5) — s4945 non-religious purpose
    "excptransind",          # 5b — s4945 transactions failed to qualify
    "rfprsnlbnftind",        # 6a — received funds for premiums (matches 990)
    "pyprsnlbnftind",        # 6b — paid premiums personal benefit (matches 990)
    "s4960_tx_pymt_cd",      # 8 — s4960 excess compensation tax
    # Part XIII — Private Operating Foundations (4-year history: curr_yr / prior1-3 / 4yr_total)
    "adjnetinccola", "adjnetinccolb", "adjnetinccolc", "adjnetinccold", "adjnetinctot",   # 2a
    "qlfydistriba", "qlfydistribb", "qlfydistribc", "qlfydistribd", "qlfydistribtot",     # 2e
    "valassetscola", "valassetscolb", "valassetscolc", "valassetscold", "valassetstot",   # 3a(1)
    "qlfyasseta", "qlfyassetb", "qlfyassetc", "qlfyassetd", "qlfyassettot",               # 3a(2)
    "endwmntscola", "endwmntscolb", "endwmntscolc", "endwmntscold", "endwmntstot",        # 3b
    "totsuprtcola", "totsuprtcolb", "totsuprtcolc", "totsuprtcold", "totsuprttot",        # 3c(1)
    "pubsuprtcola", "pubsuprtcolb", "pubsuprtcolc", "pubsuprtcold", "pubsuprttot",        # 3c(2)
    "grsinvstinca", "grsinvstincb", "grsinvstincc", "grsinvstincd", "grsinvstinctot",     # 3c(4)
    # Part V — Excise Tax computations
    "crelamt",        # 11 — credit elect amount
    "erronbkupwthld", # 6d — erroneous backup withholding credit
    "estpnlty",       # 8 — estimated tax penalty
    "esttaxcr",       # 6a — estimated tax credit
    "invstexcisetx",  # 1 — 4940 excise tax on net investment income
    "overpay",        # 10 — overpayment
    "sect511tx",      # 2 — section 511 (UBI) tax
    "subtitleatx",    # 4 — subtitle A tax
    "taxdue",         # 9 — tax due
    "totaxpyr",       # 5 — total excise tax (sum of lines 1+2+4)
    "txpaidf2758",    # 6c — tax paid with Form 2758 (extension)
    "txwithldsrc",    # 6b — tax withheld at source
    # Part XV-A — Analysis of Income-Producing Activities (col d=excluded, col e=exempt)
    "progsrvcacold","progsrvcacole","progsrvcbcold","progsrvcbcole",
    "progsrvcccold","progsrvcccole","progsrvcdcold","progsrvcdcole",
    "progsrvcecold","progsrvcecole","progsrvcfcold","progsrvcfcole",
    "progsrvcgcold","progsrvcgcole",
    "membershpduesd","membershpduese",
    "intonsvngsd","intonsvngse",
    "dvdndsintd","dvdndsinte",
    # Part XVI — Relationship with Noncharitable Exempt Organizations
    "loansguarcd",     # 1b(5)
    "perfservicescd",  # 1b(6)
    "prchsasstscd",    # 1b(2)
    "reimbrsmntscd",   # 1b(4)
    "rentlsfacltscd",  # 1b(3)
    "salesasstscd",    # 1b(1)
    "sharngasstscd",   # 1c
    "trnsfrcashcd",    # 1a(1)
    "trnsothasstscd",  # 1a(2)
    # Singletons (Parts IV/IX/X/XII/XIV/VII)
    "distribamt",      # Pt X-7 — distributable amount
    "grntapprvfut",    # Pt XIV-3b — grants approved for future
    "tfairmrktunuse",  # Pt IX-1d — FMV assets not for charitable use
    "totexcapgnls",    # Pt IV-2 — capital gain net income
    "valncharitassets",# Pt IX-5 — net value noncharitable assets
    "distribdafcd",    # Pt VII-A-12 — DAF distribution (synonym of acqdrindrintcd)
    # Header / Other
    "dividndsamt",     # Pt I-4 col a — dividends
    "grscontrgifts",   # Pt I-1 col a — contributions received
    "totexcapgn",      # Pt IV-2 > 0 — capital gain
    "totexcapls",      # Pt IV-2 < 0 — capital loss
    "assetcdgen",      # IRS-generated — asset-size category code
    "balduopt",        # Pt IV-9/10 — balance due or overpayment
    "transinccd"       # IRS-generated — total revenue size code
  ),
  harmonized_name = c(
    "total_operating_admin_expenses_col_a",
    "total_operating_admin_expenses_col_b",
    "total_operating_admin_expenses_col_d",
    "total_expenses_col_a",
    "total_expenses_col_b",
    "total_expenses_col_c",
    "total_expenses_col_d",
    "total_revenue_col_a",
    "total_revenue_col_b",
    "total_revenue_col_c",
    "excess_revenue_over_expenses",
    # Part II
    "fair_mkt_value_total_assets_eoy",
    "fair_mkt_value_total_assets_eoy",
    "mortgage_loans_eoy",
    "mortgage_notes_payable_eoy",
    "other_assets_eoy",
    "noninterest_cash_eoy",
    "other_investments_eoy",
    "other_liabilities_eoy",
    "total_fund_net_worth_eoy",
    # Part VI-A
    "influence_legislation_cd",
    "activities_not_prev_reported_cd",
    "changes_to_governing_docs_cd",
    "liquidated_terminated_cd",
    "furnished_copy_to_ag_cd",
    "claiming_private_op_status_cd",
    "new_substantial_contributors_cd",
    "distribution_to_daf_advisory_cd",
    "complied_public_inspection_cd",
    "filed_in_lieu_1041_cd",
    # Part VI-B
    "dqp_property_exchange_cd",
    "dqp_borrow_lend_cd",
    "dqp_furnish_goods_cd",
    "dqp_paid_compensation_cd",
    "dqp_transfer_assets_cd",
    "agreed_pay_govt_official_cd",
    "dqp_acts_failed_to_qualify_cd",
    "dqp_acts_prior_year_cd",
    "undistributed_income_cd",
    "not_applying_4942_provisions_cd",
    "direct_indirect_interest_2pct_cd",
    "excess_business_holdings_cd",
    "jeopardizing_investments_cd",
    "prior_year_jeopardizing_invest_cd",
    "s4945_propaganda_legislation_cd",
    "s4945_influence_election_cd",
    "s4945_grant_to_indiv_cd",
    "s4945_grant_to_non_charity_cd",
    "s4945_non_religious_purpose_cd",
    "s4945_transactions_failed_cd",
    "received_funds_for_premiums_cd",
    "paid_premiums_personal_benefit_cd",
    "s4960_excess_comp_tax_cd",
    # Part XIII
    "adjusted_net_income_curr_yr","adjusted_net_income_prior1","adjusted_net_income_prior2","adjusted_net_income_prior3","adjusted_net_income_4yr_total",
    "qualifying_distributions_curr_yr","qualifying_distributions_prior1","qualifying_distributions_prior2","qualifying_distributions_prior3","qualifying_distributions_4yr_total",
    "value_assets_curr_yr","value_assets_prior1","value_assets_prior2","value_assets_prior3","value_assets_4yr_total",
    "qualifying_assets_curr_yr","qualifying_assets_prior1","qualifying_assets_prior2","qualifying_assets_prior3","qualifying_assets_4yr_total",
    "endowments_curr_yr","endowments_prior1","endowments_prior2","endowments_prior3","endowments_4yr_total",
    "total_support_curr_yr","total_support_prior1","total_support_prior2","total_support_prior3","total_support_4yr_total",
    "public_support_curr_yr","public_support_prior1","public_support_prior2","public_support_prior3","public_support_4yr_total",
    "gross_investment_income_curr_yr","gross_investment_income_prior1","gross_investment_income_prior2","gross_investment_income_prior3","gross_investment_income_4yr_total",
    # Part V
    "credit_elect_amount",
    "erroneous_backup_withholding_credit",
    "estimated_tax_penalty",
    "estimated_tax_credit",
    "excise_tax_net_investment_income",
    "overpayment",
    "section_511_tax",
    "subtitle_a_tax",
    "tax_due",
    "total_excise_tax",
    "tax_paid_with_form_2758",
    "tax_withheld_at_source",
    # Part XV-A (10 categories x 2 cols)
    "program_revenue_1a_excluded","program_revenue_1a_exempt_function",
    "program_revenue_1b_excluded","program_revenue_1b_exempt_function",
    "program_revenue_1c_excluded","program_revenue_1c_exempt_function",
    "program_revenue_1d_excluded","program_revenue_1d_exempt_function",
    "program_revenue_1e_excluded","program_revenue_1e_exempt_function",
    "program_revenue_1f_excluded","program_revenue_1f_exempt_function",
    "program_revenue_1g_excluded","program_revenue_1g_exempt_function",
    "membership_dues_excluded","membership_dues_exempt_function",
    "interest_savings_excluded","interest_savings_exempt_function",
    "dividends_interest_excluded","dividends_interest_exempt_function",
    # Part XVI
    "nce_org_loans_or_guarantees_cd",
    "nce_org_perform_services_cd",
    "nce_org_purchase_assets_cd",
    "nce_org_reimbursements_cd",
    "nce_org_rental_facilities_cd",
    "nce_org_sale_assets_cd",
    "nce_org_sharing_assets_cd",
    "nce_org_transfer_cash_cd",
    "nce_org_transfer_other_assets_cd",
    # Singletons
    "distributable_amount",
    "grants_approved_for_future",
    "fair_mkt_value_assets_not_charitable_use",
    "capital_gain_net_income",
    "net_value_noncharitable_assets",
    "distribution_to_daf_advisory_cd",
    # Header / Other
    "dividends",
    "contributions_received",
    "capital_gain",
    "capital_loss",
    "asset_size_code",
    "balance_due_or_overpayment",
    "total_revenue_size_code"
  )
)
stopifnot(length(overrides$source_var) == length(overrides$harmonized_name))
if (nrow(overrides) > 0) {
  # Apply overrides case-insensitively (PF has lower+UPPER casings of same var)
  overrides[, key := tolower(source_var)]
  src[, key := tolower(source_var)]
  src[overrides, on = "key", harmonized_name := i.harmonized_name]
  src[, key := NULL]
  overrides[, key := NULL]
}

# ---- Output ----
out <- src[, .(source_var, harmonized_name, description, location, years_present, source)]

baseline_path <- "data/crosswalks/soi_990pf_crosswalk_BASELINE.csv"
overrides_path <- "data/crosswalks/soi_990pf_crosswalk_OVERRIDES.csv"
final_path <- "data/crosswalks/soi_990pf_crosswalk_FINAL.csv"

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

# Synonym check (case rename pairs collapsed)
final[, key := tolower(source_var)]
syn <- final[, .N, by = key][N > 1]
cat("\n=== source_vars that differ only by case (synonyms — should map to same harmonized name) ===\n")
cat("count:", nrow(syn), "\n")
if (nrow(syn) > 0) {
  print(final[key %in% syn$key, .(source_var, harmonized_name)][order(tolower(source_var))][1:30])
}
