suppressPackageStartupMessages({
  library(data.table)
})

v <- fread("data/raw/soi_dictionaries/_all_variables.csv")
v990 <- v[form == "990"]

# ---- Build years-present per source var ----
# Range-collapse a sorted integer vector with gaps preserved.
# e.g. c(2012:2016, 2020:2024) -> "2012-2016, 2020-2024"; c(2015) -> "2015"
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

# Use the most recent year's description/location as canonical (IRS reorders /
# re-labels rows between years; latest year is most reliable).
setorder(v990, variable_name, -year)
src <- v990[, .(years_present = collapse_years(year),
                description = first(description),
                location = first(location)),
            by = .(variable_name)]
setnames(src, "variable_name", "source_var")
setorder(src, source_var)

# ---- Algorithmic abbreviation expansion ----
# Order matters: longest patterns first to avoid partial matches.
expansions <- list(
  c("compnsatn",      "compensation"),
  c("compnsat",       "compensation"),
  c("compns",         "compensation"),
  c("compsn",         "compensation"),
  c("totreprtable",   "total_reportable"),
  c("totcomprelated", "total_comp_related"),
  c("totestcomp",     "total_estimated_comp"),
  c("rcvbl",          "receivable"),
  c("payable",        "payable"),
  c("payble",         "payable"),
  c("pyabl",          "payable"),
  c("paybleto",       "payable_to"),
  c("paidinsurplus",  "paid_in_surplus"),
  c("retainedearn",   "retained_earnings"),
  c("capitalstktrst", "capital_stock_or_trust"),
  c("permrstrct",     "permanently_restricted"),
  c("temprstrct",     "temporarily_restricted"),
  c("unrstrct",       "unrestricted"),
  c("netasstsend",    "net_assets_eoy"),
  c("netasset",       "net_assets"),
  c("netliabast",     "net_liab_assets"),
  c("totnetliabast",  "total_liab_and_net_assets"),
  c("totliab",        "total_liabilities"),
  c("totasset",       "total_assets"),
  c("intangibleassets","intangible_assets"),
  c("invstmnts",      "investments"),
  c("invstmnt",       "investment"),
  c("invntries",      "inventories"),
  c("prepaidexpns",   "prepaid_expenses"),
  c("notesloans",     "notes_and_loans"),
  c("rcvbldisqual",   "receivables_disqualified"),
  c("currfrmrcvbl",   "officer_receivables"),
  c("accntsrcvbl",    "accounts_receivable"),
  c("pldgegrntrcvbl", "pledges_grants_receivable"),
  c("svngstempinv",   "savings_and_temp_investments"),
  c("nonintcash",     "noninterest_cash"),
  c("escrwaccntliab", "escrow_account_liability"),
  c("escrwaccnt",     "escrow_account"),
  c("secrdmrtgs",     "secured_mortgages"),
  c("unsecurednotes", "unsecured_notes"),
  c("grntspayable",   "grants_payable"),
  c("deferedrevnue",  "deferred_revenue"),
  c("accntspayable",  "accounts_payable"),
  c("lndbldgsequip",  "land_bldg_equip"),
  c("lndbldgeqpt",    "land_bldg_equip"),
  c("txexmptbnds",    "tax_exempt_bonds"),
  c("txexmptbnd",     "tax_exempt_bond"),
  c("txexmpt",        "tax_exempt"),
  c("txrevnue",       "tax_revenues"),
  c("totfuncexpns",   "total_functional_expenses"),
  c("totfunc",        "total_functional"),
  c("deprcatndepletn","depreciation_depletion"),
  c("pymtoaffiliates","payments_to_affiliates"),
  c("pymtto",         "payment_to"),
  c("interestamt",    "interest_expense"),
  c("converconventmtng","conferences_conventions_meetings"),
  c("travelofpublicoffcl","travel_public_officials"),
  c("officexpns",     "office_expenses"),
  c("infotech",       "information_technology"),
  c("advrtpromo",     "advertising_promotion"),
  c("royaltsexpns",   "royalties_expense"),
  c("royaltsinc",     "royalties_income"),
  c("royalts",        "royalties"),
  c("feesforsrvc",    "fees_for_services"),
  c("feesforsrv",     "fees_for_services"),
  c("profndraising",  "professional_fundraising_fees"),
  c("accntingfees",   "accounting_fees"),
  c("legalfees",      "legal_fees"),
  c("payrolltx",      "payroll_taxes"),
  c("pensionplancontrb","pension_plan_contributions"),
  c("othremplyeebenef","other_employee_benefits"),
  c("othrsalwages",   "other_salaries_wages"),
  c("benifitsmembrs", "benefits_to_members"),
  c("benefits",       "benefits"),
  c("grntstofrgngovt","grants_to_foreign_govt"),
  c("grnsttoindiv",   "grants_to_individuals"),
  c("grntstogovt",    "grants_to_govt"),
  c("grntstoindv",    "grants_to_individuals"),
  c("grntstogv",      "grants_to_govt"),
  c("totrevenue",     "total_revenue"),
  c("totrev",         "total_revenue"),
  c("totprgmrevnue",  "program_service_revenue"),  # aligned with 990-EZ for 990summary stacking
  c("prgmservcode",   "prgm_serv_code"),
  c("prgmserv",       "prgm_serv"),
  c("miscrev",        "misc_revenue"),
  c("netincsales",    "net_income_sales"),
  c("netincgaming",   "net_income_gaming"),
  c("netincfndrsng",  "net_income_fundraising"),
  c("netrntlinc",     "net_rental_income"),
  c("netincunreltd",  "net_income_unrelated"),
  c("netincunrelatd", "net_income_unrelated"),
  c("grsincgaming",   "gross_income_gaming"),
  c("grsincfndrsng",  "gross_income_fundraising"),
  c("grsincmembers",  "gross_income_members"),
  c("grsincother",    "gross_income_other"),
  c("grsinc",         "gross_income"),
  c("grsales",        "gross_sales"),
  c("grsalesinvent",  "gross_sales_inventory"),
  c("lessdirfndrsng", "fundraising_expenses_direct"),
  c("lessdirgaming",  "gaming_expenses_direct"),
  c("lesscstofgoods", "cost_of_goods_sold"),
  c("cstbasis",       "cost_basis"),
  c("grsrnts",        "gross_rents"),
  c("rntlexpns",      "rental_expense"),
  c("rntlinc",        "net_rent"),
  # securities/other/personal/real_estate suffix expansions for paired columns
  c("grsalesecur",    "gross_sales_securities"),
  c("grsalesothr",    "gross_sales_other"),
  c("cstbasisecur",   "cost_basis_securities"),
  c("cstbasisothr",   "cost_basis_other"),
  c("gnlsecur",       "net_gain_loss_securities"),
  c("gnlsothr",       "net_gain_loss_other"),
  c("grsrntsreal",    "gross_rents_real_estate"),
  c("grsrntsprsnl",   "gross_rents_personal"),
  c("rntlexpnsreal",  "rental_expense_real_estate"),
  c("rntlexpnsprsnl", "rental_expense_personal"),
  c("rntlincreal",    "net_rent_real_estate"),
  c("rntlincprsnl",   "net_rent_personal"),
  # netgnls handled before gnls to prevent the double-prefix bug
  c("netgnls",        "net_gain_loss_sales_total"),
  c("gnls",           "net_gain_loss_sales"),
  c("totcntrbgfts",   "total_contributions"),
  c("solicitcntrb",   "solicit_contributions"),
  c("noindiv100k",    "n_indiv_over_100k"),
  c("nocontractor100k","n_contractors_over_100k"),
  c("noemplyeesw3",   "n_employees_w3"),
  c("filerqrdrtns",   "filed_employment_tax_returns"),
  c("unrelbusinc",    "unrelated_business_income"),
  c("filedf",         "filed_form_"),
  c("frgnacct",       "foreign_account"),
  c("frgnoffice",     "foreign_office"),
  c("frgnrevexpns",   "foreign_rev_expns"),
  c("frgnaggragrnts", "foreign_aggr_grants"),
  c("frgngrnts",      "foreign_grants"),
  c("frgn",           "foreign"),
  c("prohibtdtxshltr","prohibited_tax_shelter"),
  c("prtynotifyorg",  "party_notify_org"),
  c("providegoods",   "provide_goods"),
  c("notfydnrval",    "notify_donor_value"),
  c("solicit",        "solicit"),
  c("exprstmnt",      "express_statement"),
  c("fndsrcvd",       "funds_received"),
  c("premiumspaid",   "premiums_paid"),
  c("excbushldngs",   "excess_business_holdings"),
  c("distribtodonor", "distribution_to_donor"),
  c("s4966distrib",   "s4966_distributions"),
  c("initiationfees", "initiation_fees"),
  c("grsrcptspublicuse","gross_receipts_public_use"),
  c("grsrcptsrelatd", "gross_receipts_related"),
  c("grsrcptsrelated","gross_receipts_related"),
  c("grsrcptsadmiss", "gross_receipts_admissions"),
  c("grsrcptsadmissn","gross_receipts_admissions"),
  c("grsrcptsactivities","gross_receipts_activities"),
  c("grsrcpts",       "gross_receipts"),
  c("filedlieuf1041", "filed_in_lieu_1041"),
  c("qualhlthplncd",  "qualified_health_plan_ind"),
  c("qualhlthreqmntn","qualified_health_plan_reserves_required"),
  c("qualhlthonhnd",  "qualified_health_plan_reserves_on_hand"),
  c("rcvdpdtng",      "received_pmt_indoor_tanning"),
  c("filedf720",      "filed_form_720"),
  c("rptlndbldgeqpt", "rpt_land_bldg_equip"),
  c("rptinvstothse",  "rpt_invst_other_sec"),
  c("rptinvstprgrel", "rpt_invst_prog_related"),
  c("rptothasst",     "rpt_other_assets"),
  c("rptothliab",     "rpt_other_liab"),
  c("rptprofndrsngfees","rpt_prof_fundraising_fees"),
  c("rptincfnndrsng", "rpt_inc_fundraising"),
  c("rptincgaming",   "rpt_inc_gaming"),
  c("rptgrntstogovt", "rpt_grants_to_govt"),
  c("rptgrntstoindv", "rpt_grants_to_indv"),
  c("rptyestocompnstn","rpt_yes_to_comp"),
  c("politicalactvts","political_activities"),
  c("lbbyingactvts",  "lobbying_activities"),
  c("subjto6033",     "subject_to_6033"),
  c("dnradvisedfunds","donor_advised_funds"),
  c("prptyintrcvd",   "property_int_received"),
  c("maintwrkofart",  "maintain_works_of_art"),
  c("crcounselingqstn","credit_counseling"),
  c("hldassetsintermperm","hold_assets_term_or_perm"),
  c("sepcnsldtfinstmt","sep_consol_fin_stmt"),
  c("sepindaudfinstmt","sep_indep_audited_fin_stmt"),
  c("inclinfinstmt",  "incl_in_consol_fin_stmt"),
  c("operateschools170","operate_schools_170"),
  c("operatehosptl",  "operate_hospital"),
  c("hospaudfinstmt", "hosp_audited_fin_stmt"),
  c("invstproceeds",  "invst_proceeds"),
  c("maintescrwaccnt","maintain_escrow_account"),
  c("actonbehalf",    "act_on_behalf"),
  c("engageexcessbnft","engage_excess_benefit"),
  c("awarexcessbnft", "aware_excess_benefit"),
  c("loantofficer",   "loan_to_officer"),
  c("grantoofficer",  "grant_to_officer"),
  c("dirbusnreltd",   "dir_business_related"),
  c("fmlybusnreltd",  "family_business_related"),
  c("servasofficer",  "serv_as_officer"),
  c("recvnoncash",    "recv_noncash"),
  c("recvart",        "recv_art"),
  c("ceaseoperations","cease_operations"),
  c("sellorexch",     "sell_or_exchange"),
  c("ownsepent",      "own_sep_entity"),
  c("reltdorg",       "related_org"),
  c("intincntrl",     "interest_in_control"),
  c("orgtrnsfr",      "org_transfer"),
  c("conduct5percent","conduct_5_percent"),
  c("compltscho",     "complete_schedule_o"),
  c("wthldngrules",   "withholding_rules"),
  c("ceofficer",      "ce_officer"),
  c("schdbind",       "sched_b_required_ind"),
  c("s501c3or4947a1", "s501c3_or_4947a1"),
  c("subseccd",       "subsection_cd"),
  c("invstmntinc",    "investment_income"),
  c("totnetasset",    "total_net_assets"),
  c("totnooforgs",    "n_orgs_supported"),
  c("totsupport",     "total_support_amount"),
  c("totsupp",        "total_support"),
  c("totgftgrntrcvd", "total_gifts_grants_received"),
  c("gftgrntsrcvd",   "gifts_grants_received"),
  c("rcvdfrmdisqualsub","received_from_disqual_subtot"),
  c("samepubsuppsubtot","same_pub_support_subtotal"),
  c("pubsuppsubtot",  "pub_support_subtotal"),
  c("pubsupplesub",   "pub_support_less"),
  c("pubsupplesspct", "pub_support_less_pct"),
  c("subtotpub",      "subtotal_public"),
  c("subtotsuppinc",  "subtotal_support_income"),
  c("unreltxincls511tx","unrel_tax_inc_less_511_tax"),
  c("exceeds2pct",    "exceeds_2_pct"),
  c("exceeds1pct",    "exceeds_1_pct"),
  c("srvcsval",       "services_value"),
  c("othrinc",        "other_income"),
  c("othrexpns",      "other_expense"),
  c("othrassets",     "other_assets"),
  c("othrliab",       "other_liabilities"),
  c("othrev",         "other_revenue"),
  c("totrev",         "total_rev"),
  c("nonpfrea",       "nonpf_reason"),
  c("efile",          "efile_indicator"),
  c("eostatus",       "eo_status"),
  c("end",            "_eoy"),  # suffix for end-of-year
  c("cnt",            "_count"),
  c("cd",             "_cd"),
  c("ein",            "ein"),
  c("EIN",            "ein"),
  c("tax_pd",         "tax_period")
)

normalize <- function(x) {
  s <- tolower(x)
  for (e in expansions) s <- gsub(e[1], paste0("_", e[2], "_"), s, fixed = TRUE)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  # Word-boundary cleanup of residual short abbreviations
  cleanup <- list(
    c("othr",   "other"),
    c("prsnl",  "personal"),
    c("ecur",   "securities"),
    c("expns",  "expenses"),
    c("inc",    "income"),
    c("int",    "interest"),
    c("invent", "inventory"),
    c("hosp",   "hospital"),
    c("invst",  "investment")
  )
  for (e in cleanup) {
    pat <- sprintf("(^|_)%s(_|$)", e[1])
    s <- gsub(pat, sprintf("\\1%s\\2", e[2]), s, perl = TRUE)
  }
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}

src[, harmonized_name := vapply(source_var, normalize, character(1))]

# ---- Description-driven overrides ----
# Each entry below was hand-verified against the IRS dictionary description.
# Format: source_var, harmonized_name, # description as comment for traceability
overrides <- data.table(
  source_var = c(
    # Synonyms (IRS renamed across vintages)
    "elf",                  # E-file indicator (2015-2019)
    "efile",                # E-file indicator (2020-2024)
    # Form 990 Part IV checklist — algorithmic name didn't match description
    "prptyintrcvdcd",       # description: Conservation easements?
    "intincntrlcd",         # description: Related organization a controlled entity?
    "rptyestocompnstncd",   # description: Schedule J required?
    "schdbind",             # description: Schedule B required?  (was _ind, normalize to _cd)
    "qualhlthplncd",        # description: Qualified health plan in multiple states
    "frgngrntscd",          # description: More than $5000 to organizations Part IX, line 3
    "frgnaggragrntscd",     # description: More than $5000 to individuals Part IX, line 3
    "frgnrevexpnscd",       # description: Foreign activities, etc?
    # Counts (drop double prefix/suffix)
    "totnooforgscnt",       # description: Number of organizations supported
    "noindiv100kcnt",       # description: Number individuals greater than $100K
    "nocontractor100kcnt",  # description: Number of contractors greater than $100K
    "noemplyeesw3cnt",      # description: Number of employees
    "f1096cnt",             # description: Number forms transmitted with 1096
    "f8282cnt",             # description: Number of 8282s filed
    "fw2gcnt",              # description: Number W-2Gs included in 1a
    # Part VII compensation (column letters in source name resolved via description)
    "totreprtabled",        # description: Reportable compensation from organization
    "totcomprelatede",      # description: Reportable compensation from related orgs
    # totestcompf moved to Audited financials block below for form-text precision
    # Part IX line 24a-f (other expenses)
    "othrexpnsa",           # description: Other expenses 24a
    "othrexpnsb",           # description: Other expenses 24b
    "othrexpnsc",           # description: Other expenses 24c
    "othrexpnsd",           # description: Other expenses 24d
    "othrexpnse",           # description: Other expenses 24e
    "othrexpnsf",           # description: Other expenses 24f
    # Part VIII program revenue 2a-f (paired code/amount per line)
    "prgmservcode2acd",     # description: Program service revenue code 2a
    "prgmservcode2bcd",     # description: Program service revenue code 2b
    "prgmservcode2ccd",     # description: Program service revenue code 2c
    "prgmservcode2dcd",     # description: Program service revenue code 2d
    "prgmservcode2ecd",     # description: Program service revenue code 2e
    "totrev2acola",         # description: Program service revenue amount 2a
    "totrev2bcola",         # description: Program service revenue amount 2b
    "totrev2ccola",         # description: Program service revenue amount 2c
    "totrev2dcola",         # description: Program service revenue amount 2d
    "totrev2ecola",         # description: Program service revenue amount 2e
    "totrev2fcola",         # description: Program service revenue amount 2f
    # Part VIII line 11a-e (other revenue, paired code/amount)
    "miscrev11acd",         # description: Other revenue code 11a
    "miscrev11bcd",         # description: Other revenue code 11b
    "miscrev11ccd",         # description: Other revenue code 11c
    "miscrevtota",          # description: Other revenue amount 11a
    "miscrevtot11b",        # description: Other revenue amount 11b
    "miscrevtot11c",        # description: Other revenue amount 11c
    "miscrevtot11d",        # description: Other revenue amount 11d
    "miscrevtot11e",        # description: Other revenue (line 11e total)
    # Part X balance sheet (description-verified)
    "accntspayableend",     # description: Accounts payable and accrued expenses -- eoy
    "accntsrcvblend",       # description: Accounts receivable -- eoy
    "currfrmrcvblend",      # description: Receivables from officers, directors, etc. -- eoy
    "deferedrevnuend",      # description: Deferred revenue -- eoy
    "grntspayableend",      # description: Grants payable -- eoy
    "invntriesalesend",     # description: Inventories for sale or use -- eoy
    "invstmntsprgmend",     # description: Program-related investments -- eoy
    "paybletoffcrsend",     # description: Payables to officers, directors, etc. -- eoy
    "pldgegrntrcvblend",    # description: Pledges and grants receivable -- eoy
    "rcvbldisqualend",      # description: Receivables from disqualified persons -- eoy
    "totassetsend",         # description: Total assets -- eoy
    "totnetassetend",       # description: Total Net Assets -- eoy
    "totnetliabastend",     # description: Total Liabilities + Net Assets -- eoy
    # Schedule A Part II — sec170 public support test
    "exceeds2pct170",       # description: Amount support exceeds total (170)
    "grsinc170",            # description: Gross income from interest etc (170)
    "grsrcptsrelated170",   # description: Gross receipts from related activities (170)
    "netincunreltd170",     # description: Net UBI (170)
    "pubsupplesspct170",    # description: Public support (170)  [Part II-6f, final]
    "pubsuppsubtot170",     # description: Public support subtotal (170)
    "samepubsuppsubtot170", # description: Public support from line 4 (170)
    "srvcsval170",          # description: Services or facilities furnished by gov (170)
    # Schedule A Part III — sec509 public support test
    "exceeds1pct509",       # description: Amount support exceeds total (509)
    "grsinc509",            # description: Gross income from interest etc (509)
    "grsrcptsactivities509",# description: Gross receipts from related activities (509)
    "grsrcptsadmissn509",   # description: Receipts from admissions merchandise etc (509)
    "netincunrelatd509",    # description: Net income from UBI not in 10b (509)
    "pubsupplesub509",      # description: Public support (509)
    "pubsuppsubtot509",     # description: Public support subtotal (509)  [Part III-6f]
    "rcvdfrmdisqualsub509", # description: Amounts from disqualified persons (509)
    "samepubsuppsubtot509", # description: Public support from line 6 (509)
    "srvcsval509",          # description: Services or facilities furnished by gov (509)
    "subtotpub509",         # description: Public support subtotal (509)  [Part III-7c, distinct]
    "subtotsuppinc509",     # description: Subtotal total support (509)
    "unreltxincls511tx509", # description: Net UBI (509)  [Part III-10b]
    # Part IV checklist — Group A (form-text-verified against Form 990 PDF)
    "actonbehalfcd",        # form 24d: act as 'on behalf of' issuer for bonds
    "awarexcessbnftcd",     # form 25b: aware of prior-year excess benefit transaction
    "ceaseoperationscd",    # form 31: liquidated, terminated, dissolved (idiom: terminated)
    "conduct5percentcd",    # form 37: >5% activities through non-related partnership
    "engageexcessbnftcd",   # form 25a: engaged in excess benefit transaction
    "grantoofficercd",      # form 27: grant to officer/director/key employee/substantial contributor (related person)
    "hldassetsintermpermcd",# form 10: donor-restricted endowments or quasi-endowments
    "invstproceedscd",      # form 24b: invested tax-exempt bond proceeds beyond temp period
    "loantofficercd",       # form 26: receivables/payables to interested persons (Sch L Pt II)
    "maintwrkofartcd",      # form 8: collections of art, historical treasures, or similar
    "orgtrnsfrcd",          # form 36: transfer to exempt non-charitable related org
    "ownsepentcd",          # form 33: own 100% disregarded entity
    "recvartcd",            # form 30: contributions of art, historical treasures, or qualified conservation
    "recvnoncashcd",        # form 29: noncash contributions >$25K
    "rptgrntstogovtcd",     # form 21: >$5K grants to domestic orgs or govt
    "rptincfnndrsngcd",     # form 18: fundraising events gross income+contributions >$15K
    "rptincgamingcd",       # form 19: gaming gross income >$15K
    "sellorexchcd",         # form 32: sold/exchanged/disposed >25% net assets (idiom: partial liquidation)
    "servasofficercd",      # form 28c: business txn with 35%-controlled entity of related party
    "subjto6033cd",         # form 5: 501(c)(4)/(5)/(6) with dues (idiom: subject to proxy tax)
    # Part IV checklist — Group B style improvements
    "compltschocd",         # description: Schedule O completed?
    "dirbusnreltdcd",       # description: Business relationship with organization?
    "fmlybusnreltdcd",      # description: Business relationship thru family member?
    "maintescrwaccntcd",    # description: Escrow account?
    "operatehosptlcd",      # description: Hospital?
    "operateschools170cd",  # description: School?  (Sec 170 schools test)
    "reltdorgcd",           # description: Related entity?
    "rptinvstothsecd",      # description: Investments in other securities reported?
    "rptinvstprgrelcd",     # description: Program related investments reported?
    "rptothliabcd",         # description: Other liabilities reported?
    # Part V — form-text-verified
    "wthldngrulescd",       # form 1c: backup withholding compliance
    "unrelbusinccd",        # form 3a: UBI >= $1,000
    "prtynotifyorgcd",      # form 5b: taxable party notified org of prohibited tax shelter
    "solicitcntrbcd",       # form 6a: solicit non-deductible contributions
    "exprstmntcd",          # form 6b: express statement that gifts not deductible
    "providegoodscd",       # form 7a: quid pro quo contributions (>$75 partly goods)
    "filedf8282cd",         # form 7c: disposed of property that required Form 8282
    "fndsrcvdcd",           # form 7e: received funds for premiums on personal benefit contract
    "premiumspaidcd",       # form 7f: paid premiums on personal benefit contract
    # Part VIII — form-text-verified
    "grsincfndrsng",        # form 8a: gross income from fundraising events
    "grsincgaming",         # form 9a: gross income from gaming activities
    "netincfndrsng",        # form 8c: net income from fundraising events
    "netincgaming",         # form 9c: net income from gaming activities
    "netincsales",          # form 10c: net income from sales of inventory
    "lessdirfndrsng",       # form 8b: less direct expenses (fundraising events)
    "lessdirgaming",        # form 9b: less direct expenses (gaming activities)
    "netgnls",              # form 7d: net gain or loss on sales of assets
    # Part IX functional expenses (description-verified)
    "compnsatnandothr",     # description: Compensation of disqualified persons
    "compnsatncurrofcr",    # description: Compensation of current officers, directors, etc
    "feesforsrvcinvstmgmt", # description: Investment management fees
    "feesforsrvclobby",     # description: Lobbying fees
    "feesforsrvcmgmt",      # description: Management fees
    # Audited financials & FIN 48 (form-text-verified, IV-11f/12a/12b/20b)
    "sepcnsldtfinstmtcd",   # form 11f: FIN 48 uncertain tax position footnote
    "sepindaudfinstmtcd",   # form 12a: obtained separate independent audited
    "inclinfinstmtcd",      # form 12b: included in consolidated independent audited
    "hospaudfinstmtcd",     # form 20b: hospital audited financials attached
    "totestcompf",          # form VII-A col F: estimated amount of other compensation
    # Other description-driven fixes
    "nonpfrea",             # description: Reason for non-PF status
    "interestamt"           # description: Interest expense
  ),
  harmonized_name = c(
    "efile_indicator",
    "efile_indicator",
    "conservation_easements_cd",
    "related_org_controlled_cd",
    "sched_j_required_cd",
    "sched_b_required_cd",
    "qualified_health_plan_multi_state_cd",
    "foreign_grants_orgs_5k_cd",
    "foreign_grants_indiv_5k_cd",
    "foreign_activities_cd",
    "n_orgs_supported",
    "n_indiv_over_100k",
    "n_contractors_over_100k",
    "n_employees_w3",
    "n_form_1096",
    "n_form_8282",
    "n_form_w2g",
    "total_reportable_comp_from_org",
    "total_reportable_comp_from_related",
    # totestcompf -> total_estimated_other_compensation moved to Audited block
    "other_expenses_24a",
    "other_expenses_24b",
    "other_expenses_24c",
    "other_expenses_24d",
    "other_expenses_24e",
    "other_expenses_24f",
    "program_revenue_code_2a",
    "program_revenue_code_2b",
    "program_revenue_code_2c",
    "program_revenue_code_2d",
    "program_revenue_code_2e",
    "program_revenue_amount_2a",
    "program_revenue_amount_2b",
    "program_revenue_amount_2c",
    "program_revenue_amount_2d",
    "program_revenue_amount_2e",
    "program_revenue_amount_2f",
    "other_revenue_code_11a",
    "other_revenue_code_11b",
    "other_revenue_code_11c",
    "other_revenue_amount_11a",
    "other_revenue_amount_11b",
    "other_revenue_amount_11c",
    "other_revenue_amount_11d",
    "other_revenue_total_11e",
    # Part X balance sheet
    "accounts_payable_eoy",
    "accounts_receivable_eoy",
    "officer_receivables_eoy",
    "deferred_revenue_eoy",
    "grants_payable_eoy",
    "inventories_for_sale_eoy",
    "program_related_investments_eoy",
    "payable_to_officers_eoy",
    "pledges_grants_receivable_eoy",
    "receivables_disqualified_eoy",
    "total_assets_eoy",
    "total_net_assets_eoy",
    "total_liab_and_net_assets_eoy",
    # Schedule A Part II — sec170
    "excess_over_2pct_sec170",
    "gross_income_interest_sec170",
    "gross_receipts_related_activities_sec170",
    "net_ubi_sec170",
    "public_support_sec170",
    "public_support_subtotal_sec170",
    "public_support_subtotal_line4_ref_sec170",
    "gov_services_value_sec170",
    # Schedule A Part III — sec509
    "excess_over_1pct_sec509",
    "gross_income_interest_sec509",
    "gross_receipts_related_activities_sec509",
    "gross_receipts_admissions_sec509",
    "net_ubi_excl_10b_sec509",
    "public_support_sec509",
    "public_support_subtotal_line6_sec509",
    "disqualified_persons_amts_sec509",
    "public_support_subtotal_line6_ref_sec509",
    "gov_services_value_sec509",
    "public_support_subtotal_line7c_sec509",
    "total_support_subtotal_sec509",
    "net_ubi_line10b_sec509",
    # Part IV checklist — Group A (form-text-verified)
    "act_on_behalf_of_issuer_cd",
    "aware_prior_excess_benefit_cd",
    "terminated_cd",
    "partnership_activities_5pct_cd",
    "excess_benefit_transaction_cd",
    "grant_to_related_person_cd",
    "donor_restricted_endowments_cd",
    "bond_proceeds_invested_cd",
    "loan_to_interested_person_cd",
    "art_historical_collections_cd",
    "transfer_to_noncharitable_org_cd",
    "disregarded_entity_cd",
    "recv_art_historical_contributions_cd",
    "recv_noncash_contributions_cd",
    "rpt_grants_to_orgs_or_govt_cd",
    "rpt_fundraising_events_15k_cd",
    "rpt_gaming_income_15k_cd",
    "partial_liquidation_cd",
    "business_txn_with_35pct_entity_cd",
    "subject_to_proxy_tax_cd",
    # Part IV checklist — Group B
    "schedule_o_completed_cd",
    "director_business_relationship_cd",
    "family_business_relationship_cd",
    "escrow_account_cd",
    "operates_hospital_cd",
    "operates_school_sec170_cd",
    "related_entity_cd",
    "rpt_investments_other_securities_cd",
    "rpt_program_related_investments_cd",
    "rpt_other_liabilities_cd",
    # Part V
    "backup_withholding_compliance_cd",
    "unrelated_business_income_1k_cd",
    "taxable_party_notification_cd",
    "solicit_nondeductible_contributions_cd",
    "nondeductible_disclosure_cd",
    "quid_pro_quo_contributions_cd",
    "property_disposition_8282_cd",
    "received_funds_for_premiums_cd",
    "paid_premiums_personal_benefit_cd",
    # Part VIII
    "gross_income_fundraising_events",
    "gross_income_gaming_activities",
    "net_income_fundraising_events",
    "net_income_gaming_activities",
    "net_income_inventory_sales",
    "fundraising_events_direct_expenses",
    "gaming_activities_direct_expenses",
    "net_gain_loss_sales",
    # Part IX functional expenses
    "compensation_disqualified_persons",
    "compensation_current_officers",
    "fees_for_services_investment_mgmt",
    "fees_for_services_lobbying",
    "fees_for_services_management",
    # Audited financials & FIN 48
    "fin48_footnote_cd",
    "obtained_separate_audited_cd",
    "included_in_consol_audited_cd",
    "hospital_audited_attached_cd",
    "total_estimated_other_compensation",
    # Other
    "non_pf_status_reason",
    "interest_expense"
  )
)
stopifnot(length(overrides$source_var) == length(overrides$harmonized_name))
src[overrides, on = "source_var", harmonized_name := i.harmonized_name]

# Replace trailing IRC section markers (170 / 509) with _sec170 / _sec509.
# Skip if name already ends in _sec170 / _sec509.
fix_section <- function(s, n) {
  num <- as.character(n)
  already <- endsWith(s, paste0("sec", num))
  ends_in_num <- endsWith(s, num) & !already
  # Strip trailing num and any underscores, then append _sec{num}
  s[ends_in_num] <- sub(paste0("_*", num, "$"), "", s[ends_in_num])
  s[ends_in_num] <- paste0(s[ends_in_num], "_sec", num)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}
src[, harmonized_name := fix_section(harmonized_name, 170)]
src[, harmonized_name := fix_section(harmonized_name, 509)]

# Reorder cols
out <- src[, .(source_var, harmonized_name, description, location, years_present)]

baseline_path <- "data/crosswalks/soi_990_crosswalk_BASELINE.csv"
overrides_path <- "data/crosswalks/soi_990_crosswalk_OVERRIDES.csv"
final_path <- "data/crosswalks/soi_990_crosswalk_FINAL.csv"

# --- BASELINE: algorithmic output. Always regenerated. User does not edit. ---
fwrite(out, baseline_path)

# --- OVERRIDES: user-editable, edited in place. Initialized as full copy of
# BASELINE on first run. NEVER overwritten on subsequent runs. ---
if (!file.exists(overrides_path)) {
  fwrite(out, overrides_path)
  cat("Initialized OVERRIDES as a copy of BASELINE:", overrides_path, "\n")
  cat("  -> Edit this file in place. Future runs will not touch it.\n")
} else {
  cat("Preserving existing OVERRIDES file (script will NOT overwrite):",
      overrides_path, "\n")

  # Drift report: any BASELINE rows missing from OVERRIDES?
  ov <- fread(overrides_path)
  new_in_baseline <- setdiff(out$source_var, ov$source_var)
  removed_from_baseline <- setdiff(ov$source_var, out$source_var)
  if (length(new_in_baseline) > 0) {
    cat("\n*** ATTENTION: source_vars in BASELINE but not OVERRIDES (",
        length(new_in_baseline), ") ***\n", sep = "")
    cat("  ", paste(new_in_baseline, collapse = ", "), "\n", sep = "")
    cat("  -> Append these rows to OVERRIDES manually if you want them in FINAL.\n")
  }
  if (length(removed_from_baseline) > 0) {
    cat("\n*** Heads-up: source_vars in OVERRIDES but no longer in BASELINE (",
        length(removed_from_baseline), ") ***\n", sep = "")
    cat("  ", paste(removed_from_baseline, collapse = ", "), "\n", sep = "")
  }
}

# --- FINAL: regenerated from OVERRIDES. User does not edit. ---
final <- fread(overrides_path)
fwrite(final, final_path)

cat("=== files written ===\n")
cat("  BASELINE (algorithmic, regenerable):", baseline_path, "\n")
cat("  OVERRIDES (user-editable):          ", overrides_path, "\n")
cat("  FINAL    (merge, regenerable):      ", final_path, "\n")
cat("rows:", nrow(out), "\n\n")

cat("=== sample mappings (rows 1, 50, 100, 150, 200, 246) ===\n")
print(final[c(1, 50, 100, 150, 200, 246), .(source_var, harmonized_name, description)])

cat("\n=== potentially problematic — harmonized still contains digits/odd patterns ===\n")
suspicious <- out[grepl("[0-9]{3}", harmonized_name) | nchar(harmonized_name) > 50 |
                  !grepl("^[a-z][a-z0-9_]*$", harmonized_name)]
cat("count suspicious:", nrow(suspicious), "\n")
print(suspicious[, .(source_var, harmonized_name)], nrows=30)

cat("\n=== shared harmonized_name (synonyms — should match override list) ===\n")
dups <- out[, .N, by = harmonized_name][N > 1]
print(dups)
if (nrow(dups) > 0) {
  print(out[harmonized_name %in% dups$harmonized_name, .(source_var, harmonized_name, years_present)])
}
