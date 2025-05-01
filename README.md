# nccs-data-core

One of NCCS’s primary datasets is the Core series, containing select fields from the Form 990, Form 990EZ, and Form 990PF. 

## Data Sources

The Core Series is created from 2 sources:

1. [NCCS’s original Core Series](https://urbaninstitute.github.io/nccs-legacy/)
2. [The IRS’s SOI Extracts](https://www.irs.gov/statistics/soi-tax-stats-annual-extract-of-tax-exempt-organization-financial-data)

## Data Engineering Process

Creating the CORE Series requires the following steps:

1. Standardizing the column names in both the Legacy CORE and SOI Extracts with a crosswalk.
    a. The crosswalk maps old column names to new column names created in the concordance file
    b.Columns are renamed using the crosswalk
2. Partitioning the files according to their scope, there are 5 scopes
    a. 501C3-CHARITIES-PC: All variables common to the Form 990 and 990EZ for 501(c)(3) organizations
    b. 501C3-CHARITIES-PZ: All variables found in the Form 990 for 501(c)(3) organizations
    c. 501CE-NONPROFIT-PC: All variables common to the Form 990 and 990EZ for non-501(c)(3) organizations
    d. 501CE-NONPROFIT-PZ: All variables found in the Form 990 for non-501(c)(3) organizations
    e. 501C3-PRIVFOUND-PF: All variables found in the Form 990PF for 501(c)(3) private foundations
3. Further partitioning the files according to tax year, found in the first four characters of the column for “Tax Period”
    a. Existing SOI and Legacy files are saved according to the year the form was submitted instead of the accounting period the form was submitted for. This can cause confusion amongst researchers
4. Creating a schema for the newly created files to verify variable availability

## Output Datasets

The output datasets are saved according to this naming convention

**CORE-{YEAR}-{SCOPE}-HRMN-{Version}.csv**

For example:

`CORE-2000-501C3-CHARITIES-PC-HRMN-V0.csv` Provides the tax year, scope, and versioning for the outputs

## Harmonization Workflow

1. Run scripts in numerical order from the R/ folder
2. Outputs are saved to data/ folder
