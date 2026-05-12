#!/usr/bin/env bash
# ============================================================================
# run_pipeline.sh
#
# Thin wrapper around `Rscript R/run_pipeline.R` for cron / EC2 entry points.
# Passes all CLI args through unchanged, captures the full console session
# to a timestamped log file alongside the R-side log4r output, and exits
# with the R process's return code so cron / CI surface failures correctly.
#
# Usage:
#   bash scripts/run_pipeline.sh                                  # all defaults
#   bash scripts/run_pipeline.sh --years 2024 --forms 990ez --no-upload
#   bash scripts/run_pipeline.sh --years 2012-2024 --strict
#   bash scripts/run_pipeline.sh --skip-download --skip-unpack    # re-use intermediate
#
# Flags are documented in `R/run_pipeline.R`. This wrapper does not interpret
# them — it forwards verbatim.
#
# Cron example (run daily at 04:00 UTC, mail on failure only):
#   0 4 * * * cd /opt/nccs-data-core && bash scripts/run_pipeline.sh \
#               --years 2012-2024 --strict --upload >/dev/null
#
# Logs:
#   data/logs/run_pipeline_log.txt                    (log4r, from R)
#   data/logs/run_pipeline_<UTC_timestamp>.console.log (this wrapper's tee)
# ============================================================================
set -u -o pipefail

# cd to repo root regardless of where the script is invoked from.
cd "$(dirname "$0")/.."

mkdir -p data/logs
ts="$(date -u +%Y%m%dT%H%M%SZ)"
console_log="data/logs/run_pipeline_${ts}.console.log"

started="$(date -Iseconds)"
echo "==== [${started}] run_pipeline.sh start ===="
echo "args: $*"
echo "console log: ${console_log}"
echo

# --vanilla: skip user ~/.Rprofile, ~/.Renviron, and saved .RData so the
# pipeline runs hermetically regardless of host R configuration.
Rscript --vanilla R/run_pipeline.R "$@" 2>&1 | tee "${console_log}"
rc=${PIPESTATUS[0]}

finished="$(date -Iseconds)"
if [[ $rc -eq 0 ]]; then
  echo
  echo "==== [${finished}] run_pipeline.sh OK ===="
  echo "console log: ${console_log}"
else
  echo
  echo "==== [${finished}] run_pipeline.sh FAILED rc=${rc} ====" >&2
  echo "console log: ${console_log}" >&2
fi

exit "${rc}"
