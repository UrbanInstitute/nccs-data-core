#!/usr/bin/env bash
# ============================================================================
# run_build_panel.sh
#
# Thin wrapper around `Rscript R/run_build_panel.R` for cron / EC2 entry
# points. Mirrors scripts/run_pipeline.sh: forwards CLI args verbatim, captures
# the full console session to a timestamped log under data/logs/merged/, and
# exits with the R process's return code.
#
# Preconditions: BOTH upstream pipelines must have produced output —
# `data/intermediate/harmonized/` (from R/run_pipeline.R) and
# `data/intermediate/harmonized_legacy/` (from R/run_legacy_pipeline.R) —
# before this script can succeed. The orchestrator emits a warning if one
# side is missing and passes the present side through.
#
# Usage:
#   bash scripts/run_build_panel.sh                            # full merge -> upload
#   bash scripts/run_build_panel.sh --no-merge                 # re-use merged tree
#   bash scripts/run_build_panel.sh --upload --strict          # publish to S3
#
# Flags are documented in `R/run_build_panel.R`. This wrapper does not
# interpret them — it forwards verbatim.
#
# Logs:
#   data/logs/merged/04_legacy_merge_log.txt, 05_quality_log.txt,
#   data/logs/merged/07_render_report_log.txt, 08_upload_log.txt
#                                                       (log4r, from R)
#   data/logs/merged/run_build_panel_<UTC_timestamp>.console.log
#                                                       (this wrapper's tee)
# ============================================================================
set -u -o pipefail

cd "$(dirname "$0")/.."

mkdir -p data/logs/merged
ts="$(date -u +%Y%m%dT%H%M%SZ)"
console_log="data/logs/merged/run_build_panel_${ts}.console.log"

started="$(date -Iseconds)"
echo "==== [${started}] run_build_panel.sh start ===="
echo "args: $*"
echo "console log: ${console_log}"
echo

Rscript --vanilla R/run_build_panel.R "$@" 2>&1 | tee "${console_log}"
rc=${PIPESTATUS[0]}

finished="$(date -Iseconds)"
if [[ $rc -eq 0 ]]; then
  echo
  echo "==== [${finished}] run_build_panel.sh OK ===="
  echo "console log: ${console_log}"
else
  echo
  echo "==== [${finished}] run_build_panel.sh FAILED rc=${rc} ====" >&2
  echo "console log: ${console_log}" >&2
fi

exit "${rc}"
