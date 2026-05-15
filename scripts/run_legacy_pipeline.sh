#!/usr/bin/env bash
# ============================================================================
# run_legacy_pipeline.sh
#
# Thin wrapper around `Rscript R/run_legacy_pipeline.R` for cron / EC2 entry
# points. Mirrors scripts/run_pipeline.sh: forwards CLI args verbatim, captures
# the full console session to a timestamped log under data/logs/legacy/, and
# exits with the R process's return code.
#
# Usage:
#   bash scripts/run_legacy_pipeline.sh                                  # full run
#   bash scripts/run_legacy_pipeline.sh --no-download --no-upload        # re-use mirror
#   bash scripts/run_legacy_pipeline.sh --upload --strict                # publish to S3
#
# Flags are documented in `R/run_legacy_pipeline.R`. This wrapper does not
# interpret them — it forwards verbatim.
#
# Logs:
#   data/logs/legacy/05_quality_log.txt, 07_render_report_log.txt, 08_upload_log.txt
#                                                       (log4r, from R)
#   data/logs/legacy/run_legacy_pipeline_<UTC_timestamp>.console.log
#                                                       (this wrapper's tee)
# ============================================================================
set -u -o pipefail

cd "$(dirname "$0")/.."

mkdir -p data/logs/legacy
ts="$(date -u +%Y%m%dT%H%M%SZ)"
console_log="data/logs/legacy/run_legacy_pipeline_${ts}.console.log"

started="$(date -Iseconds)"
echo "==== [${started}] run_legacy_pipeline.sh start ===="
echo "args: $*"
echo "console log: ${console_log}"
echo

Rscript --vanilla R/run_legacy_pipeline.R "$@" 2>&1 | tee "${console_log}"
rc=${PIPESTATUS[0]}

finished="$(date -Iseconds)"
if [[ $rc -eq 0 ]]; then
  echo
  echo "==== [${finished}] run_legacy_pipeline.sh OK ===="
  echo "console log: ${console_log}"
else
  echo
  echo "==== [${finished}] run_legacy_pipeline.sh FAILED rc=${rc} ====" >&2
  echo "console log: ${console_log}" >&2
fi

exit "${rc}"
