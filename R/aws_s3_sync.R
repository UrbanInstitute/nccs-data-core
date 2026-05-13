#' Run `aws s3 sync` with optional dry-run + extra flags. Returns 0 on success.
#'
#' Direction-agnostic: either `src` or `dest` may be an `s3://...` URI. When
#' the source is local, we check that the directory exists and short-circuit
#' on a missing source so the caller logs a SKIP rather than letting the CLI
#' fail. When the source is on S3, we let the CLI handle missing prefixes
#' (it treats them as empty, which is the desired no-op for rehydrate).
#'
#' Args are shell-quoted before invocation because `system2(..., stdout=TRUE,
#' stderr=TRUE)` captures output via a shell, which would otherwise glob-expand
#' arguments like `--exclude *` into the current working directory's file list
#' before `aws` sees them. shQuote() prevents this expansion.
aws_sync <- function(src, dest, dry_run = FALSE, extra_args = character(), logger = NULL) {
  is_s3_src <- startsWith(src, "s3://")
  if (!is_s3_src && !dir.exists(src)) {
    if (!is.null(logger)) log4r::warn(logger, sprintf("SKIP sync: source %s does not exist", src))
    return(invisible(0L))
  }
  args <- c("s3", "sync", src, dest, extra_args)
  if (dry_run) args <- c(args, "--dryrun")
  if (!is.null(logger)) log4r::info(logger, sprintf("aws %s", paste(args, collapse = " ")))
  status <- system2("aws", args = shQuote(args), stdout = TRUE, stderr = TRUE)
  attr_status <- attr(status, "status")
  rc <- if (is.null(attr_status)) 0L else as.integer(attr_status)
  if (!is.null(logger)) {
    if (rc == 0L) log4r::info(logger, sprintf("  ok (%d lines)", length(status)))
    else          log4r::error(logger, sprintf("  failed rc=%d: %s",
                                               rc, paste(tail(status, 5), collapse = " | ")))
  }
  invisible(rc)
}
