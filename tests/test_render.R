# tests/test_render.R
# Tests for R/07_render_report.R's place_render_output() helper.
#
# Regression test for commit a95f48a: file.rename() silently fails (EXDEV)
# on cross-filesystem moves between tempdir (tmpfs) and the repo's persistent
# volume — observed during the first EC2 production run inside Docker, where
# /tmp resolves to tmpfs and the workdir lives on a separate mount. The
# original code logged WROTE before checking the rename's return value, so
# phase 7 reported success while 0 HTMLs landed on disk.
#
# We simulate the cross-filesystem failure by injecting a stub rename_fn that
# returns FALSE (matching base::file.rename's behavior on EXDEV) without
# needing a real cross-mount setup, which is not portable across CI / dev
# environments.
#
# Run from repo root:  Rscript tests/test_render.R

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "07_render_report.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# Each block uses its own tempdir so leftover files from one case can't
# affect the next.
make_fixture <- function(content = "<html>ok</html>") {
  tdir <- tempfile("render_test_"); dir.create(tdir)
  src  <- file.path(tdir, "rendered.html")
  dest <- file.path(tdir, "final", "out.html")
  dir.create(dirname(dest), recursive = TRUE)
  writeLines(content, src)
  list(tdir = tdir, src = src, dest = dest)
}

cat("\n[place_render_output: same-filesystem rename succeeds]\n")
{
  fx <- make_fixture()
  ok <- place_render_output(fx$src, fx$dest)
  check("returns TRUE",            ok)
  check("dest file exists",        file.exists(fx$dest))
  check("src was moved (gone)",    !file.exists(fx$src))
  unlink(fx$tdir, recursive = TRUE)
}

cat("\n[place_render_output: cross-filesystem fallback (EXDEV simulation)]\n")
{
  # Stub matches base::file.rename's EXDEV failure mode: returns FALSE
  # (with a warning in real R; we omit the warning here since the production
  # caller wraps the call in suppressWarnings()).
  fx <- make_fixture("<html>cross-fs</html>")
  rename_calls <- 0L
  fake_rename <- function(from, to) { rename_calls <<- rename_calls + 1L; FALSE }
  ok <- place_render_output(fx$src, fx$dest, rename_fn = fake_rename)
  check("returns TRUE on copy fallback",   ok)
  check("rename_fn was invoked once",      rename_calls == 1L)
  check("dest file exists",                file.exists(fx$dest))
  check("dest content matches src",
        identical(readLines(fx$dest), "<html>cross-fs</html>"))
  check("src was unlinked after copy",     !file.exists(fx$src))
  unlink(fx$tdir, recursive = TRUE)
}

cat("\n[place_render_output: returns FALSE when both rename and copy fail]\n")
{
  fx <- make_fixture()
  fake_rename <- function(from, to) FALSE
  fake_copy   <- function(from, to, overwrite = FALSE) FALSE
  ok <- place_render_output(fx$src, fx$dest,
                            rename_fn = fake_rename,
                            copy_fn   = fake_copy)
  check("returns FALSE",                ok == FALSE)
  check("dest file does not exist",     !file.exists(fx$dest))
  check("src is left in place",         file.exists(fx$src))
  unlink(fx$tdir, recursive = TRUE)
}

cat("\n[place_render_output: returns FALSE when copy 'succeeds' but dest is missing]\n")
{
  # Guards against a defensive case: a buggy copy_fn that returns TRUE
  # without actually writing the file. The post-move file.exists() check
  # catches this. Not a real failure mode of base::file.copy, but cheap
  # insurance — and the original a95f48a bug had exactly this shape (rename
  # appeared to succeed, file didn't exist).
  fx <- make_fixture()
  fake_rename <- function(from, to) FALSE
  fake_copy   <- function(from, to, overwrite = FALSE) TRUE
  ok <- place_render_output(fx$src, fx$dest,
                            rename_fn = fake_rename,
                            copy_fn   = fake_copy)
  check("returns FALSE when dest missing", ok == FALSE)
  unlink(fx$tdir, recursive = TRUE)
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
