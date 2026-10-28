# fetch_raw_data(): restore an archived data year from its GitHub Release.
#
# Archived years live as one zip per year (release tag `raw-data-{year}`,
# asset `raw-data-{year}.zip`). The repo is public, so no login is needed.
# The committed file data/{year}.sha256 is checked before anything is unpacked.
#
# Bundle layout (written by archive_year()):
#   raw/       lab files exactly as they sat in data/
#   year/      config.yml, pipeline QMDs, year_files/, output/ (incl. rendered report)
#   MANIFEST.csv
#
# what = "raw"  restores raw/ into data/ (enough to re-run the pipeline on the raw files)
# what = "all"  also restores year/ into archive_restore/{year}/ (gitignored) for inspection

fetch_raw_data <- function(year,
                           what      = c("raw", "all"),
                           repo      = "Kenai-Watershed-Forum/kenai-river-wqx-qaqc",
                           base_url  = NULL,
                           data_dir  = here::here("data"),
                           restore_dir = here::here("archive_restore"),
                           overwrite = FALSE) {
  what <- match.arg(what)
  stopifnot(requireNamespace("digest", quietly = TRUE), requireNamespace("zip", quietly = TRUE))

  asset <- sprintf("raw-data-%s.zip", year)
  if (is.null(base_url)) {
    base_url <- sprintf("https://github.com/%s/releases/download/raw-data-%s", repo, year)
  }
  sha_file <- file.path(data_dir, paste0(year, ".sha256"))
  if (!file.exists(sha_file)) {
    stop("No checksum file ", sha_file, ". Was ", year, " archived? ",
         "archive_year() writes it and it must be committed.", call. = FALSE)
  }
  expected <- strsplit(trimws(readLines(sha_file, n = 1, warn = FALSE)), "\\s+")[[1]][1]

  if (!overwrite) {
    existing <- list.files(data_dir)
    existing <- existing[!grepl("\\.sha256$", existing)]
    if (length(existing) > 0) {
      stop("data/ already holds files (", paste(utils::head(existing, 3), collapse = ", "),
           "). Move them or use overwrite = TRUE.", call. = FALSE)
    }
  }

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  utils::download.file(paste0(base_url, "/", asset), zip_path, mode = "wb", quiet = TRUE)

  actual <- digest::digest(zip_path, algo = "sha256", file = TRUE)
  if (!identical(tolower(actual), tolower(expected))) {
    stop("Checksum mismatch for ", asset, ".\n  expected: ", expected, "\n  actual:   ", actual,
         "\nNothing was unpacked.", call. = FALSE)
  }

  stage <- tempfile("bundle_")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  zip::unzip(zip_path, exdir = stage)

  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  raw_files <- list.files(file.path(stage, "raw"), recursive = TRUE, all.files = TRUE)
  for (f in raw_files) {
    dest <- file.path(data_dir, f)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(stage, "raw", f), dest, overwrite = TRUE)
  }
  message("Restored ", length(raw_files), " raw file(s) to ", data_dir)

  if (what == "all") {
    dest_root <- file.path(restore_dir, year)
    dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)
    yr_files <- list.files(file.path(stage, "year"), recursive = TRUE, all.files = TRUE)
    for (f in yr_files) {
      dest <- file.path(dest_root, f)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(stage, "year", f), dest, overwrite = TRUE)
    }
    message("Restored ", length(yr_files), " year file(s) to ", dest_root)
  }
  invisible(TRUE)
}
