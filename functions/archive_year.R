# archive_year() and remove_archived_year(): close out a data year.
#
# archive_year(year)
#   Builds raw-data-{year}.zip (the "year bundle"), writes data/{year}.sha256
#   (commit it), and prints the upload steps. Deletes nothing.
#
# remove_archived_year(year)
#   Run only after the release is uploaded. Downloads the release asset, checks it
#   against data/{year}.sha256, and only then clears the working year's files.
#
# Bundle layout (read back by fetch_raw_data()):
#   raw/    everything in data/ except *.sha256 and .gitignore
#   year/   config.yml, qaqc.qmd, prelim_report.qmd, project_year/{year}.qmd (whichever
#           exist), year_files/, output/ (including the rendered report)
#   MANIFEST.csv   path, bytes, sha256 for every bundled file

.year_bundle_paths <- function(year, root) {
  singles <- c("config.yml", "qaqc.qmd", "prelim_report.qmd",
               file.path("project_year", paste0(year, ".qmd")))
  files <- singles[file.exists(file.path(root, singles))]
  for (d in c("year_files", "output")) {
    if (dir.exists(file.path(root, d))) {
      inside <- list.files(file.path(root, d), recursive = TRUE, all.files = TRUE)
      files <- c(files, file.path(d, inside))
    }
  }
  files
}

archive_year <- function(year,
                         root           = here::here(),
                         dest_dir       = file.path(root, "archive_staging"),
                         write_checksum = TRUE) {
  stopifnot(requireNamespace("digest", quietly = TRUE),
            requireNamespace("zip", quietly = TRUE),
            requireNamespace("yaml", quietly = TRUE))

  cfg_year <- yaml::read_yaml(file.path(root, "config.yml"))$year
  if (!identical(as.integer(cfg_year), as.integer(year))) {
    stop("config.yml is for year ", cfg_year, ", not ", year,
         ". Archive from the working copy that holds ", year, ".", call. = FALSE)
  }

  data_dir <- file.path(root, "data")
  raw <- list.files(data_dir, recursive = TRUE, all.files = TRUE)
  raw <- raw[!grepl("\\.sha256$", raw) & basename(raw) != ".gitignore"]
  if (length(raw) == 0) stop("data/ has no raw files to archive.", call. = FALSE)

  yr <- .year_bundle_paths(year, root)

  stage <- tempfile("bundle_")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  copy_in <- function(from_root, rel, to_sub) {
    for (f in rel) {
      dest <- file.path(stage, to_sub, f)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(from_root, f), dest)
    }
  }
  copy_in(data_dir, raw, "raw")
  copy_in(root, yr, "year")

  bundled <- list.files(stage, recursive = TRUE, all.files = TRUE)
  manifest <- data.frame(
    path   = bundled,
    bytes  = file.size(file.path(stage, bundled)),
    sha256 = vapply(file.path(stage, bundled), digest::digest, character(1),
                    algo = "sha256", file = TRUE, USE.NAMES = FALSE)
  )
  utils::write.csv(manifest, file.path(stage, "MANIFEST.csv"), row.names = FALSE)

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, sprintf("raw-data-%s.zip", year))
  unlink(zip_path)
  zip::zip(zip_path, files = list.files(stage, all.files = TRUE, no.. = TRUE),
           root = stage, mode = "cherry-pick")

  sha <- digest::digest(zip_path, algo = "sha256", file = TRUE)
  if (write_checksum) {
    writeLines(paste0(sha, "  ", basename(zip_path)),
               file.path(data_dir, paste0(year, ".sha256")))
  }

  message(sprintf(
    "Built %s (%.1f MB; %d raw, %d year files)\nsha256: %s%s\n\nNext:\n%s",
    zip_path, file.size(zip_path) / 1e6, length(raw), length(yr), sha,
    if (write_checksum) paste0("\nWrote data/", year, ".sha256 (commit it)") else "",
    paste0(
      "  1. GitHub > Releases > Draft a new release\n",
      "  2. Tag: raw-data-", year, "   Title: ", year, " raw data and year bundle\n",
      "  3. Attach ", basename(zip_path), " and publish\n",
      "  4. Run remove_archived_year(", year, ") to verify the upload and clear the working year"
    )))
  invisible(list(zip = zip_path, sha256 = sha, manifest = manifest))
}

remove_archived_year <- function(year,
                                 root     = here::here(),
                                 repo     = "Kenai-Watershed-Forum/kenai-river-wqx-qaqc",
                                 base_url = NULL,
                                 confirm  = interactive()) {
  stopifnot(requireNamespace("digest", quietly = TRUE))
  sha_file <- file.path(root, "data", paste0(year, ".sha256"))
  if (!file.exists(sha_file)) {
    stop("No ", sha_file, "; run archive_year() first.", call. = FALSE)
  }
  expected <- strsplit(trimws(readLines(sha_file, n = 1, warn = FALSE)), "\\s+")[[1]][1]

  if (is.null(base_url)) {
    base_url <- sprintf("https://github.com/%s/releases/download/raw-data-%s", repo, year)
  }
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  utils::download.file(sprintf("%s/raw-data-%s.zip", base_url, year), tmp,
                       mode = "wb", quiet = TRUE)
  actual <- digest::digest(tmp, algo = "sha256", file = TRUE)
  if (!identical(tolower(actual), tolower(expected))) {
    stop("Published asset does not match data/", year, ".sha256. Nothing was removed.",
         call. = FALSE)
  }

  data_entries <- list.files(file.path(root, "data"))
  data_entries <- data_entries[!grepl("\\.sha256$", data_entries)]
  targets <- c(file.path(root, "data", data_entries),
               file.path(root, "year_files"),
               file.path(root, "output"))
  targets <- targets[file.exists(targets)]

  if (confirm) {
    message("Verified. About to delete:\n  ", paste(basename(targets), collapse = "\n  "))
    if (!identical(tolower(readline("Type 'yes' to delete: ")), "yes")) {
      message("Cancelled.")
      return(invisible(FALSE))
    }
  }
  unlink(targets, recursive = TRUE)
  message("Removed the working files for ", year, ". Next: git add -A, then commit.")
  invisible(TRUE)
}
