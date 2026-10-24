# ─────────────────────────────────────────────────────────────────────
# 15_sync_drive_exports.R — Copy Collect Earth exports from the shared
# Google Drive folder into the repo, so they can be version-controlled
#
# Input:  shared Drive folder (DRIVE_FOLDER), incl. subfolders
# Output: data/from_drive/*.csv (read by shiny/jt_agreement_app/app_v1.R)
#
# Run by hand. Never overwrites: a file already synced is skipped if
# unchanged; if the Drive copy changed, the new version is saved alongside
# with a suffix. Afterwards, review `git status` and commit data/from_drive.
# ─────────────────────────────────────────────────────────────────────

library(googledrive)
library(here)

options(gargle_oauth_email = TRUE)


# Setup ----

DRIVE_FOLDER <- "1xC1P6iVUxQd2cq3tSNHYuaojeWyqxvGL"
export_pattern <- "collectedData.*\\.csv$"
dest_dir <- here("data", "from_drive")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)


# List Drive exports ----

found <- drive_ls(as_id(DRIVE_FOLDER), recursive = TRUE)
found <- found[grepl(export_pattern, found$name, ignore.case = TRUE), ]

md5 <- vapply(found$drive_resource,
              function(r) r$md5Checksum %||% NA_character_, character(1))
modified <- vapply(found$drive_resource,
                   function(r) r$modifiedTime %||% NA_character_, character(1))

local_md5 <- unname(tools::md5sum(list.files(dest_dir, full.names = TRUE)))


# Download new files ----

status <- character(nrow(found))
for (i in seq_len(nrow(found))) {
  if (!is.na(md5[i]) && md5[i] %in% local_md5) {
    status[i] <- "unchanged"
    next
  }
  path <- file.path(dest_dir, found$name[i])
  if (file.exists(path)) {
    # Same name, different content: keep both versions
    stamp <- gsub("[^0-9]", "", substr(modified[i], 1, 19))
    path <- sub("\\.csv$", paste0("_drive", stamp, ".csv"), path,
                ignore.case = TRUE)
    status[i] <- "changed on Drive (saved as new version)"
  } else {
    status[i] <- "new"
  }
  drive_download(as_id(found$id[i]), path = path, overwrite = FALSE)
  local_md5 <- c(local_md5, unname(tools::md5sum(path)))
}


# Summary ----

message(nrow(found), " export(s) on Drive -> ", dest_dir)
print(table(status))
if (any(status != "unchanged")) {
  message("Review and commit:\n  git add data/from_drive && ",
          "git commit -m \"Sync Collect Earth exports\"")
}

