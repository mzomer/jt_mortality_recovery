# ─────────────────────────────────────────────────────────────────────
# 14_build_consensus_snapshot.R — Reproducible, dated snapshot of the
#                                  observer-agreement consensus pipeline.
# ─────────────────────────────────────────────────────────────────────
# Produces three linked CSVs, joined by plot_id, so any plot's full trail
# can be reconstructed: what each observer entered, how the automatic
# median/majority was derived (and which fields were in conflict), and
# what (if anything) was decided in joint review.
#
# Sources the Shiny app's own functions (read_ce_file, clean_reads,
# plot_status, etc.) directly from shiny/jt_agreement_app/app.R, so this
# pipeline can never drift from what the live app computes -- one rule
# set, not two copies to keep in sync.
#
# Run:    source("scripts/14_build_consensus_snapshot.R")
# Output: output/consensus/<timestamp>/observer_counts.csv
#         output/consensus/<timestamp>/automatic_consensus.csv
#         output/consensus/<timestamp>/final_consensus.csv
#         output/consensus/<timestamp>/run_info.txt
# ─────────────────────────────────────────────────────────────────────

library(here)
library(dplyr)
library(purrr)

app_path <- here("shiny", "jt_agreement_app", "app.R")
app_lines <- readLines(app_path)
shiny_idx <- grep("^shinyApp\\(ui, server\\)", app_lines)
if (length(shiny_idx) == 0) stop("Could not find shinyApp(ui, server) in app.R")
eval(parse(text = app_lines[1:(shiny_idx - 1)]), envir = globalenv())

# ── Settings used for this snapshot ───────────────────────────────────
# Keep these fixed for the formal analysis, or document any intentional
# change -- they are recorded in run_info.txt below either way.
ABS_TOL     <- 2
REL_TOL     <- 20
N_REQUIRED  <- 3
NEWEST_ONLY <- TRUE

# ── Load observer exports and the review sheet, exactly like the app ─
folder <- if (nzchar(DRIVE_FOLDER)) list_drive_files(DRIVE_FOLDER) else
  if (nzchar(DATA_FOLDER)) list_folder_files(DATA_FOLDER) else
    stop("Set DRIVE_FOLDER or DATA_FOLDER at the top of app.R first.")

files <- folder$exports
if (nrow(files) == 0) stop("No observer export files found.")

raw <- pmap_dfr(files, function(name, datapath, mtime) {
  read_ce_file(datapath, name, mtime)
})

if (NEWEST_ONLY) {
  info <- summarise_files(raw)
  keep <- info$source_path[info$is_newest]
  raw <- raw[raw$source_path %in% keep, ]
}

reads <- clean_reads(raw)
# Joint-review decisions are a local, git-tracked file (not Drive) -- see
# shiny/jt_agreement_app/joint_reviews.csv and LOCAL_REVIEW_PATH in app.R.
reviews <- read_reviews(LOCAL_REVIEW_PATH)

st <- plot_status(reads, ABS_TOL, REL_TOL, N_REQUIRED, reviews)

# ── Output: one dated, versioned folder per run ───────────────────────
stamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
out_dir <- here("output", "consensus", stamp)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Raw observer data -- what each observer actually entered
observer_counts <- reads %>%
  filter(completed) %>%
  arrange(fire_name, id, observer) %>%
  transmute(plot_id = id, fire = fire_name, fire_year, location = plot_location,
            observer, unsuitable, pre_fire = jt_pre_fire, post_fire = jt_post_fire,
            pre_veg, post_veg, pre_confidence, post_confidence, comments)
write.csv(observer_counts, file.path(out_dir, "observer_counts.csv"),
          row.names = FALSE, na = "")

# 2. Automatic consensus -- the median/majority BEFORE any joint review,
#    and which fields (if any) are in conflict
automatic_consensus <- st %>%
  arrange(fire_name, id) %>%
  transmute(plot_id = id, fire = fire_name, fire_year, location = plot_location,
            n_counts, rule_status, issues,
            median_pre_fire = med_pre, median_post_fire = med_post,
            majority_pre_veg = maj_pre_veg, majority_post_veg = maj_post_veg,
            pre_fire_disputed = f_pre, post_fire_disputed = f_post,
            pre_veg_disputed = f_vpre, post_veg_disputed = f_vpost,
            suitability_disputed = f_suit)
write.csv(automatic_consensus, file.path(out_dir, "automatic_consensus.csv"),
          row.names = FALSE, na = "")

# 3. Final consensus -- the value actually used, and whether it came from
#    the automatic rule or a joint-review decision, per field
field_source <- function(need, rv) {
  if_else(need & !is.na(rv), "joint_review", if_else(!need, "automatic", NA_character_))
}
final_consensus <- st %>%
  arrange(fire_name, id) %>%
  transmute(
    plot_id = id, fire = fire_name, fire_year, location = plot_location,
    status, resolved,
    pre_fire = cons_pre, pre_fire_source = field_source(need_pre, rv_pre),
    post_fire = cons_post, post_fire_source = field_source(need_post, rv_post),
    pre_veg = cons_pre_veg, pre_veg_source = field_source(need_vpre, rv_pre_veg),
    post_veg = cons_post_veg, post_veg_source = field_source(need_vpost, rv_post_veg),
    mortality_pct = round(cons_mortality, 1),
    review_note = rv_note
  )
write.csv(final_consensus, file.path(out_dir, "final_consensus.csv"),
          row.names = FALSE, na = "")

# ── Settings used, for the audit trail ────────────────────────────────
writeLines(c(
  paste("generated_at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("abs_tol:", ABS_TOL),
  paste("rel_tol:", REL_TOL),
  paste("n_required:", N_REQUIRED),
  paste("newest_only:", NEWEST_ONLY),
  paste("n_plots:", nrow(st)),
  paste("n_accepted:", sum(st$status == "Accepted")),
  paste("n_conflict_unresolved:", sum(st$status == "Conflict")),
  paste("n_excluded:", sum(st$status == "Excluded"))
), file.path(out_dir, "run_info.txt"))

message("Wrote snapshot to ", out_dir)
message(sprintf("  %d plots, %d accepted, %d unresolved conflicts, %d excluded",
                nrow(st), sum(st$status == "Accepted"),
                sum(st$status == "Conflict"), sum(st$status == "Excluded")))
