# ─────────────────────────────────────────────────────────────────────
# app.R — Joshua tree interpretation and review workflow
#
# Primary Collect Earth exports contain notes/suitability for pinned plots.
# Reviewed exports contain "_reviewed" in the filename and hold the reviewed
# counts, vegetation cover and suitability. This app tracks review workflow
# only; ecological data remain in Collect Earth.
# ─────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

options(shiny.maxRequestSize = 30 * 1024^2)
options(gargle_oauth_email = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x


# Setup ----

DRIVE_FOLDER <- "1xC1P6iVUxQd2cq3tSNHYuaojeWyqxvGL"
DATA_FOLDER <- ""
LEGACY_WORKFLOW_LOG <- "workflow_log.csv"
WORKFLOW_SHEET_NAME <- "jt_review_workflow_log"
WORKFLOW_SHEET_TAB <- "events"

drive_poll_ms <- 60000
folder_poll_ms <- 15000
workflow_poll_ms <- 15000

status_levels <- c(
  "Not started", "Ready for review", "Team review",
  "Reviewed but incomplete", "Reviewed and final"
)

status_colors <- c(
  "Not started" = "#B8B3A2",
  "Ready for review" = "#C9A55A",
  "Team review" = "#A3452F",
  "Reviewed but incomplete" = "#6E7F8D",
  "Reviewed and final" = "#4E6A3E"
)

status_text <- c(
  "Not started" = "#2B2B2B",
  "Ready for review" = "#2B2B2B",
  "Team review" = "#FFFFFF",
  "Reviewed but incomplete" = "#FFFFFF",
  "Reviewed and final" = "#FFFFFF"
)

team_reasons <- c(
  "Tree identification", "Live/dead interpretation", "Imagery/date",
  "Plot suitability", "Vegetation cover", "Other"
)


# Google Drive / local files ----

empty_files <- function() {
  data.frame(
    name = character(),
    datapath = character(),
    mtime = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

empty_folder <- function() {
  list(exports = empty_files())
}

list_folder_files <- function(folder) {

  folder <- path.expand(folder)

  if (!dir.exists(folder)) {
    stop("Folder not found: ", folder)
  }

  paths <- list.files(
    folder, pattern = "\\.csv$", ignore.case = TRUE,
    full.names = TRUE, recursive = TRUE
  )

  if (length(paths) == 0) {
    return(empty_folder())
  }

  list(
    exports = data.frame(
      name = basename(paths),
      datapath = paths,
      mtime = file.info(paths)$mtime,
      stringsAsFactors = FALSE
    )
  )
}

drive_cache <- file.path(tempdir(), "jt_review_drive_cache")

drive_list_raw <- function(folder) {

  found <- googledrive::drive_ls(
    googledrive::as_id(folder), recursive = TRUE
  )

  data.frame(
    id = as.character(found$id),
    name = found$name,
    modified = vapply(
      found$drive_resource,
      function(x) x$modifiedTime %||% NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

drive_fetch <- function(id, path) {
  googledrive::drive_download(
    googledrive::as_id(id), path = path,
    overwrite = TRUE
  )
}

cache_path <- function(id, modified) {
  file.path(
    drive_cache,
    paste0(id, "_", gsub("[^0-9]", "", modified), ".csv")
  )
}

list_drive_files <- function(folder) {

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Install googledrive first: install.packages(\"googledrive\")"
    )
  }

  found <- drive_list_raw(folder)
  found <- found[
    grepl("\\.csv$", found$name, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  if (nrow(found) == 0) {
    return(empty_folder())
  }

  dir.create(drive_cache, showWarnings = FALSE, recursive = TRUE)
  paths <- cache_path(found$id, found$modified)

  for (i in seq_len(nrow(found))) {
    if (!file.exists(paths[i]) || is.na(found$modified[i])) {
      drive_fetch(found$id[i], paths[i])
    }
  }

  list(
    exports = data.frame(
      name = found$name,
      datapath = paths,
      mtime = as.POSIXct(
        found$modified,
        format = "%Y-%m-%dT%H:%M:%OS",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )
}

folder_source <- function() {
  if (nzchar(DRIVE_FOLDER)) {
    "drive"
  } else if (nzchar(DATA_FOLDER)) {
    "local"
  } else {
    "none"
  }
}


# Collect Earth data ----

optional_cols <- c(
  "operator", "actively_saved", "actively_saved_on_year",
  "actively_saved_on_month", "actively_saved_on_day", "fire_name",
  "pre_confidence", "pre_vegetation_cover", "post_confidence",
  "post_vegetation_cover", "comments", "plot_unsuitable",
  "location_x", "location_y"
)

empty_ce <- function() {
  data.frame(
    id = character(),
    jt_pre_fire = character(),
    jt_post_fire = character(),
    operator = character(),
    actively_saved = character(),
    actively_saved_on_year = character(),
    actively_saved_on_month = character(),
    actively_saved_on_day = character(),
    fire_name = character(),
    pre_confidence = character(),
    pre_vegetation_cover = character(),
    post_confidence = character(),
    post_vegetation_cover = character(),
    comments = character(),
    plot_unsuitable = character(),
    location_x = character(),
    location_y = character(),
    observer = character(),
    stage = character(),
    source_file = character(),
    source_path = character(),
    file_time = as.POSIXct(character()),
    row_in_file = integer(),
    stringsAsFactors = FALSE
  )
}

read_ce_file <- function(path, filename, file_time = Sys.time()) {

  raw <- read.csv(
    path, na.strings = c("", "NA"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
  )

  required <- c("id", "jt_pre_fire", "jt_post_fire")
  missing <- setdiff(required, names(raw))

  if (length(missing) > 0) {
    stop(
      filename, " is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  for (col in setdiff(optional_cols, names(raw))) {
    raw[[col]] <- NA_character_
  }

  operators <- sort(table(raw$operator), decreasing = TRUE)

  observer <- if (length(operators) > 0) {
    names(operators)[1]
  } else {
    str_extract(filename, "^[^_]+")
  }

  stage <- if (
    str_detect(str_to_lower(filename), "_reviewed")
  ) {
    "reviewed"
  } else {
    "primary"
  }

  raw %>%
    mutate(
      observer = observer,
      stage = stage,
      source_file = filename,
      source_path = path,
      file_time = file_time,
      row_in_file = row_number()
    )
}

clean_reads <- function(raw) {

  if (nrow(raw) == 0) {
    return(raw)
  }

  raw %>%
    mutate(
      actively_saved = str_to_lower(
        coalesce(as.character(actively_saved), "")
      ) %in% c("true", "t", "1", "yes"),
      saved_date = suppressWarnings(
        as.Date(
          paste(
            actively_saved_on_year,
            actively_saved_on_month,
            actively_saved_on_day,
            sep = "-"
          )
        )
      ),
      fire_from_id = str_match(id, "^(.+?)_(\\d{4})_")[, 2] %>%
        str_replace_all("_", " ") %>%
        str_to_upper(),
      fire_name = coalesce(fire_from_id, str_to_upper(fire_name)),
      fire_year = suppressWarnings(
        as.integer(str_match(id, "_(\\d{4})_")[, 2])
      ),
      plot_number = suppressWarnings(
        as.integer(str_match(id, "_(\\d+)$")[, 2])
      ),
      plot_location = case_when(
        str_detect(id, "_inside_") ~ "Inside",
        str_detect(id, "_outside_") ~ "Outside",
        TRUE ~ "Other"
      ),
      unsuitable = coalesce(
        str_detect(
          str_to_lower(coalesce(plot_unsuitable, "")),
          "unsuit"
        ),
        FALSE
      ),
      jt_pre_fire = suppressWarnings(as.numeric(jt_pre_fire)),
      jt_post_fire = suppressWarnings(as.numeric(jt_post_fire)),
      pre_veg = str_to_lower(str_trim(pre_vegetation_cover)),
      post_veg = str_to_lower(str_trim(post_vegetation_cover)),
      comments = coalesce(comments, ""),
      has_primary_entry = stage == "primary" &
        (
          actively_saved | unsuitable | nzchar(comments) |
            !is.na(pre_confidence) | !is.na(post_confidence)
        ),
      has_review_entry = stage == "reviewed" &
        (
          actively_saved | unsuitable | nzchar(comments) |
            !is.na(jt_pre_fire) | !is.na(jt_post_fire) |
            !is.na(pre_veg) | !is.na(post_veg)
        )
    )
}

latest_record <- function(data, stage_name) {

  if (nrow(data) == 0) {
    return(data)
  }

  entry_col <- if (stage_name == "primary") {
    "has_primary_entry"
  } else {
    "has_review_entry"
  }

  data %>%
    filter(stage == stage_name, .data[[entry_col]]) %>%
    group_by(id) %>%
    arrange(
      desc(saved_date), desc(file_time), desc(row_in_file),
      .by_group = TRUE
    ) %>%
    slice(1) %>%
    ungroup()
}

latest_primary_records <- function(data) {

  if (nrow(data) == 0) {
    return(data)
  }

  data %>%
    filter(stage == "primary", has_primary_entry) %>%
    group_by(id, observer) %>%
    arrange(
      desc(saved_date), desc(file_time), desc(row_in_file),
      .by_group = TRUE
    ) %>%
    slice(1) %>%
    ungroup()
}


record_signature <- function(data) {

  if (nrow(data) == 0) {
    return(data.frame(id = character(), signature = character()))
  }

  data %>%
    transmute(
      id,
      signature = str_c(
        source_file,
        format(file_time, "%Y-%m-%d %H:%M:%S"),
        coalesce(as.character(jt_pre_fire), ""),
        coalesce(as.character(jt_post_fire), ""),
        coalesce(pre_veg, ""),
        coalesce(post_veg, ""),
        unsuitable,
        comments,
        sep = "|"
      )
    )
}


# Workflow log ----

empty_workflow_log <- function() {
  data.frame(
    plot_id = character(),
    needs_team_review = logical(),
    team_reason = character(),
    team_flag_note = character(),
    team_resolved = logical(),
    team_outcome = character(),
    team_resolution_note = character(),
    team_review_signature = character(),
    expert_status = character(),
    expert_correction_count = integer(),
    expert_note = character(),
    expert_signature = character(),
    expert_at = character(),
    stringsAsFactors = FALSE
  )
}

normalise_workflow_log <- function(data) {

  template <- empty_workflow_log()

  for (col in setdiff(names(template), names(data))) {

    default <- if (is.logical(template[[col]])) {
      FALSE
    } else if (is.integer(template[[col]])) {
      0L
    } else {
      ""
    }

    data[[col]] <- rep(default, nrow(data))
  }

  data %>%
    select(all_of(names(template))) %>%
    mutate(
      plot_id = as.character(plot_id),
      needs_team_review = coalesce(
        as.logical(needs_team_review), FALSE
      ),
      team_resolved = coalesce(
        as.logical(team_resolved), FALSE
      ),
      expert_correction_count = coalesce(
        suppressWarnings(as.integer(expert_correction_count)), 0L
      ),
      across(
        c(
          team_reason, team_flag_note, team_outcome,
          team_resolution_note, team_review_signature,
          expert_status, expert_note, expert_signature,
          expert_at
        ),
        ~ coalesce(as.character(.x), "")
      )
    )
}

read_legacy_workflow_log <- function(path) {

  if (!file.exists(path)) {
    return(empty_workflow_log())
  }

  read.csv(
    path, stringsAsFactors = FALSE, na.strings = c("", "NA")
  ) %>%
    normalise_workflow_log()
}

empty_workflow_events <- function() {
  data.frame(
    event_id = character(),
    event_time = character(),
    plot_id = character(),
    action = character(),
    value = character(),
    session_id = character(),
    stringsAsFactors = FALSE
  )
}

normalise_workflow_events <- function(data) {

  template <- empty_workflow_events()

  for (col in setdiff(names(template), names(data))) {
    data[[col]] <- rep("", nrow(data))
  }

  data %>%
    select(all_of(names(template))) %>%
    mutate(
      across(everything(), ~ coalesce(as.character(.x), ""))
    ) %>%
    filter(nzchar(plot_id), nzchar(action))
}

ensure_workflow_sheet <- function(folder) {

  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop(
      "Install googlesheets4 for shared workflow tracking: ",
      "install.packages(\"googlesheets4\")"
    )
  }

  files <- googledrive::drive_ls(
    googledrive::as_id(folder)
  )

  hit <- files %>%
    filter(name == WORKFLOW_SHEET_NAME)

  if (nrow(hit) > 0) {
    return(
      googlesheets4::as_sheets_id(hit$id[1])
    )
  }

  ss <- googlesheets4::gs4_create(
    WORKFLOW_SHEET_NAME,
    sheets = setNames(
      list(empty_workflow_events()),
      WORKFLOW_SHEET_TAB
    )
  )

  googledrive::drive_mv(
    googledrive::as_id(ss),
    path = googledrive::as_id(folder)
  )

  ss
}

read_workflow_events <- function(ss) {

  sheets <- googlesheets4::sheet_names(ss)

  if (!WORKFLOW_SHEET_TAB %in% sheets) {
    googlesheets4::sheet_add(
      ss, sheet = WORKFLOW_SHEET_TAB
    )
    googlesheets4::sheet_write(
      empty_workflow_events(),
      ss, sheet = WORKFLOW_SHEET_TAB
    )
  }

  data <- tryCatch(
    googlesheets4::read_sheet(
      ss,
      sheet = WORKFLOW_SHEET_TAB,
      col_types = "cccccc",
      na = ""
    ),
    error = function(e) empty_workflow_events()
  )

  normalise_workflow_events(data)
}

append_workflow_events <- function(ss, events) {

  events <- normalise_workflow_events(events)

  if (nrow(events) == 0) {
    return(invisible(NULL))
  }

  googlesheets4::sheet_append(
    ss,
    events,
    sheet = WORKFLOW_SHEET_TAB
  )

  invisible(NULL)
}

make_workflow_events <- function(
    plot_id, action, value, session_id) {

  n <- length(plot_id)
  now <- Sys.time()

  data.frame(
    event_id = paste0(
      format(now, "%Y%m%d%H%M%OS6"),
      "-", Sys.getpid(), "-", seq_len(n)
    ),
    event_time = format(
      now + seq_len(n) / 1000,
      "%Y-%m-%dT%H:%M:%OS3Z",
      tz = "UTC"
    ),
    plot_id = as.character(plot_id),
    action = as.character(action),
    value = as.character(value),
    session_id = as.character(session_id),
    stringsAsFactors = FALSE
  )
}

legacy_log_to_events <- function(path, session_id = "migration") {

  legacy <- read_legacy_workflow_log(path)

  if (nrow(legacy) == 0) {
    return(empty_workflow_events())
  }

  events <- list()
  k <- 0L

  add_event <- function(plot_id, action, value) {
    k <<- k + 1L
    events[[k]] <<- data.frame(
      plot_id = plot_id,
      action = action,
      value = value,
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(legacy))) {

    x <- legacy[i, ]

    if (isTRUE(x$needs_team_review)) {
      add_event(x$plot_id, "team_review", "TRUE")
    }

    if (isTRUE(x$team_resolved)) {
      add_event(x$plot_id, "team_resolved", "TRUE")
    }

    n_corrections <- max(
      0L, x$expert_correction_count
    )

    if (n_corrections > 0) {
      for (j in seq_len(n_corrections)) {
        add_event(
          x$plot_id,
          "expert_status",
          "Correction needed"
        )
      }
    } else if (
      identical(x$expert_status, "Correction needed")
    ) {
      add_event(
        x$plot_id,
        "expert_status",
        "Correction needed"
      )
    }

    if (identical(x$expert_status, "Pass")) {
      add_event(x$plot_id, "expert_status", "Pass")
    }
  }

  if (length(events) == 0) {
    return(empty_workflow_events())
  }

  raw <- bind_rows(events)
  make_workflow_events(
    raw$plot_id,
    raw$action,
    raw$value,
    session_id
  )
}

events_to_workflow_log <- function(events, plot_ids) {

  ids <- union(
    as.character(plot_ids),
    unique(as.character(events$plot_id))
  )

  ids <- ids[nzchar(ids)]

  if (length(ids) == 0) {
    return(empty_workflow_log())
  }

  out <- empty_workflow_log()[
    rep(NA_integer_, length(ids)),
    ,
    drop = FALSE
  ]

  out$plot_id <- ids
  out$needs_team_review <- FALSE
  out$team_resolved <- FALSE
  out$expert_correction_count <- 0L

  for (col in setdiff(
    names(out),
    c(
      "plot_id", "needs_team_review",
      "team_resolved", "expert_correction_count"
    )
  )) {
    out[[col]] <- ""
  }

  if (nrow(events) == 0) {
    return(normalise_workflow_log(out))
  }

  for (i in seq_len(nrow(out))) {

    id <- out$plot_id[i]
    ev <- events %>% filter(plot_id == id)

    team <- ev %>% filter(action == "team_review")
    resolved <- ev %>% filter(action == "team_resolved")
    expert <- ev %>% filter(action == "expert_status")

    if (nrow(team) > 0) {
      out$needs_team_review[i] <-
        identical(tail(team$value, 1), "TRUE")
    }

    if (nrow(resolved) > 0) {
      out$team_resolved[i] <-
        identical(tail(resolved$value, 1), "TRUE")
    }

    if (nrow(expert) > 0) {
      out$expert_status[i] <- tail(expert$value, 1)
      out$expert_at[i] <- tail(expert$event_time, 1)
      out$expert_correction_count[i] <- sum(
        expert$value == "Correction needed"
      )
    }
  }

  normalise_workflow_log(out)
}


# Display helpers ----

escape_html <- function(x) {
  htmltools::htmlEscape(coalesce(as.character(x), ""))
}

fmt <- function(x, blank = "—") {
  x <- as.character(x)
  ifelse(is.na(x) | !nzchar(x), blank, x)
}

status_badge <- function(status) {

  status <- as.character(status)

  paste0(
    "<span class='status-inline'>",
    "<span class='status-dot' style='background:",
    status_colors[[status]], ";'></span>",
    escape_html(status),
    "</span>"
  )
}


team_review_control <- function(plot_id, status, needs_team_review) {

  status <- as.character(status)

  if (!status %in% c("Ready for review", "Team review")) {
    return("")
  }

  selected <- isTRUE(needs_team_review)

  immediate_js <- if (selected) {
    paste0(
      "this.style.background='#FFFFFF';",
      "this.style.color='#A3452F';",
      "this.textContent='Flag team review';"
    )
  } else {
    paste0(
      "this.style.background='#A3452F';",
      "this.style.color='#FFFFFF';",
      "this.textContent='Team review';"
    )
  }

  paste0(
    "<button type='button' class='btn btn-sm team-review-btn' ",
    "style='border:1px solid #A3452F;",
    if (selected) {
      "background:#A3452F;color:#FFFFFF;"
    } else {
      "background:#FFFFFF;color:#A3452F;"
    },
    "' onclick=\"event.stopPropagation(); ",
    immediate_js,
    "this.disabled=true;",
    "setTimeout(() => { this.disabled=false; }, 800);",
    "Shiny.setInputValue('toggle_team_review', '",
    plot_id,
    "', {priority: 'event'});\">",
    if (selected) "Team review" else "Flag team review",
    "</button>"
  )
}

resolved_control <- function(plot_id, status, needs_team_review,
                             team_resolved) {

  status <- as.character(status)

  if (status != "Team review" || !isTRUE(needs_team_review)) {
    return("")
  }

  selected <- isTRUE(team_resolved)

  immediate_js <- if (selected) {
    paste0(
      "this.style.background='#FFFFFF';",
      "this.style.color='#4E6A3E';"
    )
  } else {
    paste0(
      "this.style.background='#4E6A3E';",
      "this.style.color='#FFFFFF';"
    )
  }

  paste0(
    "<button type='button' class='btn btn-sm resolved-btn' ",
    "style='border:1px solid #4E6A3E;",
    if (selected) {
      "background:#4E6A3E;color:#FFFFFF;"
    } else {
      "background:#FFFFFF;color:#4E6A3E;"
    },
    "' onclick=\"event.stopPropagation(); ",
    immediate_js,
    "this.disabled=true;",
    "setTimeout(() => { this.disabled=false; }, 800);",
    "Shiny.setInputValue('toggle_team_resolved', '",
    plot_id,
    "', {priority: 'event'});\">Resolved</button>"
  )
}

plot_details <- function(data, primary_entries) {

  if (nrow(data) == 0) {
    return(setNames(character(), character()))
  }

  out <- vapply(seq_len(nrow(data)), function(i) {

    x <- data[i, ]
    p <- primary_entries %>%
      filter(id == x$plot_id) %>%
      arrange(observer)

    duplicate_note <- if (nrow(p) > 1) {
      paste0(" · ", nrow(p), " primary observers")
    } else {
      ""
    }

    summary <- paste0(
      "<p class='detail-summary'><strong>",
      escape_html(x$plot_id), "</strong>",
      " · ", escape_html(fmt(x$fire_name)),
      " · ", escape_html(fmt(x$location)),
      " · ", escape_html(as.character(x$status)),
      duplicate_note,
      "</p>"
    )

    primary_rows <- if (nrow(p) == 0) {
      paste0(
        "<tr class='text-muted'>",
        "<td>Primary</td><td colspan='7'>No saved primary record yet.</td>",
        "</tr>"
      )
    } else {
      paste0(
        vapply(seq_len(nrow(p)), function(j) {

          duplicate_class <- if (nrow(p) > 1) {
            " class='duplicate-primary'"
          } else {
            ""
          }

          paste0(
            "<tr>",
            "<td>Primary</td>",
            "<td", duplicate_class, ">",
            escape_html(fmt(p$observer[j])), "</td>",
            "<td>", escape_html(fmt(p$jt_pre_fire[j])), "</td>",
            "<td>", escape_html(fmt(p$jt_post_fire[j])), "</td>",
            "<td>", escape_html(fmt(p$pre_veg[j])), "</td>",
            "<td>", escape_html(fmt(p$post_veg[j])), "</td>",
            "<td>",
            ifelse(p$unsuitable[j], "Unsuitable", "Suitable"),
            "</td>",
            "<td class='cmt'>",
            escape_html(fmt(p$comments[j])), "</td>",
            "</tr>"
          )
        }, character(1)),
        collapse = ""
      )
    }

    reviewed_row <- if (isTRUE(x$reviewed_exists)) {
      paste0(
        "<tr>",
        "<td>Reviewed</td>",
        "<td>", escape_html(fmt(x$review_observer)), "</td>",
        "<td>", escape_html(fmt(x$pre_fire)), "</td>",
        "<td>", escape_html(fmt(x$post_fire)), "</td>",
        "<td>", escape_html(fmt(x$pre_veg)), "</td>",
        "<td>", escape_html(fmt(x$post_veg)), "</td>",
        "<td>", escape_html(fmt(x$review_suitability)), "</td>",
        "<td class='cmt'>", escape_html(fmt(x$review_comments)), "</td>",
        "</tr>"
      )
    } else {
      paste0(
        "<tr class='text-muted'>",
        "<td>Reviewed</td><td colspan='7'>No reviewed Collect Earth record yet.</td>",
        "</tr>"
      )
    }

    table <- paste0(
      "<table class='table table-sm detail-table'>",
      "<thead><tr>",
      "<th>Stage</th><th>Observer</th>",
      "<th>Pre-fire</th><th>Post-fire</th>",
      "<th>Veg pre</th><th>Veg post</th>",
      "<th>Suitable</th><th>Comments</th>",
      "</tr></thead><tbody>",
      primary_rows, reviewed_row,
      "</tbody></table>"
    )

    paste0(summary, table)
  }, character(1))

  setNames(out, data$plot_id)
}

expandable_datatable <- function(
    data, details, key_col = "Plot",
    empty_text = "No plots in this stage.",
    page_length = 20, selection = "single",
    raw_html_cols = c("Status", "Team review", "Resolved")) {

  if (nrow(data) == 0) {
    return(
      datatable(
        data.frame(Message = empty_text),
        rownames = FALSE, selection = "none",
        options = list(dom = "t")
      )
    )
  }

  data$.detail <- unname(details[data[[key_col]]])
  detail_col <- match(".detail", names(data))
  raw_cols <- match(raw_html_cols, names(data))
  raw_cols <- raw_cols[!is.na(raw_cols)]
  no_escape <- unique(c(raw_cols, detail_col))

  datatable(
    data, rownames = FALSE, selection = selection,
    escape = -no_escape,
    callback = JS(
      sprintf(
        "table.on('click', 'tbody tr', function(e) {
           var tr = $(this);
           var row = table.row(tr);
           if (!row.data()) return;
           if (row.child.isShown()) {
             row.child.hide();
             tr.removeClass('shown');
           } else {
             table.rows('.shown').every(function() {
               this.child.hide();
               $(this.node()).removeClass('shown');
             });
             row.child(row.data()[%d], 'expanded-detail').show();
             tr.next('tr').addClass('expanded-detail');
             tr.addClass('shown');
           }
         });",
        detail_col - 1
      )
    ),
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      order = list(),
      columnDefs = list(
        list(visible = FALSE, targets = detail_col - 1)
      )
    )
  )
}


# UI ----

theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#1F2622",
  primary = "#4E6A3E",
  secondary = "#C9A55A",
  base_font = font_google("Source Sans 3", local = FALSE),
  heading_font = font_google("Source Serif 4", local = FALSE),
  "border-radius" = "4px"
)

app_css <- "
.status-bar { display:flex; width:100%; border-radius:4px; overflow:hidden;
              margin-bottom:.45rem; }
.status-seg { flex:1; min-width:0; padding:20px 8px; text-align:center;
              cursor:pointer; font-size:1.05rem; user-select:none; }
.status-seg:hover { filter:brightness(1.06); }
.status-seg.active { box-shadow:inset 0 -5px 0 rgba(255,255,255,.95); }
.status-seg .label { display:block; font-weight:600; line-height:1.15; }
.status-seg .n { display:inline-block; min-width:2.4em; margin-top:.45rem;
                 padding:1px 8px; border:1.5px solid currentColor;
                 border-radius:999px; font-variant-numeric:tabular-nums; }
.status-note { color:#5f6660; font-size:.92rem; margin:.45rem 0 1rem; }
.status-inline {
  display:inline-flex;
  align-items:center;
  gap:.45rem;
  white-space:nowrap;
  font-size:.9rem;
  font-weight:600;
  color:#1F2622;
}
.status-dot {
  width:.62rem;
  height:.62rem;
  min-width:.62rem;
  border-radius:50%;
  display:inline-block;
}
.team-review-btn, .resolved-btn {
  white-space:nowrap; font-size:.82rem; padding:.2rem .5rem; font-weight:600;
}
.team-review-btn:disabled,
.resolved-btn:disabled {
  opacity:1;
}
.small-note { color:#666b67; }

/* Compact sidebar: related controls are grouped so the bslib sidebar does not
   insert a large gap between every individual element. */
.sidebar-block {
  margin:0;
}
.drive-status-block {
  display:flex;
  flex-direction:column;
  gap:.45rem;
}
.drive-status-title {
  font-weight:700;
  margin-bottom:.15rem;
}
.sidebar-help {
  color:#6B716D;
  font-size:.9rem;
  margin:.15rem 0 .75rem;
}
.sidebar-block .form-group {
  margin-bottom:.75rem;
}
.sidebar-block .checkbox {
  margin-top:.35rem;
  margin-bottom:0;
}
.sidebar-divider {
  margin:.35rem 0 .55rem;
}
.sidebar-filters .form-group:last-of-type {
  margin-bottom:.35rem;
}
#checks_table table.dataTable td:first-child,
#checks_table table.dataTable th:first-child {
  min-width:320px;
  white-space:normal;
}

#workflow_table table.dataTable td:nth-child(7),
#workflow_table table.dataTable th:nth-child(7) {
  min-width:190px;
}
.action-box { background:#f7f6f2; border:1px solid #ddd9cf;
              border-radius:4px; padding:15px; margin-top:14px; }
table.dataTable td { vertical-align:top; }
table.dataTable th { word-break:normal; overflow-wrap:normal; }
table.dataTable > tbody > tr { cursor:pointer; }
.detail-summary { margin:0 0 .6rem; }
.detail-workflow { margin:.65rem 0 0; color:#5f6660; }
.detail-table { margin:0; width:100%; }
.detail-table td, .detail-table th {
  padding:.3rem .7rem .3rem 0;
  vertical-align:top;
}
.detail-table th { white-space:nowrap; color:#5f6660; }
.detail-table td.cmt { min-width:300px; white-space:normal; }
.detail-table td.duplicate-primary { background-color:#F4DED6; font-weight:600; }
table.dataTable tbody > tr.shown > td {
  border-top:1px solid #B9BDBA;
}
table.dataTable tbody > tr.shown > td:first-child {
  border-left:1px solid #B9BDBA;
}
table.dataTable tbody > tr.shown > td:last-child {
  border-right:1px solid #B9BDBA;
}
table.dataTable tbody > tr.child > td,
table.dataTable tbody > tr.expanded-detail > td {
  background:#FFFFFF !important;
  border-left:1px solid #B9BDBA !important;
  border-right:1px solid #B9BDBA !important;
  border-bottom:1px solid #B9BDBA !important;
  padding:.9rem 1.2rem 1.1rem;
  cursor:default;
}
.detail-table tbody tr:hover > td,
.detail-table tbody tr:hover > th {
  background-color:transparent !important;
}
.about { max-width:48rem; padding:.5rem 0 2rem; line-height:1.55; }
.about h3 { margin-top:1.7rem; font-size:1.25rem; }

/* Keep expanded/selected rows visually neutral. Selection is still used
   internally for workflow actions, but it should not recolour the plot. */
table.dataTable tbody tr.selected > *,
table.dataTable tbody tr.selected:hover > *,
table.dataTable tbody tr.shown > *,
table.dataTable tbody tr.shown:hover > * {
  background-color:#FFFFFF !important;
  box-shadow:none !important;
  color:#1F2622 !important;
}

/* Expanded child rows and their nested comparison table stay white. */
table.dataTable tbody tr.child > td,
table.dataTable tbody tr.expanded-detail > td {
  background:#FFFFFF !important;
  color:#1F2622 !important;
  box-shadow:none !important;
}

table.dataTable tbody tr.expanded-detail table.detail-table,
table.dataTable tbody tr.expanded-detail table.detail-table tr,
table.dataTable tbody tr.expanded-detail table.detail-table td,
table.dataTable tbody tr.expanded-detail table.detail-table th {
  background:#FFFFFF !important;
  color:#1F2622 !important;
  box-shadow:none !important;
}

/* Preserve only the intentional warning highlight for duplicate primary
   observers; everything else in the expanded row remains neutral. */
table.dataTable tbody tr.expanded-detail table.detail-table td.duplicate-primary {
  background-color:#F4DED6 !important;
  font-weight:600;
}
"


ui <- page_navbar(
  title = "Joshua tree interpretation · review workflow",
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css))),
  sidebar = sidebar(
    width = 310,
    div(
      class = "sidebar-block",
      uiOutput("drive_status"),
      uiOutput("workflow_sync_status")
    ),
    div(
      class = "sidebar-block",
      fileInput(
        "files",
        "Add Collect Earth exports (optional)",
        multiple = TRUE,
        accept = ".csv"
      ),
      p(
        class = "sidebar-help",
        "Uploads are added for this session only."
      ),
      checkboxInput(
        "newest_only",
        "Use only each observer's newest export per stage",
        value = TRUE
      )
    ),
    tags$hr(class = "sidebar-divider"),
    div(
      class = "sidebar-block sidebar-filters",
      selectInput(
        "fire_filter",
        "Fires",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "primary_filter",
        "Primary observer / review batch",
        choices = NULL,
        multiple = TRUE
      ),
      p(
        class = "sidebar-help",
        "Leave filters empty to include all plots. In Ready for review,",
        "use Primary observer / review batch to isolate the plots assigned",
        "to your small group."
      )
    )
  ),

  nav_panel(
    "Overview",
    uiOutput("status_bar"),
    uiOutput("status_note"),
    card(
      card_header(textOutput("workflow_table_title", inline = TRUE)),
      DTOutput("workflow_table", fill = FALSE)
    )
  ),

  nav_panel(
    "Review summary",
    card(
      card_header("Progress by fire"),
      DTOutput("fire_progress_table", fill = FALSE)
    ),
    card(
      card_header("Review outcomes"),
      uiOutput("review_outcomes_note"),
      DTOutput("review_outcomes_table", fill = FALSE)
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Collect Earth files"),
        DTOutput("files_table", fill = FALSE)
      ),
      card(
        card_header("Progress by primary observer"),
        DTOutput("progress_table", fill = FALSE)
      )
    )
  ),

  nav_panel(
    "Data checks",
    card(
      card_header("Entries worth checking"),
      p(
        class = "small-note",
        "These checks identify workflow or data-entry issues worth fixing.",
        "Click a row to inspect the primary and reviewed records for that plot."
      ),
      DTOutput("checks_table", fill = FALSE)
    )
  ),

  nav_panel(
    "How it works",
    div(
      class = "about",
      h3("Primary interpretation"),
      p(
        "Primary observers pin trees in Google Earth and record imagery/date,",
        "notes and suitability in Collect Earth. Counts and vegetation cover",
        "are left blank. Primary exports do not contain '_reviewed' in the",
        "filename."
      ),
      h3("Joint review"),
      p(
        "Each plot is reviewed jointly, in weekly sessions, by two observers",
        "who did not perform the initial pinning. They use the primary notes",
        "shown in this app, check and correct the tree markers, confirm or",
        "revise plot suitability, classify vegetation cover, and record",
        "pre- and post-fire tree counts from the reviewed markers in their",
        "Collect Earth review survey. Difficult cases are flagged directly",
        "from the Team review column while the review is in progress."
      ),
      h3("Team review"),
      p(
        "Low-confidence, difficult, or novel cases are flagged for the full",
        "team (including an expert reviewer) to discuss before the session",
        "ends. Click Resolved directly in the table once the case is",
        "settled. Any resulting correction is made directly in the existing",
        "Collect Earth review survey and re-exported -- there is no separate",
        "expert sign-off step afterward."
      ),
      h3("Reviewed but incomplete / Reviewed and final"),
      p(
        "As soon as a reviewed Collect Earth survey is exported for a plot",
        "('_reviewed' in the filename), it moves to Reviewed and final --",
        "the app always reads the newest reviewed file, so a later",
        "correction and re-export simply updates it in place. If the",
        "reviewed record is missing a required field (suitability,",
        "vegetation cover, or a tree count), the plot shows as Reviewed",
        "but incomplete instead, until that field is filled in and",
        "re-exported."
      ),
      h3("Using the Overview"),
      p(
        "The large status bar filters the same expandable plot table. Click",
        "a status once to show only those plots and click it again to return",
        "to all plots. Click a row to expand the primary and reviewed records.",
        "CSV source filenames are kept only in Data checks for provenance and",
        "debugging."
      ),
      h3("Review summary"),
      p(
        "Review summary contains progress by fire, overall review outcomes,",
        "the Collect Earth source-file list, and progress by primary observer.",
        "These supporting tables are kept out of Overview so the main review",
        "workspace stays focused on plot-level decisions."
      ),
      h3("Working in two small groups"),
      p(
        "Use Primary observer / review batch in the sidebar to isolate the",
        "plots assigned to each group. Workflow actions are stored as an",
        "append-only event log in a shared Google Sheet in the same Drive",
        "folder, so two Shiny sessions can work at the same time without",
        "overwriting each other's progress. Shared workflow state refreshes",
        "about every 15 seconds."
      )
    )
  )
)

# Server ----

server <- function(input, output, session) {

  folder_state <- reactiveVal(empty_folder())
  folder_error <- reactiveVal(NULL)
  folder_checked <- reactiveVal(NULL)

  observe({

    input$refresh_drive
    source <- folder_source()

    if (source == "none") {
      return()
    }

    invalidateLater(
      if (source == "drive") drive_poll_ms else folder_poll_ms
    )

    result <- tryCatch(
      {
        if (source == "drive") {
          list_drive_files(DRIVE_FOLDER)
        } else {
          list_folder_files(DATA_FOLDER)
        }
      },
      error = function(e) e
    )

    if (inherits(result, "error")) {
      folder_error(conditionMessage(result))
    } else {
      folder_error(NULL)
      folder_state(result)
    }

    folder_checked(Sys.time())
  })

  folder_files <- reactive(folder_state()$exports)

  all_files <- reactive({

    uploaded <- if (is.null(input$files)) {
      NULL
    } else {
      data.frame(
        name = input$files$name,
        datapath = input$files$datapath,
        mtime = Sys.time(),
        stringsAsFactors = FALSE
      )
    }

    bind_rows(folder_files(), uploaded)
  })

  file_results <- reactive({

    files <- all_files()

    if (nrow(files) == 0) {
      return(list())
    }

    pmap(
      files,
      function(name, datapath, mtime) {

        result <- tryCatch(
          read_ce_file(datapath, name, mtime),
          error = function(e) e
        )

        if (inherits(result, "error")) {
          list(
            data = NULL,
            info = data.frame(
              File = name,
              Stage = "",
              Observer = "",
              `Saved plots` = NA_integer_,
              Status = conditionMessage(result),
              stringsAsFactors = FALSE
            )
          )
        } else {

          cleaned <- clean_reads(result)
          stage <- unique(result$stage)[1]
          observer <- unique(result$observer)[1]

          saved_n <- if (stage == "primary") {
            sum(cleaned$has_primary_entry, na.rm = TRUE)
          } else {
            sum(cleaned$has_review_entry, na.rm = TRUE)
          }

          list(
            data = result,
            info = data.frame(
              File = name,
              Stage = str_to_title(stage),
              Observer = observer,
              `Saved plots` = saved_n,
              Status = "OK",
              stringsAsFactors = FALSE
            )
          )
        }
      }
    )
  })

  raw_all <- reactive({

    data <- map(file_results(), "data")
    data <- compact(data)

    if (length(data) == 0) {
      return(empty_ce())
    }

    bind_rows(data)
  })

  file_info <- reactive({

    info <- map(file_results(), "info")

    if (length(info) == 0) {
      return(
        data.frame(
          File = character(),
          Stage = character(),
          Observer = character(),
          `Saved plots` = integer(),
          Status = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    bind_rows(info)
  })

  reads <- reactive({

    data <- clean_reads(raw_all())

    if (nrow(data) == 0 || !isTRUE(input$newest_only)) {
      return(data)
    }

    newest_files <- data %>%
      distinct(
        observer, stage, source_file, source_path, file_time
      ) %>%
      group_by(observer, stage) %>%
      arrange(
        desc(file_time), desc(source_file),
        .by_group = TRUE
      ) %>%
      slice(1) %>%
      ungroup() %>%
      pull(source_path)

    data %>%
      filter(source_path %in% newest_files)
  })

  workflow_sheet_id <- reactiveVal(NULL)
  workflow_events <- reactiveVal(empty_workflow_events())
  workflow_sync_error <- reactiveVal(NULL)
  workflow_checked <- reactiveVal(NULL)

  observe({

    invalidateLater(workflow_poll_ms)

    if (!nzchar(DRIVE_FOLDER)) {
      workflow_sync_error(
        "A Google Drive folder is required for shared workflow tracking."
      )
      return()
    }

    result <- tryCatch(
      {
        ss <- workflow_sheet_id()

        if (is.null(ss)) {
          ss <- ensure_workflow_sheet(DRIVE_FOLDER)
          workflow_sheet_id(ss)

          existing <- read_workflow_events(ss)

          if (nrow(existing) == 0) {
            migrated <- legacy_log_to_events(
              LEGACY_WORKFLOW_LOG
            )

            if (nrow(migrated) > 0) {
              append_workflow_events(ss, migrated)
            }
          }
        }

        read_workflow_events(ss)
      },
      error = function(e) e
    )

    if (inherits(result, "error")) {
      workflow_sync_error(conditionMessage(result))
    } else {
      workflow_sync_error(NULL)

      current_ids <- workflow_events()$event_id
      result_ids <- result$event_id

      if (!identical(current_ids, result_ids)) {
        workflow_events(result)
      }

      workflow_checked(Sys.time())
    }
  })

  plot_catalog <- reactive({

    data <- reads()

    if (nrow(data) == 0) {
      return(
        data.frame(
          plot_id = character(),
          plot_number = integer(),
          fire_name = character(),
          fire_year = integer(),
          location = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    data %>%
      group_by(id) %>%
      summarise(
        plot_number = first(
          plot_number[!is.na(plot_number)],
          default = NA_integer_
        ),
        fire_name = first(
          fire_name[!is.na(fire_name)],
          default = NA_character_
        ),
        fire_year = first(
          fire_year[!is.na(fire_year)],
          default = NA_integer_
        ),
        location = first(
          plot_location[!is.na(plot_location)],
          default = "Other"
        ),
        .groups = "drop"
      ) %>%
      rename(plot_id = id) %>%
      arrange(fire_name, plot_number, plot_id)
  })

  workflow_log <- reactive({
    events_to_workflow_log(
      workflow_events(),
      plot_catalog()$plot_id
    )
  })

  primary_saved <- reactive({
    latest_primary_records(reads())
  })

  reviewed_saved <- reactive({
    latest_record(reads(), "reviewed")
  })

  workflow <- reactive({

    catalog <- plot_catalog()

    if (nrow(catalog) == 0) {
      return(
        data.frame(
          plot_id = character(),
          plot_number = integer(),
          fire_name = character(),
          fire_year = integer(),
          location = character(),
          primary_observer = character(),
          primary_observer_n = integer(),
          review_observer = character(),
          status = factor(character(), levels = status_levels),
          stringsAsFactors = FALSE
        )
      )
    }

    primary <- primary_saved() %>%
      group_by(id) %>%
      summarise(
        primary_observer = paste(
          sort(unique(observer)),
          collapse = ", "
        ),
        primary_observer_n = n_distinct(observer),
        primary_file = paste(
          sort(unique(source_file)),
          collapse = "; "
        ),
        primary_suitability = case_when(
          all(unsuitable) ~ "Unsuitable",
          all(!unsuitable) ~ "Suitable",
          TRUE ~ "Mixed"
        ),
        primary_comments = paste(
          unique(
            str_c(observer[nzchar(comments)], ": ",
                  comments[nzchar(comments)])
          ),
          collapse = " | "
        ),
        primary_has_counts_or_veg = any(
          !is.na(jt_pre_fire) |
            !is.na(jt_post_fire) |
            !is.na(pre_veg) |
            !is.na(post_veg)
        ),
        .groups = "drop"
      ) %>%
      rename(plot_id = id)

    reviewed <- reviewed_saved() %>%
      transmute(
        plot_id = id,
        review_observer = observer,
        review_file = source_file,
        review_file_time = file_time,
        review_suitability = if_else(
          unsuitable, "Unsuitable", "Suitable"
        ),
        pre_fire = jt_pre_fire,
        post_fire = jt_post_fire,
        pre_veg,
        post_veg,
        review_comments = comments,
        review_complete = unsuitable |
          (
            !is.na(jt_pre_fire) & !is.na(jt_post_fire) &
              !is.na(pre_veg) & !is.na(post_veg)
          )
      )

    signatures <- record_signature(reviewed_saved())

    catalog %>%
      left_join(primary, by = "plot_id") %>%
      left_join(reviewed, by = "plot_id") %>%
      left_join(signatures, by = c("plot_id" = "id")) %>%
      left_join(workflow_log(), by = "plot_id") %>%
      mutate(
        across(
          c(
            needs_team_review, team_resolved
          ),
          ~ coalesce(.x, FALSE)
        ),
        across(
          c(
            team_reason, team_flag_note, team_outcome,
            team_resolution_note, team_review_signature,
            expert_status, expert_note, expert_signature,
            expert_at
          ),
          ~ coalesce(.x, "")
        ),
        expert_correction_count = coalesce(
          expert_correction_count, 0L
        ),
        primary_observer_n = coalesce(primary_observer_n, 0L),
        primary_has_counts_or_veg = coalesce(
          primary_has_counts_or_veg, FALSE
        ),
        primary_exists = primary_observer_n > 0,
        reviewed_exists = !is.na(review_observer),
        current_signature = coalesce(signature, ""),
        team_complete = !needs_team_review | team_resolved,
        status = case_when(
          needs_team_review &
            (!team_resolved | !reviewed_exists) ~ "Team review",
          reviewed_exists & !review_complete ~ "Reviewed but incomplete",
          reviewed_exists ~ "Reviewed and final",
          primary_exists ~ "Ready for review",
          TRUE ~ "Not started"
        ),
        status = factor(status, levels = status_levels)
      )
  })

  details <- reactive(plot_details(workflow(), primary_saved()))

  observe({

    data <- workflow()

    updateSelectInput(
      session, "fire_filter",
      choices = sort(unique(na.omit(data$fire_name))),
      selected = intersect(
        input$fire_filter %||% character(),
        sort(unique(na.omit(data$fire_name)))
      )
    )

    primary_choices <- sort(
      unique(na.omit(primary_saved()$observer))
    )

    updateSelectInput(
      session, "primary_filter",
      choices = primary_choices,
      selected = intersect(
        input$primary_filter %||% character(),
        primary_choices
      )
    )
  })

  selected_status <- reactiveVal(NULL)

  observeEvent(input$status_click, {

    if (identical(selected_status(), input$status_click)) {
      selected_status(NULL)
    } else {
      selected_status(input$status_click)
    }
  })

  base_filtered_workflow <- reactive({

    data <- workflow()

    if (length(input$fire_filter) > 0) {
      data <- data %>%
        filter(fire_name %in% input$fire_filter)
    }

    if (length(input$primary_filter) > 0) {

      primary_ids <- primary_saved() %>%
        filter(observer %in% input$primary_filter) %>%
        distinct(id) %>%
        pull(id)

      data <- data %>%
        filter(plot_id %in% primary_ids)
    }

    data
  })

  filtered_workflow <- reactive({

    data <- base_filtered_workflow()

    if (!is.null(selected_status())) {
      data <- data %>%
        filter(as.character(status) == selected_status())
    }

    data
  })

  output$drive_status <- renderUI({

    source <- folder_source()
    n_files <- nrow(folder_files())

    if (source == "none") {
      return(
        div(
          class = "drive-status-block",
          div(class = "sidebar-help", "No shared folder configured."),
          div(actionLink("refresh_drive", "Check now"))
        )
      )
    }

    if (!is.null(folder_error())) {
      return(
        div(
          class = "drive-status-block",
          div(
            class = "drive-status-title",
            if (source == "drive") {
              "Google Drive folder"
            } else {
              paste("Folder:", basename(DATA_FOLDER))
            }
          ),
          div(class = "text-danger", folder_error()),
          div(actionLink("refresh_drive", "Check again"))
        )
      )
    }

    div(
      class = "drive-status-block",
      div(
        class = "drive-status-title",
        if (source == "drive") {
          "Google Drive folder"
        } else {
          paste("Folder:", basename(DATA_FOLDER))
        }
      ),
      div(
        paste0(
          n_files, " Collect Earth export",
          if (n_files == 1) "" else "s"
        )
      ),
      div(
        class = "text-muted",
        if (is.null(folder_checked())) {
          "Checking..."
        } else {
          paste("Checked", format(folder_checked(), "%H:%M"))
        }
      ),
      div(
        actionLink("refresh_drive", "Check now")
      )
    )
  })

  output$workflow_sync_status <- renderUI({

    if (!is.null(workflow_sync_error())) {
      return(
        div(
          class = "sidebar-help text-danger",
          paste(
            "Shared workflow:",
            workflow_sync_error()
          )
        )
      )
    }

    if (is.null(workflow_sheet_id())) {
      return(
        div(
          class = "sidebar-help",
          "Shared workflow: connecting..."
        )
      )
    }

    div(
      class = "sidebar-help",
      paste0(
        "Shared workflow: Google Sheet · synced ",
        if (is.null(workflow_checked())) {
          "now"
        } else {
          format(workflow_checked(), "%H:%M:%S")
        }
      )
    )
  })


  output$status_bar <- renderUI({

    counts <- table(
      factor(
        as.character(base_filtered_workflow()$status),
        levels = status_levels
      )
    )

    data <- data.frame(
      status = status_levels,
      n = as.integer(counts),
      stringsAsFactors = FALSE
    )

    div(
      class = "status-bar",
      lapply(seq_len(nrow(data)), function(i) {

        status <- as.character(data$status[i])
        active <- identical(selected_status(), status)

        div(
          class = paste(
            "status-seg",
            if (active) "active" else ""
          ),
          style = paste0(
            "background:", status_colors[[status]], ";",
            "color:", status_text[[status]], ";"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('status_click', '%s', {priority: 'event'});",
            status
          ),
          span(class = "label", status),
          span(class = "n", data$n[i])
        )
      })
    )
  })

  output$status_note <- renderUI({

    if (is.null(selected_status())) {
      p(
        class = "status-note",
        "Showing all plots. Click a status above to filter the table."
      )
    } else {
      p(
        class = "status-note",
        paste0(
          "Showing ", selected_status(),
          ". Click the same status again to show all plots."
        )
      )
    }
  })

  output$workflow_table_title <- renderText({
    if (is.null(selected_status())) {
      "All plots"
    } else {
      selected_status()
    }
  })

  workflow_data <- reactive({

    filtered_workflow() %>%
      transmute(
        Plot = plot_id,
        Fire = fire_name,
        Location = location,
        `Primary observers` = primary_observer,
        `Primary entries` = primary_observer_n,
        Reviewer = review_observer,
        Status = vapply(
          as.character(status),
          status_badge,
          character(1)
        ),
        `Team review` = pmap_chr(
          list(
            plot_id, status, needs_team_review
          ),
          team_review_control
        ),
        Resolved = pmap_chr(
          list(
            plot_id, status, needs_team_review, team_resolved
          ),
          resolved_control
        )
      )
  })

  output$workflow_table <- renderDT({
    expandable_datatable(
      workflow_data(), details(),
      empty_text = if (is.null(selected_status())) {
        "No Collect Earth plots found."
      } else {
        paste("No plots currently have status:", selected_status())
      }
    )
  }, server = FALSE)

  output$files_table <- renderDT({

    datatable(
      file_info(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tip"
      )
    )
  })

  output$progress_table <- renderDT({

    data <- primary_saved() %>%
      distinct(id, observer) %>%
      rename(
        plot_id = id,
        Observer = observer
      ) %>%
      left_join(
        workflow() %>%
          select(
            plot_id,
            reviewed_exists,
            needs_team_review,
            status
          ),
        by = "plot_id"
      ) %>%
      group_by(Observer) %>%
      summarise(
        Primary = n(),
        `Team review` = sum(
          needs_team_review,
          na.rm = TRUE
        ),
        Reviewed = sum(
          reviewed_exists,
          na.rm = TRUE
        ),
        `Reviewed and final` = sum(
          as.character(status) == "Reviewed and final",
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    datatable(
      data,
      rownames = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE,
        ordering = TRUE
      )
    )
  })

  selected_plot_id <- reactiveVal(NULL)

  observeEvent(input$workflow_table_rows_selected, {

    i <- input$workflow_table_rows_selected

    if (length(i) == 1 && nrow(workflow_data()) >= i) {
      selected_plot_id(workflow_data()$Plot[i])
    }
  })

  observeEvent(
    list(selected_status(), input$fire_filter, input$primary_filter),
    {
      selected_plot_id(NULL)
    },
    ignoreInit = TRUE
  )

  append_workflow_event <- function(
      plot_id, action, value) {

    ss <- workflow_sheet_id()

    if (is.null(ss)) {
      showNotification(
        "Shared workflow sheet is not connected yet.",
        type = "error"
      )
      return(FALSE)
    }

    event <- make_workflow_events(
      plot_id,
      action,
      value,
      session$token
    )

    result <- tryCatch(
      {
        append_workflow_events(ss, event)
        TRUE
      },
      error = function(e) e
    )

    if (inherits(result, "error")) {
      workflow_sync_error(conditionMessage(result))
      showNotification(
        "Could not save shared workflow state.",
        type = "error"
      )
      return(FALSE)
    }

    workflow_sync_error(NULL)
    workflow_events(
      bind_rows(workflow_events(), event) %>%
        distinct(event_id, .keep_all = TRUE)
    )
    workflow_checked(Sys.time())
    TRUE
  }

  append_workflow_event_batch <- function(events) {

    ss <- workflow_sheet_id()

    if (is.null(ss)) {
      showNotification(
        "Shared workflow sheet is not connected yet.",
        type = "error"
      )
      return(FALSE)
    }

    result <- tryCatch(
      {
        append_workflow_events(ss, events)
        TRUE
      },
      error = function(e) e
    )

    if (inherits(result, "error")) {
      workflow_sync_error(conditionMessage(result))
      showNotification(
        "Could not save shared workflow state.",
        type = "error"
      )
      return(FALSE)
    }

    workflow_sync_error(NULL)
    workflow_events(
      bind_rows(workflow_events(), events) %>%
        distinct(event_id, .keep_all = TRUE)
    )
    workflow_checked(Sys.time())
    TRUE
  }

  observeEvent(input$toggle_team_review, {

    id <- input$toggle_team_review

    row <- workflow() %>%
      filter(plot_id == id) %>%
      slice(1)

    if (nrow(row) == 0) {
      return()
    }

    new_value <- !isTRUE(row$needs_team_review)

    if (new_value) {
      append_workflow_event(
        id, "team_review", "TRUE"
      )
    } else {
      events <- make_workflow_events(
        c(id, id),
        c("team_review", "team_resolved"),
        c("FALSE", "FALSE"),
        session$token
      )
      append_workflow_event_batch(events)
    }
  })

  observeEvent(input$toggle_team_resolved, {

    id <- input$toggle_team_resolved

    row <- workflow() %>%
      filter(plot_id == id) %>%
      slice(1)

    if (nrow(row) == 0 || !isTRUE(row$needs_team_review)) {
      return()
    }

    append_workflow_event(
      id,
      "team_resolved",
      if (isTRUE(row$team_resolved)) "FALSE" else "TRUE"
    )
  })


  fire_progress <- reactive({

    workflow() %>%
      group_by(fire_name) %>%
      summarise(
        Total = n(),
        `Ready for review` = sum(
          as.character(status) == "Ready for review"
        ),
        `Team review` = sum(
          as.character(status) == "Team review"
        ),
        Reviewed = sum(reviewed_exists),
        `Reviewed but incomplete` = sum(
          as.character(status) == "Reviewed but incomplete"
        ),
        `Reviewed and final` = sum(
          as.character(status) == "Reviewed and final"
        ),
        .groups = "drop"
      ) %>%
      rename(Fire = fire_name) %>%
      arrange(Fire)
  })

  output$fire_progress_table <- renderDT({

    datatable(
      fire_progress(),
      rownames = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE,
        ordering = TRUE
      )
    )
  })

  review_outcomes <- reactive({

    data <- workflow()

    reached_review <- data %>%
      filter(reviewed_exists | needs_team_review)

    reviewed <- data %>%
      filter(reviewed_exists)

    team_n <- sum(reached_review$needs_team_review)
    team_total <- nrow(reached_review)

    incomplete_n <- sum(as.character(reviewed$status) == "Reviewed but incomplete")
    incomplete_total <- nrow(reviewed)

    tibble(
      Outcome = c(
        "Needed team review",
        "Reviewed record incomplete"
      ),
      Plots = c(team_n, incomplete_n),
      `Plots assessed` = c(team_total, incomplete_total),
      Percent = c(
        if (team_total > 0) {
          round(100 * team_n / team_total, 1)
        } else {
          NA_real_
        },
        if (incomplete_total > 0) {
          round(100 * incomplete_n / incomplete_total, 1)
        } else {
          NA_real_
        }
      )
    ) %>%
      mutate(
        Percent = if_else(
          is.na(Percent),
          "",
          paste0(Percent, "%")
        )
      )
  })

  output$review_outcomes_note <- renderUI({

    data <- workflow()

    if (!any(data$reviewed_exists | data$needs_team_review)) {
      return(
        p(
          class = "small-note",
          "No plots have reached review yet."
        )
      )
    }

    NULL
  })

  output$review_outcomes_table <- renderDT({

    datatable(
      review_outcomes(),
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    )
  })

  checks <- reactive({

    data <- workflow()

    if (nrow(data) == 0) {
      return(
        data.frame(
          Check = character(),
          Plot = character(),
          Fire = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    reviewer_is_primary <- reviewed_saved() %>%
      select(id, review_observer = observer) %>%
      inner_join(
        primary_saved() %>%
          select(id, primary_observer = observer),
        by = "id"
      ) %>%
      filter(review_observer == primary_observer) %>%
      distinct(id) %>%
      pull(id)

    bind_rows(
      data %>%
        filter(primary_observer_n > 1) %>%
        transmute(
          Check = paste0(
            "Multiple primary observers: ",
            primary_observer
          ),
          Plot = plot_id,
          Fire = fire_name
        ),
      data %>%
        filter(
          primary_exists,
          primary_has_counts_or_veg
        ) %>%
        transmute(
          Check = "Primary record contains counts or vegetation",
          Plot = plot_id,
          Fire = fire_name
        ),
      data %>%
        filter(plot_id %in% reviewer_is_primary) %>%
        transmute(
          Check = "A primary observer also reviewed the plot",
          Plot = plot_id,
          Fire = fire_name
        ),
      data %>%
        filter(reviewed_exists, !primary_exists) %>%
        transmute(
          Check = "Reviewed record exists without a primary record",
          Plot = plot_id,
          Fire = fire_name
        )
    ) %>%
      left_join(
        workflow() %>%
          select(
            plot_id,
            `Primary CSV` = primary_file,
            `Reviewed CSV` = review_file
          ),
        by = c("Plot" = "plot_id")
      ) %>%
      arrange(Check, Fire, Plot)
  })

  output$checks_table <- renderDT({

    data <- checks()

    if (nrow(data) == 0) {
      return(
        datatable(
          data.frame(
            Message = "No data checks currently need attention."
          ),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        )
      )
    }

    data$.detail <- unname(details()[data$Plot])
    detail_col <- match(".detail", names(data))

    datatable(
      data,
      rownames = FALSE,
      selection = "none",
      escape = FALSE,
      callback = JS(
        sprintf(
          "table.on('click', 'tbody tr', function(e) {
             var tr = $(this);
             var row = table.row(tr);
             if (!row.data()) return;
             if (row.child.isShown()) {
               row.child.hide();
               tr.removeClass('shown');
             } else {
               table.rows('.shown').every(function() {
                 this.child.hide();
                 $(this.node()).removeClass('shown');
               });
               row.child(
                 row.data()[%d],
                 'expanded-detail'
               ).show();
               tr.addClass('shown');
             }
           });",
          detail_col - 1
        )
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = FALSE,
        order = list(),
        columnDefs = list(
          list(
            visible = FALSE,
            targets = detail_col - 1
          ),
          list(
            width = "34%",
            targets = 0
          ),
          list(
            width = "18%",
            targets = 1
          ),
          list(
            width = "12%",
            targets = 2
          ),
          list(
            width = "18%",
            targets = 3
          ),
          list(
            width = "18%",
            targets = 4
          )
        )
      )
    )
  }, server = FALSE)
}

shinyApp(ui, server)
