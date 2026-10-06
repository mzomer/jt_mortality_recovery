# ─────────────────────────────────────────────────────────────────────
# 14_build_consensus.R — Build final Joshua tree consensus dataset
#
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

library(tidyverse)


# Setup ----

primary_files <- c(
  observer_1 = here::here("data", "final", "observer1_collectedData.csv"),
  observer_2 = here::here("data", "final", "observer2_collectedData.csv"),
  observer_3 = here::here("data", "final", "observer3_collectedData.csv")
)

fourth_files <- c(
  reviewer_1 = here::here("data", "final", "reviewer1_collectedData.csv"),
  reviewer_2 = here::here("data", "final", "reviewer2_collectedData.csv")
)

joint_review_file <- here::here(
  "shiny", "jt_agreement_app", "joint_reviews.csv"
)

output_dir <- here::here("output", "analysis")

abs_tol <- 2
rel_tol <- 0.20
low_conf_min <- 2
veg_levels <- c("low", "medium", "high")


# Functions ----

read_export <- function(file, observer) {

  data <- read_csv(file, show_col_types = FALSE, na = c("", "NA"))
  optional <- c("comments", "location_x", "location_y")
  data[setdiff(optional, names(data))] <- NA

  data %>%
    transmute(
      plot_id = as.character(id), observer,
      pre_fire = as.numeric(jt_pre_fire),
      post_fire = as.numeric(jt_post_fire),
      pre_veg = str_to_lower(str_trim(pre_vegetation_cover)),
      post_veg = str_to_lower(str_trim(post_vegetation_cover)),
      pre_confidence = str_to_lower(str_trim(pre_confidence)),
      post_confidence = str_to_lower(str_trim(post_confidence)),
      unsuitable = str_detect(
        str_to_lower(coalesce(plot_unsuitable, "")), "unsuit"
      ),
      comments, longitude = as.numeric(location_x),
      latitude = as.numeric(location_y)
    )
}

count_consensus <- function(x) {

  x <- x[!is.na(x)]

  if (length(x) < 3) {
    return(NA_real_)
  }

  trios <- combn(x, 3, simplify = FALSE)
  agrees <- map_lgl(trios, \(x) {
    max(x) - min(x) <= max(abs_tol, rel_tol * median(x))
  })

  if (!any(agrees)) {
    return(NA_real_)
  }

  trios <- trios[agrees]
  spreads <- map_dbl(trios, \(x) max(x) - min(x))

  median(trios[[which.min(spreads)]])
}

veg_consensus <- function(x) {

  x <- x[!is.na(x)]

  if (length(x) < 2) {
    return(NA_character_)
  }

  counts <- table(x)

  if (max(counts) < 2 || sum(counts == max(counts)) > 1) {
    return(NA_character_)
  }

  names(counts)[which.max(counts)]
}


# Read observer data ----

primary <- imap_dfr(primary_files, read_export)

primary_duplicates <- primary %>%
  count(observer, plot_id) %>%
  filter(n > 1)

if (nrow(primary_duplicates) > 0) {
  stop("Duplicate observer-plot rows in the primary files.")
}

incomplete_plots <- primary %>%
  count(plot_id) %>%
  filter(n != 3)

if (nrow(incomplete_plots) > 0) {
  stop("Not every plot has three primary observations.")
}

plot_info <- primary %>%
  group_by(plot_id) %>%
  summarise(
    pre_low_n = sum(pre_confidence == "low", na.rm = TRUE),
    post_low_n = sum(post_confidence == "low", na.rm = TRUE),
    longitude = first(na.omit(longitude), default = NA_real_),
    latitude = first(na.omit(latitude), default = NA_real_),
    comments = str_c(
      unique(comments[!is.na(comments) & comments != ""]),
      collapse = " | "
    ),
    .groups = "drop"
  )

primary <- primary %>%
  select(
    plot_id, observer, pre_fire, post_fire, pre_veg, post_veg, unsuitable
  ) %>%
  pivot_wider(
    names_from = observer,
    values_from = c(pre_fire, post_fire, pre_veg, post_veg, unsuitable),
    names_glue = "{.value}_{observer}"
  )


# Check primary data ----

primary <- primary %>%
  mutate(
    n_unsuitable = rowSums(across(starts_with("unsuitable_"))),
    excluded = n_unsuitable >= 2
  )

suitability_split <- primary %>%
  filter(n_unsuitable == 1)

if (nrow(suitability_split) > 0) {
  stop("Some plots still have unresolved suitability reviews.")
}

missing_primary <- primary %>%
  filter(
    !excluded,
    if_any(
      matches("^(pre_fire|post_fire|pre_veg|post_veg)_"),
      is.na
    )
  )

if (nrow(missing_primary) > 0) {
  stop("Some suitable plots still have missing primary-observer values.")
}

bad_primary_veg <- primary %>%
  select(plot_id, starts_with("pre_veg_"), starts_with("post_veg_")) %>%
  pivot_longer(-plot_id, values_to = "veg") %>%
  filter(!is.na(veg), !veg %in% veg_levels)

if (nrow(bad_primary_veg) > 0) {
  stop("Invalid vegetation class in the primary files.")
}


# Primary-observer consensus ----

primary <- primary %>%
  rowwise() %>%
  mutate(
    pre_primary = if_else(
      excluded, NA_real_,
      count_consensus(c_across(starts_with("pre_fire_")))
    ),
    post_primary = if_else(
      excluded, NA_real_,
      count_consensus(c_across(starts_with("post_fire_")))
    ),
    pre_veg_primary = if (excluded) {
      NA_character_
    } else {
      veg_consensus(c_across(starts_with("pre_veg_")))
    },
    post_veg_primary = if (excluded) {
      NA_character_
    } else {
      veg_consensus(c_across(starts_with("post_veg_")))
    }
  ) %>%
  ungroup()


# Fourth review ----

fourth <- imap_dfr(fourth_files, read_export) %>%
  filter(
    !is.na(pre_fire) | !is.na(post_fire) |
      !is.na(pre_veg) | !is.na(post_veg)
  )

fourth_duplicates <- fourth %>%
  count(plot_id) %>%
  filter(n > 1)

if (nrow(fourth_duplicates) > 0) {
  stop("More than one fourth reviewer completed the same plot.")
}

bad_fourth_veg <- fourth %>%
  select(plot_id, pre_veg, post_veg) %>%
  pivot_longer(c(pre_veg, post_veg), values_to = "veg") %>%
  filter(!is.na(veg), !veg %in% veg_levels)

if (nrow(bad_fourth_veg) > 0) {
  stop("Invalid vegetation class in a fourth-review file.")
}

fourth <- fourth %>%
  transmute(
    plot_id, fourth_reviewer = observer,
    fourth_pre = pre_fire, fourth_post = post_fire,
    fourth_pre_veg = pre_veg, fourth_post_veg = post_veg,
    fourth_pre_low = pre_confidence == "low",
    fourth_post_low = post_confidence == "low"
  )


# Automatic consensus ----

# Fourth-review values are used only where the primary three did not agree.

consensus <- primary %>%
  left_join(fourth, by = "plot_id") %>%
  left_join(plot_info, by = "plot_id") %>%
  rowwise() %>%
  mutate(
    pre_fire = if (!is.na(pre_primary)) {
      pre_primary
    } else {
      count_consensus(c(c_across(starts_with("pre_fire_")), fourth_pre))
    },
    post_fire = if (!is.na(post_primary)) {
      post_primary
    } else {
      count_consensus(c(c_across(starts_with("post_fire_")), fourth_post))
    },
    pre_veg = if (!is.na(pre_veg_primary)) {
      pre_veg_primary
    } else {
      veg_consensus(c(c_across(starts_with("pre_veg_")), fourth_pre_veg))
    },
    post_veg = if (!is.na(post_veg_primary)) {
      post_veg_primary
    } else {
      veg_consensus(c(c_across(starts_with("post_veg_")), fourth_post_veg))
    },
    pre_source = case_when(
      excluded ~ NA_character_,
      !is.na(pre_primary) ~ "primary_3",
      !is.na(pre_fire) ~ "fourth_review",
      TRUE ~ "joint_review"
    ),
    post_source = case_when(
      excluded ~ NA_character_,
      !is.na(post_primary) ~ "primary_3",
      !is.na(post_fire) ~ "fourth_review",
      TRUE ~ "joint_review"
    ),
    pre_veg_source = case_when(
      excluded ~ NA_character_,
      !is.na(pre_veg_primary) ~ "primary_3",
      !is.na(pre_veg) ~ "fourth_review",
      TRUE ~ "joint_review"
    ),
    post_veg_source = case_when(
      excluded ~ NA_character_,
      !is.na(post_veg_primary) ~ "primary_3",
      !is.na(post_veg) ~ "fourth_review",
      TRUE ~ "joint_review"
    ),
    status = case_when(
      excluded ~ "Excluded",
      !is.na(pre_fire) & !is.na(post_fire) &
        !is.na(pre_veg) & !is.na(post_veg) ~ "Accepted",
      TRUE ~ "Conflict"
    )
  ) %>%
  ungroup() %>%
  mutate(
    pre_low_n = pre_low_n + coalesce(
      as.integer(is.na(pre_primary) & fourth_pre_low), 0L
    ),
    post_low_n = post_low_n + coalesce(
      as.integer(is.na(post_primary) & fourth_post_low), 0L
    )
  )

missing_fourth <- consensus %>%
  filter(
    !excluded,
    (is.na(pre_primary) & is.na(fourth_pre)) |
      (is.na(post_primary) & is.na(fourth_post)) |
      (is.na(pre_veg_primary) & is.na(fourth_pre_veg)) |
      (is.na(post_veg_primary) & is.na(fourth_post_veg))
  )

if (nrow(missing_fourth) > 0) {
  stop("Some unresolved fields still need a fourth review.")
}

accepted <- consensus %>%
  filter(status == "Accepted")

excluded <- consensus %>%
  filter(status == "Excluded")

conflicts <- consensus %>%
  filter(status == "Conflict")


# Joint review ----

# Joint review fills unresolved values but cannot change resolved values.

if (nrow(conflicts) > 0) {

  if (!file.exists(joint_review_file)) {
    stop("Conflict plots remain but joint_reviews.csv does not exist.")
  }

  joint_review <- read_csv(
    joint_review_file, show_col_types = FALSE, na = c("", "NA")
  )

  if (!"note" %in% names(joint_review)) {
    joint_review$note <- NA_character_
  }

  joint_review <- joint_review %>%
    transmute(
      plot_id = as.character(plot_id),
      joint_pre = as.numeric(pre_fire),
      joint_post = as.numeric(post_fire),
      joint_pre_veg = str_to_lower(str_trim(pre_veg)),
      joint_post_veg = str_to_lower(str_trim(post_veg)),
      review_note = note
    )

  joint_duplicates <- joint_review %>%
    count(plot_id) %>%
    filter(n > 1)

  if (nrow(joint_duplicates) > 0) {
    stop("Duplicate plots found in joint_reviews.csv.")
  }

  bad_joint_veg <- joint_review %>%
    select(plot_id, joint_pre_veg, joint_post_veg) %>%
    pivot_longer(-plot_id, values_to = "veg") %>%
    filter(!is.na(veg), !veg %in% veg_levels)

  if (nrow(bad_joint_veg) > 0) {
    stop("Invalid vegetation class in joint_reviews.csv.")
  }

  missing_reviews <- conflicts %>%
    anti_join(joint_review, by = "plot_id")

  if (nrow(missing_reviews) > 0) {
    stop("Some conflict plots are missing from joint_reviews.csv.")
  }

  reviewed <- conflicts %>%
    left_join(joint_review, by = "plot_id")

  changed_values <- reviewed %>%
    filter(
      (!is.na(pre_fire) &
         (is.na(joint_pre) | pre_fire != joint_pre)) |
        (!is.na(post_fire) &
           (is.na(joint_post) | post_fire != joint_post)) |
        (!is.na(pre_veg) &
           (is.na(joint_pre_veg) | pre_veg != joint_pre_veg)) |
        (!is.na(post_veg) &
           (is.na(joint_post_veg) | post_veg != joint_post_veg))
    )

  if (nrow(changed_values) > 0) {
    stop("Joint review changed an already-resolved value.")
  }

  incomplete_reviews <- reviewed %>%
    filter(
      is.na(joint_pre) | is.na(joint_post) |
        is.na(joint_pre_veg) | is.na(joint_post_veg)
    )

  if (nrow(incomplete_reviews) > 0) {
    stop("Some joint reviews are incomplete.")
  }

  reviewed <- reviewed %>%
    mutate(
      pre_source = if_else(is.na(pre_fire), "joint_review", pre_source),
      post_source = if_else(is.na(post_fire), "joint_review", post_source),
      pre_veg_source = if_else(
        is.na(pre_veg), "joint_review", pre_veg_source
      ),
      post_veg_source = if_else(
        is.na(post_veg), "joint_review", post_veg_source
      )
    ) %>%
    transmute(
      plot_id, status = "Accepted",
      pre_fire = joint_pre, post_fire = joint_post,
      pre_veg = joint_pre_veg, post_veg = joint_post_veg,
      pre_source, post_source, pre_veg_source, post_veg_source,
      fourth_reviewer, pre_low_n, post_low_n, longitude, latitude,
      comments, review_note
    )

} else {

  reviewed <- tibble()
}


# Final dataset ----

accepted <- accepted %>%
  transmute(
    plot_id, status, pre_fire, post_fire, pre_veg, post_veg,
    pre_source, post_source, pre_veg_source, post_veg_source,
    fourth_reviewer, pre_low_n, post_low_n, longitude, latitude,
    comments, review_note = NA_character_
  )

excluded <- excluded %>%
  transmute(
    plot_id, status,
    pre_fire = NA_real_, post_fire = NA_real_,
    pre_veg = NA_character_, post_veg = NA_character_,
    pre_source = NA_character_, post_source = NA_character_,
    pre_veg_source = NA_character_, post_veg_source = NA_character_,
    fourth_reviewer = NA_character_,
    pre_low_n, post_low_n, longitude, latitude, comments,
    review_note = NA_character_
  )

final <- bind_rows(accepted, reviewed, excluded) %>%
  mutate(
    fire_name = str_match(plot_id, "^(.+?)_(\\d{4})_")[, 2] %>%
      str_replace_all("_", " ") %>%
      str_to_upper(),
    fire_year = as.integer(str_match(plot_id, "_(\\d{4})_")[, 2]),
    location = case_when(
      str_detect(plot_id, "_inside_") ~ "Inside",
      str_detect(plot_id, "_outside_") ~ "Outside",
      TRUE ~ "Other"
    ),
    mortality_pct = if_else(
      status == "Accepted" & location == "Inside" & pre_fire > 0,
      (pre_fire - post_fire) / pre_fire * 100,
      NA_real_
    ),
    pre_low_confidence = pre_low_n >= low_conf_min,
    post_low_confidence = post_low_n >= low_conf_min
  ) %>%
  select(
    plot_id, fire_name, fire_year, location, status,
    pre_fire, post_fire, pre_veg, post_veg, mortality_pct,
    pre_source, post_source, pre_veg_source, post_veg_source,
    fourth_reviewer, pre_low_confidence, post_low_confidence,
    longitude, latitude, comments, review_note
  ) %>%
  arrange(fire_name, plot_id)


# Final checks ----

final_duplicates <- final %>%
  count(plot_id) %>%
  filter(n > 1)

if (nrow(final_duplicates) > 0) {
  stop("Duplicate plots found in the final dataset.")
}

if (nrow(final) != nrow(primary)) {
  stop("The final dataset does not contain the expected number of plots.")
}

incomplete_final <- final %>%
  filter(
    status == "Accepted",
    is.na(pre_fire) | is.na(post_fire) |
      is.na(pre_veg) | is.na(post_veg)
  )

if (nrow(incomplete_final) > 0) {
  stop("Accepted plots still contain missing final values.")
}


# Export ----

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  final,
  file.path(output_dir, "jt_consensus_all_plots.csv"),
  na = ""
)

final %>%
  filter(status == "Accepted") %>%
  write_csv(
    file.path(output_dir, "jt_analysis.csv"),
    na = ""
  )