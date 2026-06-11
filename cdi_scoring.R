# =============================================================================
# CDI Scoring Functions
# Children's Depression Inventory (original 27-item) & CDI 2 (28-item)
# Tidyverse-style implementation
#
# References:
#   Kovacs, M. (1992). Children's Depression Inventory (CDI) manual.
#     Multi-Health Systems.
#   Kovacs, M., & MHS Staff. (2011). Children's Depression Inventory 2nd
#     edition (CDI 2) technical manual. Multi-Health Systems.
#   Jelínek, M., Květon, P., Burešová, I., & Klimusová, H. (2021).
#     PLoS ONE, 16(4), e0249943. https://doi.org/10.1371/journal.pone.0249943
# =============================================================================

library(tidyverse)

# ---------------------------------------------------------------------------
# 1. Item-to-subscale mapping (original CDI, 27 items)
# ---------------------------------------------------------------------------
# Based on Kovacs (1992) as published in Jelínek et al. (2021), Table 2.

cdi_item_map <- tibble::tribble(
  ~item_id,    ~symptom,              ~subscale,
  "CDI_01",    "sadness",             "Negative_Mood",
  "CDI_02",    "pessimism",           "Negative_Self_Esteem",
  "CDI_03",    "self_criticism",      "Ineffectiveness",
  "CDI_04",    "anhedonia",           "Anhedonia",
  "CDI_05",    "misbehavior",         "Interpersonal_Problems",
  "CDI_06",    "pessimistic_worry",   "Negative_Mood",
  "CDI_07",    "self_hate",           "Negative_Self_Esteem",
  "CDI_08",    "self_blame",          "Negative_Mood",
  "CDI_09",    "suicidal_ideation",   "Negative_Self_Esteem",
  "CDI_10",    "tearfulness",         "Negative_Mood",
  "CDI_11",    "irritability",        "Negative_Mood",
  "CDI_12",    "reduced_social_interest", "Interpersonal_Problems",
  "CDI_13",    "indecisiveness",      "Negative_Mood",
  "CDI_14",    "negative_body_image", "Negative_Self_Esteem",
  "CDI_15",    "school_amotivation",  "Ineffectiveness",
  "CDI_16",    "sleep_disturbance",   "Anhedonia",
  "CDI_17",    "fatigue",             "Anhedonia",
  "CDI_18",    "reduced_appetite",    "Anhedonia",
  "CDI_19",    "somatic_concerns",    "Anhedonia",
  "CDI_20",    "loneliness",          "Anhedonia",
  "CDI_21",    "school_dislike",      "Anhedonia",
  "CDI_22",    "few_friends",         "Anhedonia",
  "CDI_23",    "academic_decline",    "Ineffectiveness",
  "CDI_24",    "negative_peer_comparison", "Ineffectiveness",
  "CDI_25",    "feeling_unloved",     "Negative_Self_Esteem",
  "CDI_26",    "disobedience",        "Interpersonal_Problems",
  "CDI_27",    "fighting",            "Interpersonal_Problems"
)

# Named list mapping subscale -> vector of item IDs for the original CDI
cdi_subscale_items <- split(cdi_item_map$item_id, cdi_item_map$subscale)

# ---------------------------------------------------------------------------
# 2. Item-to-scale/subscale mapping (CDI 2, 28 items)
# ---------------------------------------------------------------------------
# The CDI 2 uses 28 items forming 2 scales and 4 subscales.
# The exact item-to-subscale assignment is proprietary (CDI 2 manual).
# Below we provide the subscale structure and item counts.
# Replace the item_id placeholders with your actual item column names.

cdi2_subscale_map <- tibble::tribble(
  ~subscale,                    ~scale,                ~n_items,
  "Negative_Mood_Physical",     "Emotional_Problems",  9L,
  "Negative_Self_Esteem",       "Emotional_Problems",  6L,
  "Interpersonal_Problems",     "Functional_Problems", 5L,
  "Ineffectiveness",            "Functional_Problems", 8L
)

# Helper: scale composition from subscales
cdi2_scale_map <- cdi2_subscale_map %>%
  distinct(scale, subscale)

# ---------------------------------------------------------------------------
# 3. Utility: build scoring key from a data frame mapping
# ---------------------------------------------------------------------------

#' Build a scoring key list from a mapping data frame
#'
#' @param map_df A data frame with columns: item_id, score_name
#'   (where score_name is the subscale or scale name).
#' @return A named list mapping score_name -> character vector of item IDs.
build_scoring_key <- function(map_df) {
  split(map_df$item_id, map_df$score_name)
}

# ---------------------------------------------------------------------------
# 4. Main scoring function
# ---------------------------------------------------------------------------

#' Score the CDI (original or CDI 2)
#'
#' @param data       A data frame containing item responses.
#' @param item_cols  Character vector of column names for the CDI items.
#' @param scoring_key A named list mapping score_name -> item_cols subset.
#'   If NULL, the original CDI 27-item key is used.
#' @param scale_map  Optional: a named list mapping scale_name -> subscale names
#'   to aggregate subscales into broader scales (for CDI 2).
#' @param max_missing_prop Maximum proportion of missing items allowed per
#'   subscale before proration is disabled (returns NA). Default = 0.2.
#' @param id_col     Optional column name for a respondent identifier.
#' @param cutoff_community  Cutoff score for community screening (default 19).
#' @param cutoff_clinical   Cutoff score for clinic-referred (default 13).
#' @param reverse_items     Optional character vector of item names to reverse.
#'   All CDI items are scored 0-2 with 0 = absence, 2 = definite symptom.
#'   Some administrations may require reversal depending on item phrasing.
#'
#' @return A data frame with one row per respondent, containing:
#'   - id_col (if provided)
#'   - one column per subscale (sum scores)
#'   - one column per scale (sum scores, if scale_map provided)
#'   - total_score
#'   - total_score_prorated
#'   - n_missing
#'   - screening_result
#'   - severity_category
#'
#' @examples
#' \dontrun{
#'   # Score original CDI with default 27-item key
#'   scores <- score_cdi(my_data,
#'     item_cols = paste0("CDI_", sprintf("%02d", 1:27)),
#'     id_col = "subject_id")
#'
#'   # Score CDI 2 with custom scoring key
#'   cdi2_key <- list(
#'     Negative_Mood_Physical    = c("CDI2_01", "CDI2_02", ...),
#'     Negative_Self_Esteem      = c("CDI2_03", "CDI2_04", ...),
#'     Interpersonal_Problems    = c(...),
#'     Ineffectiveness           = c(...)
#'   )
#'   scores <- score_cdi(my_data, item_cols = item_names,
#'                       scoring_key = cdi2_key, scale_map = cdi2_scale_map)
#' }
score_cdi <- function(data,
                      item_cols,
                      scoring_key = NULL,
                      scale_map = NULL,
                      max_missing_prop = 0.2,
                      id_col = NULL,
                      cutoff_community = 19,
                      cutoff_clinical = 13,
                      reverse_items = NULL) {

  # ---- Default to original CDI 27-item key if none provided ----
  if (is.null(scoring_key)) {
    scoring_key <- cdi_subscale_items
  }

  # ---- Validate item columns exist ----
  missing_items <- setdiff(unlist(scoring_key), colnames(data))
  if (length(missing_items) > 0) {
    stop("The following item columns are missing from data: ",
         paste(missing_items, collapse = ", "))
  }

  # ---- Validate that item_cols matches scoring_key items ----
  all_key_items <- unique(unlist(scoring_key))
  if (missing(id_col)) {
    if (!setequal(item_cols, all_key_items)) {
      stop("item_cols must exactly match the items in scoring_key")
    }
  }

  # ---- Reverse items if needed ----
  data <- data %>%
    mutate(across(all_of(intersect(reverse_items, item_cols)),
                  ~ 2 - .x))

  # ---- Compute subscale scores with missing data handling ----
  subscale_scores <- map_dfc(names(scoring_key), function(ss_name) {
    ss_items <- scoring_key[[ss_name]]
    n_ss <- length(ss_items)
    max_missing <- floor(n_ss * max_missing_prop)

    scores <- data %>%
      select(all_of(ss_items)) %>%
      rowwise() %>%
      mutate(
        n_missing_ss = sum(is.na(c_across(everything()))),
        ss_sum = if_else(
          n_missing_ss <= max_missing,
          sum(c_across(all_of(ss_items)), na.rm = TRUE) *
            (n_ss / (n_ss - n_missing_ss)),
          NA_real_
        )
      ) %>%
      pull(ss_sum)

    tibble(!!ss_name := scores)
  })

  # ---- Compute scale scores by aggregating subscales ----
  scale_scores <- NULL
  if (!is.null(scale_map)) {
    scale_scores <- map_dfc(names(scale_map), function(sc_name) {
      subscales_in_scale <- scale_map[[sc_name]]
      sc_score <- rowSums(
        subscale_scores[, subscales_in_scale, drop = FALSE],
        na.rm = FALSE
      )
      # NA if any constituent subscale is NA
      sc_score <- if_else(
        rowSums(is.na(subscale_scores[, subscales_in_scale, drop = FALSE])) > 0,
        NA_real_,
        sc_score
      )
      tibble(!!sc_name := sc_score)
    })
  }

  # ---- Compute total score ----
  all_items_df <- data %>% select(all_of(item_cols))
  n_total <- length(item_cols)
  max_missing_total <- floor(n_total * max_missing_prop)

  total_scores <- all_items_df %>%
    rowwise() %>%
    mutate(
      n_missing = sum(is.na(c_across(everything()))),
      total_raw = sum(c_across(everything()), na.rm = TRUE),
      total_prorated = if_else(
        n_missing <= max_missing_total,
        total_raw * (n_total / (n_total - n_missing)),
        NA_real_
      )
    ) %>%
    ungroup() %>%
    select(n_missing, total_raw, total_prorated)

  # ---- Assemble output ----
  result <- bind_cols(
    # ID column
    if (!is.null(id_col)) data %>% select(all_of(id_col)),
    # Subscale scores
    subscale_scores,
    # Scale scores
    if (!is.null(scale_scores)) scale_scores,
    # Total scores
    total_scores
  )

  # ---- Screening classification ----
  # Use prorated total if available, otherwise raw total
  result <- result %>%
    mutate(
      total_score = if_else(!is.na(total_prorated),
                            total_prorated, total_raw),
      screening_result = case_when(
        is.na(total_score) ~ NA_character_,
        total_score >= cutoff_community ~ "Community_screen_positive",
        total_score >= cutoff_clinical ~ "Clinical_elevated",
        TRUE ~ "Within_normal_limits"
      ),
      severity_category = case_when(
        is.na(total_score) ~ NA_character_,
        total_score >= 25 ~ "Severe",
        total_score >= 20 ~ "Moderate",
        total_score >= 15 ~ "Mild",
        TRUE ~ "Minimal"
      )
    )

  # ---- Add interpretive notes ----
  attr(result, "scoring_key") <- scoring_key
  attr(result, "scale_map") <- scale_map
  attr(result, "max_missing_prop") <- max_missing_prop
  attr(result, "cutoff_community") <- cutoff_community
  attr(result, "cutoff_clinical") <- cutoff_clinical
  attr(result, "reference") <- c(
    "Jelínek et al. (2021) PLoS ONE, 16(4), e0249943",
    "Kovacs (1992) CDI Manual",
    "Kovacs & MHS Staff (2011) CDI 2 Technical Manual"
  )

  result
}

# ---------------------------------------------------------------------------
# 5. Summarise scores across a group
# ---------------------------------------------------------------------------

#' Summarise CDI scores across a group
#'
#' @param scored_data A data frame returned by score_cdi().
#' @param group_col   Optional column name for grouping variable.
#' @return A tibble with descriptive statistics for the total score.
summarise_cdi <- function(scored_data, group_col = NULL) {
  if (!"total_score" %in% colnames(scored_data)) {
    stop("scored_data must contain a total_score column (from score_cdi())")
  }

  if (!is.null(group_col)) {
    scored_data %>%
      group_by(!!sym(group_col)) %>%
      summarise(
        n = n(),
        mean_total = mean(total_score, na.rm = TRUE),
        sd_total   = sd(total_score, na.rm = TRUE),
        median_total = median(total_score, na.rm = TRUE),
        min_total  = min(total_score, na.rm = TRUE),
        max_total  = max(total_score, na.rm = TRUE),
        pct_elevated = mean(screening_result != "Within_normal_limits",
                            na.rm = TRUE) * 100,
        .groups = "drop"
      )
  } else {
    scored_data %>%
      summarise(
        n = n(),
        mean_total = mean(total_score, na.rm = TRUE),
        sd_total   = sd(total_score, na.rm = TRUE),
        median_total = median(total_score, na.rm = TRUE),
        min_total  = min(total_score, na.rm = TRUE),
        max_total  = max(total_score, na.rm = TRUE),
        pct_elevated = mean(screening_result != "Within_normal_limits",
                            na.rm = TRUE) * 100
      )
  }
}

# ---------------------------------------------------------------------------
# 6. Flag invalid or suspicious response patterns
# ---------------------------------------------------------------------------

#' Flag potentially invalid CDI response patterns
#'
#' @param data       A data frame with CDI item responses.
#' @param item_cols  Character vector of item column names.
#' @param id_col     Optional respondent ID column.
#' @return The input data frame with additional flag columns.
flag_cdi_patterns <- function(data, item_cols, id_col = NULL) {

  range_ok <- function(x) !is.na(x) & x >= 0 & x <= 2

  result <- data %>%
    mutate(
      n_missing  = rowSums(!across(all_of(item_cols), range_ok), na.rm = TRUE),
      n_zero     = rowSums(across(all_of(item_cols), ~ . == 0), na.rm = TRUE),
      n_two      = rowSums(across(all_of(item_cols), ~ . == 2), na.rm = TRUE),
      all_zero   = n_zero == length(item_cols),
      all_two    = n_two == length(item_cols),
      flag_invariant        = all_zero | all_two,
      flag_excessive_missing = n_missing > floor(length(item_cols) * 0.2)
    )

  if (!is.null(id_col)) {
    result <- result %>% relocate(all_of(id_col), everything())
  }

  result
}

# =============================================================================
# Example usage (commented out)
# =============================================================================
#
# # ---- Simulate some CDI data (27 items, 100 respondents) ----
# set.seed(42)
# sim_items <- replicate(27, sample(0:2, 100, replace = TRUE, prob = c(0.6, 0.3, 0.1)))
# colnames(sim_items) <- paste0("CDI_", sprintf("%02d", 1:27))
# sim_data <- as_tibble(sim_items)
# sim_data$subject_id <- sprintf("S%03d", 1:100)
#
# # ---- Score the original CDI ----
# scores <- score_cdi(sim_data,
#                     item_cols = paste0("CDI_", sprintf("%02d", 1:27)),
#                     id_col = "subject_id")
#
# scores %>% select(subject_id, total_score, screening_result, severity_category)
#
# # ---- Group summary ----
# scores %>% summarise_cdi()
#
# # ---- Flag invalid patterns ----
# flagged <- flag_cdi_patterns(sim_data,
#                              item_cols = paste0("CDI_", sprintf("%02d", 1:27)),
#                              id_col = "subject_id")
# flagged %>% filter(flag_invariant)
