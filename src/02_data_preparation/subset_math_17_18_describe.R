# math_v2_ua_22_24_17-18_describe.R
# ============================================================
# Subset YoungerS (traditional college age students, 
# TCAS, age = 17 or 18). Run final missingness diagnostics, 
# create descriptive summaries, item-level
# sample-size tables, and save the final cleaned dataset.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(openxlsx)
})

# Pathing aligned with math project structure
project_dir <- "C:/Users/orosc/OneDrive/Math"
source(file.path(project_dir, "utils_math_pipeline.R"))


output_dir <- file.path(project_dir, 
                        "math_v2_ua_22_24_17-18_describe")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Input from the math finalized step
input_file <- file.path(project_dir, 
                        "math_v2_umgc_ua_22_24_combined_qa_outputs", "math_v2_umgc_ua_22_24.rds")

stopifnot(file.exists(input_file))

# ------------------------------------------------------------
# Section 1. Load files and finalize cleaned dataset
# ------------------------------------------------------------

math_v2_umgc_ua_22_24 <- readRDS(input_file)

# Subset for 17-18 year olds
# Note: uses 'age' as numeric for filtering
math_v2_umgc_ua_22_24_17_18 <- 
  math_v2_umgc_ua_22_24[math_v2_umgc_ua_22_24$age == 17 |  
                          math_v2_umgc_ua_22_24$age == 18,]

# Subset for specific UA colleges
math_v2_ua_22_24_17_18 <- 
  math_v2_umgc_ua_22_24_17_18[math_v2_umgc_ua_22_24_17_18$college == "ua2" |  
                                math_v2_umgc_ua_22_24_17_18$college == "ua23",]

# Quick QA checks
stopifnot(sum(duplicated(math_v2_ua_22_24_17_18$global_id)) == 0)

n_input <- nrow(math_v2_ua_22_24_17_18)
cat("Input raw rows:   ", n_input, "\n")

miss_nodup <- run_missingness_diagnostics(
  df = math_v2_ua_22_24_17_18,
  stage_name = "younger_subset",
  save_plots = TRUE,
  output_dir = output_dir
)

# ------------------------------------------------------------
# Section 2. Descriptive statistics
# ------------------------------------------------------------

math_v2_ua_22_24_17_18 <- math_v2_ua_22_24_17_18 %>%
  mutate(
    college   = as.factor(college),
    wave      = as.factor(wave),
    age       = as.factor(age),
    gender    = as.factor(gender),
    ethnicity = as.factor(ethnicity),
    military  = as.factor(military),
    pell      = as.factor(pell),
    transfer  = suppressWarnings(as.numeric(transfer)),
    mathTime  = suppressWarnings(as.numeric(mathTime)) # math-specific
  )

cat_vars <- c("college", "wave", "age", "gender", "ethnicity", "military", "pell")

cat_descriptives <- lapply(cat_vars, function(v) {
  math_v2_ua_22_24_17_18 %>%
    count(.data[[v]], .drop = FALSE) %>%
    mutate(
      variable = v,
      percent = 100 * n / sum(n)
    ) %>%
    rename(category = 1) %>%
    select(variable, category, n, percent)
})

cat_descriptives_df <- bind_rows(cat_descriptives)

num_descriptives <- math_v2_ua_22_24_17_18 %>%
  summarise(
    transfer_n = sum(!is.na(transfer)),
    transfer_mean = mean(transfer, na.rm = TRUE),
    transfer_sd = sd(transfer, na.rm = TRUE),
    transfer_min = min(transfer, na.rm = TRUE),
    transfer_q1 = as.numeric(quantile(transfer, 0.25, na.rm = TRUE)),
    transfer_median = median(transfer, na.rm = TRUE),
    transfer_q3 = as.numeric(quantile(transfer, 0.75, na.rm = TRUE)),
    transfer_max = max(transfer, na.rm = TRUE),
    
    mathTime_n = sum(!is.na(mathTime)),
    mathTime_mean = mean(mathTime, na.rm = TRUE),
    mathTime_sd = sd(mathTime, na.rm = TRUE),
    mathTime_min = min(mathTime, na.rm = TRUE),
    mathTime_q1 = as.numeric(quantile(mathTime, 0.25, na.rm = TRUE)),
    mathTime_median = median(mathTime, na.rm = TRUE),
    mathTime_q3 = as.numeric(quantile(mathTime, 0.75, na.rm = TRUE)),
    mathTime_max = max(mathTime, na.rm = TRUE)
  )

p_transfer_hist <- ggplot(math_v2_ua_22_24_17_18, aes(x = transfer)) +
  geom_histogram(bins = 30) +
  labs(title = "Transferred Credits Distribution", x = "Transferred Credits", y = "Count") +
  theme_minimal()

p_mathTime_hist <- ggplot(math_v2_ua_22_24_17_18, aes(x = mathTime)) +
  geom_histogram(bins = 30) +
  labs(title = "Math Time Distribution", x = "Math Time", y = "Count") +
  theme_minimal()

p_transfer_box <- ggplot(math_v2_ua_22_24_17_18, aes(y = transfer)) +
  geom_boxplot() +
  labs(title = "Boxplot of Transferred Credits", y = "Transferred Credits") +
  theme_minimal()

p_mathTime_box <- ggplot(math_v2_ua_22_24_17_18, aes(y = mathTime)) +
  geom_boxplot() +
  labs(title = "Boxplot of Math Time", y = "Math Time") +
  theme_minimal()

# ------------------------------------------------------------
# Section 3. Save outputs
# ------------------------------------------------------------

# Generate math-specific item sample size report
sample_size_report <- make_item_sample_size_report(math_v2_ua_22_24_17_18)

save_both(math_v2_ua_22_24_17_18, output_dir, "math_v2_ua_22_24_17_18")

write.csv(
  cat_descriptives_df,
  file.path(output_dir, "categorical_descriptives_math_v2_ua_22_24_17_18.csv"),
  row.names = FALSE
)

write.csv(
  num_descriptives,
  file.path(output_dir, "numeric_descriptives_math_v2_ua_22_24_17_18.csv"),
  row.names = FALSE
)

write.csv(
  sample_size_report$overall_counts,
  file.path(output_dir, "item_sample_size_overall.csv"),
  row.names = FALSE
)

write.csv(
  sample_size_report$by_demo_counts,
  file.path(output_dir, "item_sample_size_by_demo.csv"),
  row.names = FALSE
)

saveRDS(sample_size_report, file.path(output_dir, "item_sample_size_report.rds"))

ggsave(file.path(output_dir, "transfer_histogram.png"), p_transfer_hist, width = 7, height = 5, dpi = 300)
ggsave(file.path(output_dir, "mathTime_histogram.png"), p_mathTime_hist, width = 7, height = 5, dpi = 300)
ggsave(file.path(output_dir, "transfer_boxplot.png"), p_transfer_box, width = 5, height = 5, dpi = 300)
ggsave(file.path(output_dir, "mathTime_boxplot.png"), p_mathTime_box, width = 5, height = 5, dpi = 300)


# ============================================================
# Create six summary tables for item selection in the planned
# math study by counting eligible items across math domains
# under response-count thresholds.
# ============================================================

item_file <- file.path(output_dir, "item_sample_size_by_demo.csv")
item_sample_size_by_demo <- read_csv(item_file, show_col_types = FALSE)

# ----------------------------
# helpers
# ----------------------------

# Adjusted for the 6 specific domains
domain_from_qid <- function(x) {
  # Strip prefix and difficulty suffixes
  code <- gsub("^Q\\d{3}", "", x)
  code <- gsub("(_p|E|M|H|E_p|M_p|H_p)$", "", code)
  code <- tolower(code)
  
  case_when(
    code == "l" ~ "Lines and Functions",
    code == "w" ~ "Word Problems",
    code == "v" ~ "Variables and Equations",
    code == "g" ~ "Geometry",
    code == "s" ~ "Statistics",
    code == "n" ~ "Numbers and Calculation",
    TRUE        ~ "Other"
  )
}

# Domains ordered alphabetically
domain_order <- c(
  "Geometry",
  "Lines and Functions",
  "Numbers and Calculation",
  "Statistics",
  "Variables and Equations",
  "Whole Numbers"
)

# ----------------------------
# build item lookup from QID
# ----------------------------

qid_lookup <- item_sample_size_by_demo %>%
  distinct(QID) %>%
  mutate(
    domain = domain_from_qid(QID),
    difficulty = extract_assigned_difficulty_from_qid(QID),
    difficulty = if_else(difficulty == "HARD_P", "HARD", difficulty)
  )

# ----------------------------
# threshold metrics
# ----------------------------

overall_n <- item_sample_size_by_demo %>%
  group_by(QID, demographic) %>%
  summarise(n_responded = sum(n_responded), .groups = "drop") %>%
  group_by(QID) %>%
  summarise(overall_n = max(n_responded), .groups = "drop")

min_demo_group_n <- item_sample_size_by_demo %>%
  filter(
    demographic %in% c("gender", "military", "pell"),
    !is.na(group_value),
    group_value != "NA",
    group_value != ""
  ) %>%
  group_by(QID) %>%
  summarise(min_demo_group_n = min(n_responded), .groups = "drop")

summary_df <- qid_lookup %>%
  left_join(overall_n, by = "QID") %>%
  left_join(min_demo_group_n, by = "QID")

# ----------------------------
# function to make one table
# ----------------------------

make_domain_difficulty_table <- function(df, metric_col, threshold) {
  out <- df %>%
    filter(.data[[metric_col]] >= threshold) %>%
    count(domain, difficulty, name = "n_items") %>%
    pivot_wider(names_from = difficulty, values_from = n_items, values_fill = 0)
  
  for(col in c("EASY", "MEDIUM", "HARD")) {
    if(!col %in% names(out)) out[[col]] <- 0
  }
  
  out <- tibble(domain = domain_order) %>%
    left_join(out, by = "domain") %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
    mutate(Total = rowSums(select(., where(is.numeric))))
  
  bind_rows(
    out,
    summarise(out, domain = "Total", across(where(is.numeric), sum))
  )
}

# ----------------------------
# six tables
# ----------------------------

tbl_irt_300 <- make_domain_difficulty_table(summary_df, "overall_n", 300)
tbl_irt_150 <- make_domain_difficulty_table(summary_df, "overall_n", 150)

tbl_dif_200 <- make_domain_difficulty_table(summary_df, "min_demo_group_n", 200)
tbl_dif_50  <- make_domain_difficulty_table(summary_df, "min_demo_group_n", 50)

tbl_factor_100 <- make_domain_difficulty_table(summary_df, "min_demo_group_n", 100)
tbl_factor_40  <- make_domain_difficulty_table(summary_df, "min_demo_group_n", 40)

# ----------------------------
# save to Excel
# ----------------------------

out_xlsx <- file.path(output_dir, "Summary_Tables_for_Item_Selection_math.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(wb, "README", data.frame(
  Note = c(
    "File used: item_sample_size_by_demo.csv",
    "Domain and difficulty derived from Math QID patterns (N, V, W, L, G, S).",
    "IRT tables: overall item responses threshold.",
    "DIF and Factor Level tables: minimum item responses across gender, military, and pell groups."
  )
))

sheet_write <- function(wb, sheet, df, subtitle) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, subtitle, startRow = 1, startCol = 1)
  writeData(wb, sheet, df, startRow = 3, startCol = 1)
  
  hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E78", halign = "center", textDecoration = "bold", border = "Bottom")
  addStyle(wb, sheet, hs, rows = 3, cols = 1:ncol(df), gridExpand = TRUE)
  setColWidths(wb, sheet, cols = 1:ncol(df), widths = "auto")
}

# Labels updated as requested
sheet_write(wb, "IRT_Frequentist_300", tbl_irt_300, 
            "IRT 2PL Fit: minimum frequentist threshold = 300 responses")

sheet_write(wb, "IRT_Bayesian_150", tbl_irt_150, 
            "IRT 2PL Fit: Bayesian stabilization = 150 responses")

sheet_write(wb, "DIF_Frequentist_200", tbl_dif_200, 
            "DIF: 200 per gender/military/pell group")

sheet_write(wb, "DIF_Bayesian_50", tbl_dif_50, 
            "DIF: Bayesian stabilization = 50 per gender/military/pell group")

sheet_write(wb, "FactorLevel_Freq_100", tbl_factor_100, 
            "Factor level: 100 per factor level on gender/military/pell")

sheet_write(wb, "FactorLevel_Bayes_40", tbl_factor_40, 
            "Factor level: Bayesian stabilization = 40 per factor level on gender/military/pell")

saveWorkbook(wb, out_xlsx, overwrite = TRUE)