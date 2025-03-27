# -------------------------------------------------------------------------
# PVLens - SPL Label Data Review Analysis
#
# This script processes the SPL label review dataset
# to evaluate algorithm performance against adjudicated expert decisions.
#
# Author: Jeffery Painter <jeffery.l.painter@gsk.com>
# Date: 07-Mar-2025
# -------------------------------------------------------------------------

# ---- Load Required Libraries ----
library(dplyr)
library(tidyr)
library(irr)      # Kappa calculations
library(ggplot2)  # Visualization
library(purrr)    # Functional programming
library(readr)    # CSV handling
library(boot)     # Bootstrap confidence intervals
library(ggrepel)  # Avoid collisions in graphs
library(ggpubr)   # For arranging plots

# Set current working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ---- Step 1: Define File Paths & Import Data ----
phase1_data_directory <- "../data/review_results/"
graph_directory <- "../figures/"

# Function to load datasets safely
read_data <- function(filename) {
  filepath <- paste0(phase1_data_directory, filename)
  if (file.exists(filepath)) {
    return(read_csv(filepath, show_col_types = FALSE))
  } else {
    warning("⚠️ File not found: ", filename)
    return(NULL)
  }
}

# Load datasets
ind_reviews <- read_data("ind_reviews.csv")
ae_reviews <- read_data("ae_reviews.csv")
blackbox_reviews <- read_data("blackbox_reviews.csv")
missing_terms <- read_data("spl_missing_terms.csv")

# -------------------------------------------------------------------------

# ---- Step 3: Reshape Data into Wide Format ----
reshape_reviews <- function(data, category) {
  if (!is.null(data)) {
    data %>%
      mutate(Reviewer = case_when(
        REVIEW_COUNT == 1 ~ "Reviewer1",
        REVIEW_COUNT == 2 ~ "Reviewer2",
        REVIEW_COUNT == 3 ~ "Adjudicator"
      )) %>%
      select(SPL_ID, MDR_CODE, Reviewer, USER_ID, INCLUDE) %>%
      pivot_wider(names_from = Reviewer, values_from = c(USER_ID, INCLUDE), names_glue = "{Reviewer}_{.value}") %>%
      mutate(Category = category)
  } else {
    return(NULL)
  }
}

# Apply reshaping function
indications_wide <- reshape_reviews(ind_reviews, "Indication")
ae_wide <- reshape_reviews(ae_reviews, "AdverseEvent")
blackbox_wide <- reshape_reviews(blackbox_reviews, "BlackBox")



# ---- Step 4: Assemble Fully Adjudicated Reviews ----
fully_adjudicated_reviews <- bind_rows(
  ind_reviews %>% filter(REVIEW_COUNT == 3) %>% mutate(Category = "Indication"),
  ae_reviews %>% filter(REVIEW_COUNT == 3) %>% mutate(Category = "AdverseEvent"),
  blackbox_reviews %>% filter(REVIEW_COUNT == 3) %>% mutate(Category = "BlackBox")
)


# -------------------------------------------------------------------------
# Missing term analysis
# -------------------------------------------------------------------------

# Step 1A: Identify all user-added terms that were found in MedDRA
excluded_user_added_terms <- missing_terms %>%
  filter(
    !is.na(MDR_CODE),        # Must have a mapped MedDRA code
    CATEGORY %in% c("AdverseEvent", "BlackBox", "Indication")
  ) %>%
  distinct(SPL_ID, MDR_CODE, CATEGORY, .keep_all = TRUE)  # Keep unique SPL_ID & MDR_CODE

# Step 1B: Identify the subset that should be included in the false_negative report
user_added_terms <- excluded_user_added_terms %>%
  filter(
    !is.na(MDR_CODE),        
    INCLUDE_STY == TRUE,    
    SYNONYM_FOUND == FALSE,  
    STOP_WORD == FALSE,      
    CATEGORY %in% c("AdverseEvent", "BlackBox", "Indication")
  ) %>%
  distinct(SPL_ID, MDR_CODE, CATEGORY, .keep_all = TRUE)  

# Step 1C: Final filter that the MedDRA codes did not appear in the fully adjudicated review
false_negatives <- user_added_terms %>%
  anti_join(
    fully_adjudicated_reviews %>%
      filter(INCLUDE == 1) %>%  
      select(SPL_ID, MDR_CODE, Category),
    by = c("SPL_ID", "MDR_CODE", "CATEGORY" = "Category")
  )


# -------------------------------------------------------------------------
# Step 2: Summarize the Reasons for Non-Inclusion
# -------------------------------------------------------------------------

false_negative_summary <- excluded_user_added_terms %>%
  group_by(CATEGORY) %>%
  summarise(
    Not_Valid_Semantic_Type = sum(INCLUDE_STY == FALSE, na.rm = TRUE),
    Captured_By_Synonym = sum(SYNONYM_FOUND == TRUE, na.rm = TRUE),
    Excluded_Stop_Word = sum(STOP_WORD == TRUE, na.rm = TRUE),
    Total_False_Negatives = n(),
    .groups = "drop"
  )

# Step 2B: Merge Total User-Added Terms
final_summary <- tibble(CATEGORY = c("AdverseEvent", "BlackBox", "Indication")) %>%
  left_join(user_added_terms %>%
              count(CATEGORY, name = "User_Added_Term"), by = "CATEGORY") %>%
  left_join(false_negative_summary, by = "CATEGORY") %>%
  replace_na(list(User_Added_Term = 0, Not_Valid_Semantic_Type = 0, Captured_By_Synonym = 0, Excluded_Stop_Word = 0, Total_False_Negatives = 0))

# Step 2C: Convert to Long Format for Visualization
false_negative_long <- final_summary %>%
  pivot_longer(cols = c(User_Added_Term, Not_Valid_Semantic_Type, Captured_By_Synonym, Excluded_Stop_Word), 
               names_to = "Exclusion_Reason", 
               values_to = "Count") %>%
  filter(Count > 0 | Exclusion_Reason == "User_Added_Term") %>%  # ✅ Ensure all categories appear
  mutate(
    Label = paste0(Count, " (", round(Count / sum(Count) * 100, 1), "%)"),
    Exclusion_Reason = factor(Exclusion_Reason, levels = c("User_Added_Term", "Not_Valid_Semantic_Type", "Captured_By_Synonym", "Excluded_Stop_Word"))
  )

# -------------------------------------------------------------------------
# Step 3: Create Pie Chart Visualization with Category Breakdown
# -------------------------------------------------------------------------


print(false_negative_long)

ggplot(false_negative_long, aes(x = "", y = Count, fill = Exclusion_Reason)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  coord_polar("y", start = 0) +  
  theme_minimal() +
  scale_fill_manual(values = c("User_Added_Term" = "#999999",  
                               "Not_Valid_Semantic_Type" = "#1f78b4", 
                               "Captured_By_Synonym" = "#33a02c", 
                               "Excluded_Stop_Word" = "#e31a1c")) +
  geom_text_repel(aes(label = Label), 
                  position = position_stack(vjust = 0.5), 
                  segment.color = "black", 
                  size = 5, 
                  show.legend = FALSE) +  
  facet_wrap(~CATEGORY) +  
  labs(
    title = "User-Added Terms and Reasons for NLP Exclusion",
    subtitle = "Breakdown of False Negatives by Category",
    x = NULL, y = NULL
  ) +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        legend.title = element_blank())

output_file = paste0(graph_directory, "fig_01_useradded_terms.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)

message("\n✅ Data Processing Complete. Ready for Analysis.\n")

# -------------------------------------------------------------------------
# Step 6: Compute Overall Algorithm Performance
# -------------------------------------------------------------------------

# Compute Performance Metrics
num_false_negatives <- nrow(false_negatives)
num_true_positives <- sum(fully_adjudicated_reviews$INCLUDE == 1, na.rm = TRUE)
num_false_positives <- sum(fully_adjudicated_reviews$INCLUDE == 0, na.rm = TRUE)

precision <- num_true_positives / (num_true_positives + num_false_positives)
recall <- num_true_positives / (num_true_positives + num_false_negatives)
f1_score <- 2 * ((precision * recall) / (precision + recall))

cat("🔹 Overall Algorithm Performance:\n")
message("📌 Precision: ", round(precision * 100, 2), "%")
message("📌 Recall: ", round(recall * 100, 2), "%")
message("📌 F1-Score: ", round(f1_score * 100, 2), "%")

# ---- Step 7: Performance by Label Section ----

# Ensure FN is counted within each category correctly
false_negatives_per_category <- false_negatives %>%
  group_by(CATEGORY) %>%
  summarise(FN = n(), .groups = "drop") %>%
  rename(Category = CATEGORY)  # Ensure matching column name for join

# Compute TP, FP, and FN per category
category_metrics <- fully_adjudicated_reviews %>%
  group_by(Category) %>%
  summarise(
    TP = sum(INCLUDE == 1, na.rm = TRUE),
    FP = sum(INCLUDE == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(false_negatives_per_category, by = "Category") %>%  # Merge FN values
  mutate(
    FN = replace_na(FN, 0),  # Replace NA with 0 if no false negatives found
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    F1_Score = 2 * ((Precision * Recall) / (Precision + Recall))
  )

# -------------------------------------------------------------------------
# These are the results reported in Table 1: Validation study results
# -------------------------------------------------------------------------
# Print updated category metrics
print(category_metrics)


# -------------------------------------------------------------------------
# Step 9: Visualizations
# -------------------------------------------------------------------------

# ---- Full algorithm compared to full adjudication ----
# Create a dataframe for plotting
performance_metrics <- tibble(
  Metric = c("Precision", "Recall", "F1-Score"),
  Value = c(precision, recall, f1_score)
)

# Plot the performance metrics as a bar chart
ggplot(performance_metrics, aes(x = Metric, y = Value * 100, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(values = c("Precision" = "#1f78b4", "Recall" = "#33a02c", "F1-Score" = "#e31a1c")) +
  geom_text(aes(label = paste0(round(Value * 100, 2), "%")), vjust = -0.5, size = 5) +
  labs(
    title = "Algorithm Performance Metrics Compared to Fully Adjudicated Labels",
    x = "Metric",
    y = "Percentage (%)"
  ) +
  ylim(0, 110) # Ensure consistent scaling


output_file = paste0(graph_directory, "fig_02_alg_performance.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)


# ---- Full algorithm compared to full adjudication by category ----
category_metrics_long <- category_metrics %>%
  pivot_longer(cols = c(Precision, Recall, F1_Score), names_to = "Metric", values_to = "Value")

ggplot(category_metrics_long, aes(x = Category, y = Value * 100, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(values = c("Precision" = "#1f78b4", "Recall" = "#33a02c", "F1_Score" = "#e31a1c")) +
  geom_text(aes(label = paste0(round(Value * 100, 2), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(
    title = "Algorithm Performance by Label Category",
    x = "Label Category",
    y = "Percentage (%)"
  ) +
  ylim(0, 110)

output_file = paste0(graph_directory, "fig_03_alg_performance_by_category.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)



# ---- Step 9: Create Confusion Matrix Table ----
conf_matrix_summary <- data.frame(
  Category = c("True Positives (TP)", "False Positives (FP)", "False Negatives (FN)"),
  Count = c(num_true_positives, num_false_positives, num_false_negatives)
)

# ---- Step 10: Visualize Confusion Matrix ----
ggplot(conf_matrix_summary, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Algorithm vs. Adjudicator Confusion Matrix",
    x = "Prediction Outcome",
    y = "Count"
  ) +
  scale_fill_manual(values = c(
    "True Positives (TP)" = "green",
    "False Positives (FP)" = "red",
    "False Negatives (FN)" = "orange"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


output_file = paste0(graph_directory, "fig_04_alg_confusion_matrix.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)


# -------------------------------------------------------------------------
# Step 10: Compute Algorithm Performance excluding indications
# -------------------------------------------------------------------------

# ---- Step 1: Filter Out Indications ----
ae_bb_reviews <- fully_adjudicated_reviews %>%
  filter(Category %in% c("AdverseEvent", "BlackBox"))  # Keep only AE & BlackBox

# ---- Step 2: Compute False Negatives for AE & BlackBox ----
false_negatives_ae_bb <- false_negatives %>%
  filter(CATEGORY %in% c("AdverseEvent", "BlackBox")) 

# Count total false negatives for AE & BlackBox
num_false_negatives_ae_bb <- nrow(false_negatives_ae_bb)

# ---- Step 3: Compute Updated Performance Metrics ----
num_true_positives_ae_bb <- sum(ae_bb_reviews$INCLUDE == 1, na.rm = TRUE)
num_false_positives_ae_bb <- sum(ae_bb_reviews$INCLUDE == 0, na.rm = TRUE)

# Compute precision, recall, and F1-score for AE & BlackBox only
precision_ae_bb <- num_true_positives_ae_bb / (num_true_positives_ae_bb + num_false_positives_ae_bb)
recall_ae_bb <- num_true_positives_ae_bb / (num_true_positives_ae_bb + num_false_negatives_ae_bb)
f1_score_ae_bb <- 2 * ((precision_ae_bb * recall_ae_bb) / (precision_ae_bb + recall_ae_bb))

# ---- Step 4: Print Updated Performance Metrics ----
cat("🔹 Algorithm Performance (Adeverse Event & BlackBox Only):\n")
message("📌 Precision: ", ifelse(is.na(precision_ae_bb), "Undefined", paste0(round(precision_ae_bb * 100, 2), "%")))
message("📌 Recall: ", ifelse(is.na(recall_ae_bb), "Undefined", paste0(round(recall_ae_bb * 100, 2), "%")))
message("📌 F1-Score: ", ifelse(is.na(f1_score_ae_bb), "Undefined", paste0(round(f1_score_ae_bb * 100, 2), "%")))

# ---- Step 5: Visualize Algorithm Performance (AE & BlackBox) ----
performance_metrics_ae_bb <- tibble(
  Metric = c("Precision", "Recall", "F1-Score"),
  Value = c(precision_ae_bb, recall_ae_bb, f1_score_ae_bb)
)

ggplot(performance_metrics_ae_bb, aes(x = Metric, y = Value * 100, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(values = c("Precision" = "#1f78b4", "Recall" = "#33a02c", "F1-Score" = "#e31a1c")) +
  geom_text(aes(label = paste0(round(Value * 100, 2), "%")), vjust = -0.5, size = 5) +
  labs(
    title = "Algorithm Performance (AE & BlackBox Only)",
    x = "Metric",
    y = "Percentage (%)"
  ) +
  ylim(0, 110)  # Keep scaling consistent

output_file = paste0(graph_directory, "fig_05_alg_ae_bb_only.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)



# Check false negatives by category
false_negatives %>%
  count(CATEGORY)

# -------------------------------------------------------------------------
# How did the adjudicator agree with the students?
# -------------------------------------------------------------------------

# ---- Step 4: Assemble Fully Adjudicated Reviews ----
fully_adjudicated_wide <- bind_rows(
  indications_wide,
  ae_wide,
  blackbox_wide
)

# ---- Step 3: Compute Agreement at the SPL Level ----
spl_level_agreement <- fully_adjudicated_wide %>%
  filter(!is.na(Adjudicator_INCLUDE)) %>%
  mutate(
    R1_Agreement = if_else(!is.na(Reviewer1_INCLUDE), Reviewer1_INCLUDE == Adjudicator_INCLUDE, NA_real_),
    R2_Agreement = if_else(!is.na(Reviewer2_INCLUDE), Reviewer2_INCLUDE == Adjudicator_INCLUDE, NA_real_)
  ) %>%
  pivot_longer(cols = c(Reviewer1_USER_ID, Reviewer2_USER_ID), 
               names_to = "Reviewer_Type", values_to = "Reviewer_ID") %>%
  pivot_longer(cols = c(R1_Agreement, R2_Agreement), 
               names_to = "Agreement_Type", values_to = "Agreement") %>%
  filter(!is.na(Reviewer_ID), !is.na(Agreement)) %>%
  group_by(SPL_ID, Reviewer_ID) %>%
  summarise(
    SPL_Cases = n(),  # Count unique SPL ID & reviewer pairs
    Agreement_Percentage = mean(Agreement, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# ---- Step 4: Compute Overall Reviewer-Level Agreement ----
reviewer_agreement <- spl_level_agreement %>%
  group_by(Reviewer_ID) %>%
  summarise(
    Total_Cases = n(),  # Now counting unique SPL ID cases
    Agreement_Percentage = mean(Agreement_Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Agreement_Percentage))



# ---- Step 5: Visualize Agreement by User ----
ggplot(reviewer_agreement, aes(x = reorder(as.factor(Reviewer_ID), Agreement_Percentage), y = Agreement_Percentage)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Overall Agreement with Adjudicator by Reviewer",
       x = "Reviewer ID",
       y = "Agreement Percentage")

# ---- Print Results ----
print(reviewer_agreement)

# ---- Compute Overall Median Agreement ----
overall_median_agreement <- median(reviewer_agreement$Agreement_Percentage, na.rm = TRUE)
cat("Overall Median Reviewer Agreement with Adjudicator:", overall_median_agreement, "%\n")

# ---- Step 1: Prepare AE Review Data ----
ae_wide <- ae_reviews %>%
  mutate(Reviewer = case_when(
    REVIEW_COUNT == 1 ~ "Reviewer1",
    REVIEW_COUNT == 2 ~ "Reviewer2",
    REVIEW_COUNT == 3 ~ "Adjudicator"
  )) %>%
  select(SPL_ID, MDR_CODE, Reviewer, USER_ID, INCLUDE) %>%
  pivot_wider(names_from = Reviewer, values_from = c(USER_ID, INCLUDE), names_glue = "{Reviewer}_{.value}")


# ---- Step 3: Compute Agreement at the SPL Level ----
spl_level_agreement <- ae_wide %>%
  filter(!is.na(Adjudicator_INCLUDE)) %>%
  mutate(
    R1_Agreement = if_else(!is.na(Reviewer1_INCLUDE), Reviewer1_INCLUDE == Adjudicator_INCLUDE, NA_real_),
    R2_Agreement = if_else(!is.na(Reviewer2_INCLUDE), Reviewer2_INCLUDE == Adjudicator_INCLUDE, NA_real_)
  ) %>%
  pivot_longer(cols = c(Reviewer1_USER_ID, Reviewer2_USER_ID), 
               names_to = "Reviewer_Type", values_to = "Reviewer_ID") %>%
  pivot_longer(cols = c(R1_Agreement, R2_Agreement), 
               names_to = "Agreement_Type", values_to = "Agreement") %>%
  filter(!is.na(Reviewer_ID), !is.na(Agreement)) %>%
  group_by(SPL_ID, Reviewer_ID) %>%
  summarise(
    SPL_Cases = n(),  # Count unique SPL ID & reviewer pairs
    Agreement_Percentage = mean(Agreement, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# ---- Step 4: Compute Overall Reviewer-Level Agreement ----
reviewer_agreement <- spl_level_agreement %>%
  group_by(Reviewer_ID) %>%
  summarise(
    Total_Cases = n(),  # Now counting unique SPL ID cases
    Agreement_Percentage = mean(Agreement_Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Agreement_Percentage))

# ---- Step 5: Visualize Agreement by User ----
ggplot(reviewer_agreement, aes(x = reorder(as.factor(Reviewer_ID), Agreement_Percentage), y = Agreement_Percentage)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  coord_flip() +
  labs(title = "AE Agreement with Adjudicator by Reviewer",
       x = "Reviewer ID",
       y = "Agreement Percentage")



# ---- Print Results ----
print(reviewer_agreement)

# ---- Compute Overall Median Agreement ----
overall_median_agreement <- median(reviewer_agreement$Agreement_Percentage, na.rm = TRUE)
cat("AE: Overall Median Reviewer Agreement with Adjudicator:", overall_median_agreement, "%\n")

############################################################################
# Modality of agreement
############################################################################
# ---- Step 1: Categorize SPL Entries by Agreement ----
spl_agreement_modality <- spl_level_agreement %>%
  group_by(SPL_ID) %>%
  summarise(
    Median_Agreement = median(Agreement_Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Agreement_Category = factor(case_when(
      Median_Agreement == 100 ~ "Full Agreement (100%)",
      Median_Agreement >= 90 ~ "High Agreement (90-99%)",
      Median_Agreement >= 80 ~ "Moderate Agreement (80-89%)",
      Median_Agreement >= 60 ~ "Low Agreement (60-79%)",
      Median_Agreement > 0 ~ "Minimal Agreement (1-59%)",
      TRUE ~ "No Agreement (0%)"
    ), levels = c(
      "Full Agreement (100%)",
      "High Agreement (90-99%)",
      "Moderate Agreement (80-89%)",
      "Low Agreement (60-79%)",
      "Minimal Agreement (1-59%)",
      "No Agreement (0%)"
    ))
  )

# ---- Step 2: Count SPL Entries in Each Agreement Category ----
agreement_summary <- spl_agreement_modality %>%
  count(Agreement_Category) %>%
  arrange(desc(n))

agreement_summary

# ---- Step 3: Plot Adjudicator Agreement with Students ----
ggplot(agreement_summary, aes(x = Agreement_Category, y = n, fill = Agreement_Category)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Adjudicator Agreement with Student Reviewers",
    x = "Agreement Category",
    y = "Number of SPL Entries"
  ) +
  scale_fill_manual(values = c(
    "Full Agreement (100%)" = "green",
    "High Agreement (90-99%)" = "blue",
    "Moderate Agreement (80-89%)" = "lightblue",
    "Low Agreement (60-79%)" = "orange",
    "Minimal Agreement (1-59%)" = "red",
    "No Agreement (0%)" = "darkred"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

output_file = paste0(graph_directory, "fig_06_adjudicator_agreement.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)

############################################################################
# Compute overall agreement between the students on all sections
############################################################################

# ---- Step 1: Combine Indications, AE, and Black Box Reviews ----
all_reviews <- bind_rows(
  ind_reviews %>% filter(REVIEW_COUNT <= 2) %>% mutate(Category = "Indication"),
  ae_reviews %>% filter(REVIEW_COUNT <= 2) %>% mutate(Category = "AE"),
  blackbox_reviews %>% filter(REVIEW_COUNT <= 2) %>% mutate(Category = "BlackBox")
)

# Verify each question was reviewed by 2 students independently
num_reviewers_per_code <- all_reviews %>%
  group_by(SPL_ID, MDR_CODE) %>%
  summarise(Num_Reviewers = n_distinct(USER_ID), .groups = "drop")

print(table(num_reviewers_per_code$Num_Reviewers))


student_agreement <- all_reviews %>%
  group_by(SPL_ID, MDR_CODE, Category) %>%
  summarise(
    Num_Reviewers = n(),
    Num_Include = sum(INCLUDE, na.rm = TRUE),  # Count how many voted 'Include'
    Num_Exclude = Num_Reviewers - Num_Include, # Count 'Exclude' votes
    Agreement = as.numeric(Num_Include == 0 | Num_Exclude == 0),  # 1 if full agreement, 0 if disagreement
    Cases_Agreed = sum(Agreement),  # Total cases where reviewers agreed
    .groups = "drop"
  )

# Compute per-category agreement statistics
category_agreement <- student_agreement %>%
  group_by(Category) %>%
  summarise(
    Total_Cases = n(),  # ✅ This should now correctly count all reviewed terms
    Cases_Agreed = sum(Agreement),  # ✅ This is the correct count of agreements
    Mean_Agreement = round(mean(Agreement, na.rm = TRUE) * 100, 2),
    Median_Agreement = round(median(Agreement, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

print(category_agreement)

############################################################################
# Compute overall agreement between the students on all sections
############################################################################

# ---- Step 1: Combine Indications, AE, and Black Box Reviews ----
all_reviews <- bind_rows(
  ind_reviews %>% filter(REVIEW_COUNT <= 2) %>% mutate(Category = "Indication"),
  ae_reviews %>% filter(REVIEW_COUNT <= 2) %>% mutate(Category = "AE"),
  blackbox_reviews %>% filter(REVIEW_COUNT <= 2) %>% mutate(Category = "BlackBox")
)

# Verify each question was reviewed by 2 students independently
num_reviewers_per_code <- all_reviews %>%
  group_by(SPL_ID, MDR_CODE) %>%
  summarise(Num_Reviewers = n_distinct(USER_ID), .groups = "drop")

print(table(num_reviewers_per_code$Num_Reviewers))

student_agreement <- all_reviews %>%
  group_by(SPL_ID, MDR_CODE, Category) %>%
  summarise(
    Num_Reviewers = n(),
    Num_Include = sum(INCLUDE, na.rm = TRUE),  # Count how many voted 'Include'
    Num_Exclude = Num_Reviewers - Num_Include, # Count 'Exclude' votes
    Agreement = as.numeric(Num_Include == 0 | Num_Exclude == 0),  # 1 if full agreement, 0 if disagreement
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# Reported on Page 3 of the report - 77% agreement among student reviewers
# -------------------------------------------------------------------------
overall_student_agreement <- mean(student_agreement$Agreement, na.rm = TRUE) * 100
cat("Overall Student Agreement Across All Reviews:", round(overall_student_agreement, 2), "%\n")
# -------------------------------------------------------------------------

category_agreement <- student_agreement %>%
  group_by(Category) %>%
  summarise(
    Total_Cases = n(),
    Mean_Agreement = round(mean(Agreement, na.rm = TRUE) * 100, 2),
    Median_Agreement = round(median(Agreement, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

print(category_agreement)


# Plot Student Agreement by Category
ggplot(category_agreement, aes(x = Category, y = Mean_Agreement, fill = Category)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +  # Main bars for mean agreement
  geom_errorbar(aes(ymin = Median_Agreement, ymax = Median_Agreement),  # Add median as error bar
                width = 0.3, color = "black", size = 1.2) +
  theme_minimal() +
  labs(title = "Student Agreement by Category",
       x = "Review Category",
       y = "Agreement Percentage (%)") +
  scale_fill_manual(values = c("AE" = "blue", "BlackBox" = "red", "Indication" = "green")) +
  theme(legend.position = "none")

# Define refined colors (muted but distinct)
category_colors <- c("AE" = "#377EB8", "BlackBox" = "#E41A1C", "Indication" = "#4DAF4A")

# Generate the enhanced bar plot
ggplot(category_agreement, aes(x = reorder(Category, -Mean_Agreement), 
                               y = Mean_Agreement, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", alpha = 0.85) +  # Black border for contrast
  scale_fill_manual(values = category_colors) +  # Apply professional colors
  theme_minimal(base_size = 14) +  # Use a minimal theme with larger fonts
  labs(
    title = "Student Agreement by Review Category",
    x = "Review Category",
    y = "Agreement Percentage (%)",
    fill = "Category"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center the title, bold it
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),  # Remove major gridlines for a cleaner look
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )

output_file = paste0(graph_directory, "fig_07_student_agreement_by_category.png")
ggsave(output_file, width = 8, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# End of report
# -------------------------------------------------------------------------
