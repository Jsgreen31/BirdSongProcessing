### Script to validate Hawkears using data from 2021, 2022, 2024 ###
## J. Green modified 23/03/25 ##

## data wrangling and prep ##

## Load Required Libraries ##
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(purrr)

##  Define File Paths for WildTrax and HawkEars Data (2021, 2022, 2024)
wildtrax_files <- list(
  "2021" = "/Users/Jorda/Desktop/Data/Cleaned Data/wildtrax_validation_2021.csv",
  "2022" = "/Users/Jorda/Desktop/Data/Cleaned Data/wildtrax_validation_2022.csv",
  "2024" = "/Users/Jorda/Desktop/Data/Cleaned Data/wildtrax_validation_2024.csv"
)

hawkears_files <- list(
  "2021" = "/Users/Jorda/Desktop/Data/Cleaned Data/hawkears_outputs_2021.csv",
  "2022" = "/Users/Jorda/Desktop/Data/Cleaned Data/hawkears_outputs_2022.csv",
  "2024" = "/Users/Jorda/Desktop/Data/Cleaned Data/hawkears_outputs_2024.csv"
)

##  Load New Brunswick Species List & Species Codes
nb_species_list <- read.csv("/Users/Jorda/Desktop/Data/Cleaned Data/species_list_nb.csv") %>%
  mutate(common_name = as.character(common_name))

species_reference <- read.csv("/Users/Jorda/Desktop/Data/Cleaned Data/bird banding codes.csv") %>%
  mutate(
    species_code = as.character(species_code),
    common_name = as.character(common_name)
  )

## analysis ##

## Load and Process WildTrax Data for All Years
wildtrax_data <- map_dfr(names(wildtrax_files), function(year) {
  read.csv(wildtrax_files[[year]]) %>%
    filter(!is.na(species_scientific_name) & species_scientific_name != "") %>%
    filter(species_class == "Aves") %>%
    mutate(
      location = as.character(location),  
      recording_date_time = as.POSIXct(recording_datetime, format = "%Y-%m-%d %H:%M"),
      filename = paste0(location, "_", format(recording_date_time, "%Y%m%d_%H%M%S")),
      year = year
    ) %>%
    select(filename, location, species_code, year) %>%
    distinct() %>%
    mutate(present_wildtrax = 1)
})


##  Load and Process HawkEars Data for All Years
hawkears_data <- map_dfr(names(hawkears_files), function(year) {
  read.csv(hawkears_files[[year]]) %>%
    mutate(
      location = as.character(location),  
      recording_date_time = as.POSIXct(recording_datetime, format = "%Y-%m-%d %H:%M"),  # Convert to datetime
      filename = paste0(location, "_", format(recording_date_time, "%Y%m%d_%H%M%S")),  # Standardize filename
      year = year  # Add year column
    ) %>%
    # **Exclude detections after 180s for 2024 (only 3-mins of 5 were processed by wildtrax)
    filter(!(year == "2024" & start_time > 180)) %>%  ## only use first 3 mins of 2024 data
    select(filename, location, species_code, score, start_time, year)  # Keep only relevant columns
})


##  Merge HawkEars Data with Species Codes and Filter to NB Species
hawkears_filtered <- hawkears_data %>%
  left_join(species_reference, by = "species_code") %>%  # Add common names
  filter(common_name %in% nb_species_list$common_name)  # Keep only NB species

##  Filter HawkEars to Match WildTrax Processed Recordings
processed_recordings <- unique(wildtrax_data$filename)

## Merge HawkEars Data with Species Codes & Apply All Filters
hawkears_filtered <- hawkears_data %>%
  left_join(species_reference, by = "species_code") %>%  # Add common names
  filter(common_name %in% nb_species_list$common_name) %>%  # Keep only NB species
  filter(filename %in% processed_recordings)  # Keep only recordings analyzed by WildTrax

##  Reduce to One Detection per Species per Recording (Keep Highest Score)
hawkears_summary <- hawkears_filtered %>%
  filter(!is.na(species_code)) %>%
  group_by(filename, species_code, year) %>%
  summarise(
    max_score = max(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(present_hawkears = 1)

## Merge WildTrax & HawkEars Presence/Absence Matrices
combined_data <- full_join(
  wildtrax_data, 
  hawkears_summary, 
  by = c("filename", "species_code", "year"),
  suffix = c("_wildtrax", "_hawkears")
)

## Compute Overall Validation Metrics
metrics <- combined_data %>%
  summarise(
    TP = sum(!is.na(present_wildtrax) & !is.na(present_hawkears)),
    FP = sum(is.na(present_wildtrax) & !is.na(present_hawkears)),
    FN = sum(!is.na(present_wildtrax) & is.na(present_hawkears)),
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    F1_score = 2 * (Precision * Recall) / (Precision + Recall)
  )

print(metrics)

##  Compute Species-Level Metrics
species_metrics <- combined_data %>%
  group_by(species_code) %>%
  summarise(
    TP = sum(!is.na(present_wildtrax) & !is.na(present_hawkears)),
    FP = sum(is.na(present_wildtrax) & !is.na(present_hawkears)),
    FN = sum(!is.na(present_wildtrax) & is.na(present_hawkears)),
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    F1_score = 2 * (Precision * Recall) / (Precision + Recall),
    .groups = "drop"
  )

## Explore Prediction Score Thresholds
thresholds <- seq(0.75, 1.00, by = 0.05)

species_threshold_analysis <- map_dfr(thresholds, function(thresh) {
  
  filtered_hawkears <- hawkears_summary %>%
    filter(max_score >= thresh)
  
  filtered_data <- full_join(
    wildtrax_data, 
    filtered_hawkears, 
    by = c("filename", "species_code", "year"),
    suffix = c("_wildtrax", "_hawkears")
  )
  
  filtered_data %>%
    group_by(species_code) %>%
    summarise(
      TP = sum(!is.na(present_wildtrax) & !is.na(present_hawkears)),
      FP = sum(is.na(present_wildtrax) & !is.na(present_hawkears)),
      FN = sum(!is.na(present_wildtrax) & is.na(present_hawkears)),
      Precision = TP / (TP + FP),
      Recall = TP / (TP + FN),
      F1_score = 2 * (Precision * Recall) / (Precision + Recall),
      Threshold = thresh,
      .groups = "drop"
    )
})

##  Find Best Threshold per Species
best_threshold_per_species <- species_threshold_analysis %>%
  group_by(species_code) %>%
  filter(!all(is.na(F1_score))) %>%  # Remove species where all F1-scores are NA
  slice_max(F1_score, n = 1, with_ties = FALSE) %>%  
  arrange(desc(F1_score))  

##  View & Save Results
View(best_threshold_per_species)

write.csv(best_threshold_per_species, "best_threshold_per_species.csv", row.names = FALSE)

# Now that we have calculated metrics, let's visualize the data


##### Figures #####

# Let's look at threshold optimization

# Remove species with fewer than 25 detections
filtered_best_threshold <- best_threshold_per_species %>%
  filter(TP + FP + FN >= 50)  


ggplot(filtered_best_threshold, aes(x = Threshold, y = F1_score)) +
  geom_point(color = "blue", size = 2) +
  geom_text(aes(label = species_code), vjust = -0.5, size = 3, check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Prediction Score Threshold Optimization",
       x = "Threshold", y = "F1-score")

# Let's look at how metrics change across thresholds

# Compute precision, recall, and F1-score across all thresholds
threshold_performance <- species_threshold_analysis %>%
  group_by(Threshold) %>%
  summarise(
    TP = sum(TP, na.rm = TRUE),
    FP = sum(FP, na.rm = TRUE),
    FN = sum(FN, na.rm = TRUE),
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    F1_score = 2 * (Precision * Recall) / (Precision + Recall),
    .groups = "drop"
  )

# Plot Precision, Recall, and F1-score across thresholds
ggplot(threshold_performance, aes(x = Threshold)) +
  geom_line(aes(y = Precision, color = "Precision"), size = 1) +
  geom_line(aes(y = Recall, color = "Recall"), size = 1) +
  geom_line(aes(y = F1_score, color = "F1-score"), size = 1) +
  scale_color_manual(values = c("Precision" = "blue", "Recall" = "green", "F1-score" = "red")) +
  labs(title = "Performance Metrics Across Score Thresholds",
       x = "Score Threshold",
       y = "Metric Value") +
  theme_minimal()

## Let's plot species metrics
ggplot(species_metrics %>% filter(TP + FN >= 10), aes(x = Recall, y = Precision)) +
  geom_point(aes(color = F1_score, size = TP + FN)) +  
  geom_text(aes(label = species_code), vjust = -0.5, size = 3, check_overlap = TRUE) +  
  scale_color_gradient(low = "red", high = "blue") +  
  labs(
    title = "Species-Level Precision vs. Recall",
    x = "Recall",
    y = "Precision",
    color = "F1-score",
    size = "Total Detections"
  ) +
  theme_minimal()



## We could plot precision recall curves on a species by species basis to observe trade-offs

# Aggregate performance metrics across thresholds
pr_curve_data <- species_threshold_analysis %>%
  group_by(Threshold) %>%
  summarise(
    Precision = sum(TP) / (sum(TP) + sum(FP)),
    Recall = sum(TP) / (sum(TP) + sum(FN)),
    F1_score = 2 * (Precision * Recall) / (Precision + Recall),
    .groups = "drop"
  )

#overall curve
ggplot(pr_curve_data, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(aes(color = Threshold), size = 3) +  
  scale_color_gradient(low = "red", high = "blue") +
  labs(
    title = "Precision-Recall Curve Across Score Thresholds",
    x = "Recall",
    y = "Precision",
    color = "Threshold"
  ) +
  theme_minimal()

# Let's look at a species-by-species analysis

selected_species <- c("HETH", "OVEN", "EWPW", "CONI", "EAWP", "CAWA")

filtered_species_data <- species_threshold_analysis %>%
  filter(species_code %in% selected_species)


ggplot(filtered_species_data, aes(x = Recall, y = Precision, color = as.factor(Threshold), group = Threshold)) +
  geom_line(size = 1.2) +  
  geom_point(size = 2) +  
  facet_wrap(~species_code, scales = "free") +  
  scale_color_viridis_d(name = "Threshold") +  # Use discrete color scale
  labs(
    title = "Precision-Recall Curves for Selected Species",
    x = "Recall",
    y = "Precision",
    color = "Threshold"
  ) +
  theme_minimal()


# Create a table of validation metrics for selected species
selected_species <- c("HETH", "OVEN", "EWPW", "CONI", "EAWP", "CAWA")

species_performance <- species_metrics %>%
  filter(species_code %in% selected_species) %>%
  arrange(desc(F1_score))  # Sort by F1-score

# View table
print(species_performance)

# Save as CSV
write.csv(species_performance, "species_performance_summary.csv", row.names = FALSE)


# Filter data for CONI
coni_threshold_data <- species_threshold_analysis %>%
  filter(species_code == "CONI")

# Plot F1-score, Precision, and Recall across thresholds
ggplot(coni_threshold_data, aes(x = Threshold)) +
  geom_line(aes(y = Precision, color = "Precision"), size = 1) +
  geom_line(aes(y = Recall, color = "Recall"), size = 1) +
  geom_line(aes(y = F1_score, color = "F1-score"), size = 1.2) +
  scale_color_manual(values = c("Precision" = "blue", "Recall" = "green", "F1-score" = "red")) +
  labs(
    title = "Performance Across Thresholds: CONI",
    x = "Prediction Score Threshold",
    y = "Metric Value",
    color = "Metric"
  ) +
  theme_minimal()


# Count the number of CONI detections at each threshold
coni_detection_counts <- species_threshold_analysis %>%
  filter(species_code == "CONI") %>%
  group_by(Threshold) %>%
  summarise(
    Total_Detections = sum(TP + FP, na.rm = TRUE),  # All detections by HawkEars
    TP_Detections = sum(TP, na.rm = TRUE),  # Correctly identified species
    FP_Detections = sum(FP, na.rm = TRUE),  # Incorrect detections (False Positives)
    FN_Detections = sum(FN, na.rm = TRUE),  # Missed detections (False Negatives)
    .groups = "drop"
  )

# View results
print(coni_detection_counts)

# Save to CSV if needed
write.csv(coni_detection_counts, "coni_detection_counts_by_threshold.csv", row.names = FALSE)

# View results
print(coni_detection_counts)

# Save to CSV if needed
write.csv(coni_detection_counts, "coni_detection_counts_by_threshold.csv", row.names = FALSE)

#### Reciever Operator Characteristic (ROC) Curves and Precision-Recall (PR) Curves Analysis and Figures

## ** Load Required Libraries**
library(dplyr)
library(ggplot2)
library(pROC)  # For ROC Curve
library(PRROC) # For PR Curve

## ** Prepare Data for ROC & PR Curves**
# Ensure HawkEars and WildTrax are merged with relevant data

# Filter combined_data to exclude species with fewer than 25 detections
filtered_data <- combined_data %>%
  group_by(species_code) %>%
  filter(n() >= 25)  # Change the threshold to 25 if desired

# Recalculate the ROC and PR curves using this filtered data
roc_pr_data <- filtered_data %>%
  mutate(
    # Create Binary_Label: 
    # If max_score is NA (HawkEars did not detect the species but WildTrax did), it's a False Negative (FN).
    # Otherwise, it's a True Positive (TP) if 'present_wildtrax' exists.
    Binary_Label = ifelse(!is.na(present_wildtrax), 1, 0),  # 1 = True Positive, 0 = False Positive
    max_score = ifelse(is.na(max_score), 0, max_score)  # Treat NA as a score of 0 (or any other value indicating non-detection)
  ) %>%
  filter(!is.na(max_score) | !is.na(present_wildtrax))

# ROC and PR calculations
roc_curve <- roc(roc_pr_data$Binary_Label, roc_pr_data$max_score)
pr_curve <- pr.curve(scores.class0 = roc_pr_data$max_score[roc_pr_data$Binary_Label == 1], 
                     scores.class1 = roc_pr_data$max_score[roc_pr_data$Binary_Label == 0], 
                     curve = TRUE)

# Calculate AUC
roc_auc <- auc(roc_curve)
pr_auc <- pr_curve$auc.integral

# Print AUC values
print(data.frame(Metric = c("ROC AUC", "PR AUC"), AUC_Value = c(roc_auc, pr_auc)))



## ** Plot ROC Curve**
ggplot(data.frame(TPR = roc_curve$sensitivities, FPR = 1 - roc_curve$specificities),
       aes(x = FPR, y = TPR)) +
  geom_line(color = "red", size = 1.2) +
  geom_abline(linetype = "dashed", color = "grey") +
  labs(title = "HawkEars Receiver Operating Characteristic (ROC) Curve",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()

## ** Plot ROC Curve with AUC Value**
ggplot(data.frame(TPR = roc_curve$sensitivities, FPR = 1 - roc_curve$specificities),
       aes(x = FPR, y = TPR)) +
  geom_line(color = "red", size = 1.2) +
  geom_abline(linetype = "dashed", color = "grey") +
  labs(title = "HawkEars Receiver Operating Characteristic (ROC) Curve",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  annotate("text", x = 0.6, y = 0.2, label = paste("AUC =", round(roc_auc, 2)), color = "black", size = 5, hjust = 0) +  # Add AUC value
  theme_minimal()


 ## ** Plot Precision-Recall Curve**
ggplot(data.frame(Recall = pr_curve$curve[,1], Precision = pr_curve$curve[,2]),
       aes(x = Recall, y = Precision)) +
  geom_line(color = "red", size = 1.2) +
  labs(title = "HawkEars Precision-Recall Curve",
       x = "Recall",
       y = "Precision") +
  theme_minimal()

## ** Plot Precision-Recall Curve with AUC Value**
ggplot(data.frame(Recall = pr_curve$curve[,1], Precision = pr_curve$curve[,2]),
       aes(x = Recall, y = Precision)) +
  geom_line(color = "red", size = 1.2) +
  labs(title = "HawkEars Precision-Recall Curve",
       x = "Recall",
       y = "Precision") +
  annotate("text", x = 0.6, y = 0.5, label = paste("AUC =", round(pr_auc, 2)), color = "black", size = 5, hjust = 0) +  # Add AUC value
  theme_minimal()



# Let's calculate AUC values for the ROC and PR curves
## ** Calculate AUC for ROC & PR Curves **
roc_auc <- auc(roc_curve)
pr_auc <- pr_curve$auc.integral

## ** Create a Summary Table for AUC Values **
auc_table <- data.frame(
  Metric = c("ROC AUC", "PR AUC"),
  AUC_Value = c(roc_auc, pr_auc)
)

## ** Print the AUC Table **
print(auc_table)



##  Compute & Plot Species-Specific ROC & PR Curves
selected_species <- c("HETH", "OVEN", "EWPW", "CONI", "EAWP", "CAWA")
species_roc_pr <- list()

for (species in selected_species) {
  species_data <- roc_pr_data %>% filter(species_code == species)
  
  if (nrow(species_data) >= 10) {  # Ensure enough detections
    roc_species <- roc(species_data$Binary_Label, species_data$max_score)
    pr_species <- pr.curve(scores.class0 = species_data$max_score[species_data$Binary_Label == 1],
                           scores.class1 = species_data$max_score[species_data$Binary_Label == 0],
                           curve = TRUE)
    
    species_roc_pr[[species]] <- list(roc = roc_species, pr = pr_species)
  }
}

# Create an empty list to store ROC curve data
roc_plot_data <- list()

# Extract ROC data for each species
for (species in names(species_roc_pr)) {
  roc_curve <- species_roc_pr[[species]]$roc
  roc_df <- data.frame(FPR = rev(roc_curve$specificities), 
                       TPR = rev(roc_curve$sensitivities), 
                       species = species)
  roc_plot_data[[species]] <- roc_df
}

# Combine into one dataframe
roc_plot_df <- do.call(rbind, roc_plot_data)

# Plot ROC Curves
ggplot(roc_plot_df, aes(x = FPR, y = TPR, color = species)) +
  geom_line(size = 1.2) +
  labs(title = "ROC Curves for Selected Species",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = "Species") +
  theme_minimal()


##  Save & Display Results
print(roc_curve$auc)  # Overall ROC AUC
print(pr_curve$auc.integral)  # Overall PR AUC

# To access individual species:
# species_roc_pr[["CONI"]]$roc  # CONI ROC Curve
# species_roc_pr[["CONI"]]$pr   # CONI PR Curve


