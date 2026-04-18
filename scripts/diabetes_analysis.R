# Diabetes risk factor analysis for the presentation

required_packages <- c("tidyverse", "psych")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(psych)

# Create output folders and remove old files

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

old_output_files <- list.files("outputs", full.names = TRUE)
if (length(old_output_files) > 0) unlink(old_output_files)

old_plot_files <- list.files("plots", full.names = TRUE)
if (length(old_plot_files) > 0) unlink(old_plot_files)

set.seed(123)

# Helper function for printed output

say <- function(...) cat(..., "\n")

# Plot appearance settings

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold")
    )
)

colour_navy  <- "#355C7D"
colour_blue  <- "#4C78A8"
colour_teal  <- "#3B8EA5"
colour_green <- "#5AA469"
colour_olive <- "#7A9E7E"
colour_amber <- "#D9A441"
colour_gold  <- "#E0B04B"
colour_coral <- "#D96C5F"
colour_plum  <- "#8E6C8A"
colour_pale  <- "#9CCFD8"
colour_slate <- "#577590"
colour_grey  <- "#6C757D"

palette_outcome <- c(
  "No Diabetes" = colour_navy,
  "Diabetes" = colour_coral
)

palette_age <- c(
  "<30" = colour_blue,
  "30-39" = colour_teal,
  "40-49" = colour_olive,
  "50+" = colour_amber
)

palette_bmi <- c(
  "Underweight" = colour_pale,
  "Normal" = colour_green,
  "Overweight" = colour_amber,
  "Obese" = colour_coral
)

palette_pregnancy <- c(
  "0" = colour_pale,
  "1-2" = colour_teal,
  "3-4" = colour_slate,
  "5+" = colour_amber
)

palette_age_lines <- c(
  "Age 25" = colour_blue,
  "Age 35" = colour_teal,
  "Age 45" = colour_olive,
  "Age 55" = colour_coral
)

# Helper function to round only numeric columns in a data frame

round_numeric_df <- function(df, digits = 2) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  df
}

# Import the dataset

diabetes <- read_csv("data/diabetes.csv", show_col_types = FALSE)

say("Amount of participants in the raw dataset:", nrow(diabetes))
say("Amount of variables in the raw dataset:", ncol(diabetes))
say("These are the variables in the dataset:")
print(names(diabetes))

# Check missing values and zero values in the raw data

missing_values <- colSums(is.na(diabetes))

zero_values <- sapply(diabetes, function(x) {
  if (is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA
})

variable_audit <- data.frame(
  variable = names(diabetes),
  class = sapply(diabetes, function(x) class(x)[1]),
  missing_values = as.numeric(missing_values),
  zero_values = as.numeric(zero_values)
)

write.csv(variable_audit, "outputs/variable_audit.csv", row.names = FALSE)

numeric_summary_raw <- psych::describe(diabetes)
write.csv(as.data.frame(numeric_summary_raw), "outputs/numeric_summary_raw.csv", row.names = TRUE)

clinical_zero_check <- data.frame(
  variable = c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI"),
  zero_count = c(
    sum(diabetes$Glucose == 0, na.rm = TRUE),
    sum(diabetes$BloodPressure == 0, na.rm = TRUE),
    sum(diabetes$SkinThickness == 0, na.rm = TRUE),
    sum(diabetes$Insulin == 0, na.rm = TRUE),
    sum(diabetes$BMI == 0, na.rm = TRUE)
  )
)

write.csv(clinical_zero_check, "outputs/clinical_zero_check.csv", row.names = FALSE)

say("This is the number of missing values in each variable:")
print(missing_values)

say("This is the number of zero values in each numeric variable:")
print(zero_values)

say("This is the count of clinically implausible zero values:")
print(clinical_zero_check)

# Recode clinically implausible zero values as missing

diabetes_clean <- diabetes %>%
  mutate(
    Glucose = na_if(Glucose, 0),
    BloodPressure = na_if(BloodPressure, 0),
    SkinThickness = na_if(SkinThickness, 0),
    Insulin = na_if(Insulin, 0),
    BMI = na_if(BMI, 0)
  )

missing_values_clean <- colSums(is.na(diabetes_clean))

write.csv(
  data.frame(
    variable = names(diabetes_clean),
    missing_values_after_cleaning = as.numeric(missing_values_clean)
  ),
  "outputs/missing_values_after_cleaning.csv",
  row.names = FALSE
)

numeric_summary_clean <- psych::describe(diabetes_clean)
write.csv(as.data.frame(numeric_summary_clean), "outputs/numeric_summary_clean.csv", row.names = TRUE)

cleaning_decisions <- data.frame(
  variable = c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI"),
  raw_zero_count = c(
    sum(diabetes$Glucose == 0, na.rm = TRUE),
    sum(diabetes$BloodPressure == 0, na.rm = TRUE),
    sum(diabetes$SkinThickness == 0, na.rm = TRUE),
    sum(diabetes$Insulin == 0, na.rm = TRUE),
    sum(diabetes$BMI == 0, na.rm = TRUE)
  ),
  treatment = c(
    "0 recoded to NA",
    "0 recoded to NA",
    "0 recoded to NA",
    "0 recoded to NA",
    "0 recoded to NA"
  )
)

write.csv(cleaning_decisions, "outputs/cleaning_decisions.csv", row.names = FALSE)

say("This is how the clinically implausible zero values were handled:")
print(cleaning_decisions)

say("This is the number of missing values in each variable after cleaning:")
print(missing_values_clean)

# Create grouped variables used later in the analysis

diabetes_clean <- diabetes_clean %>%
  mutate(
    OutcomeFactor = factor(Outcome, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")),
    PregnancyStatus = if_else(Pregnancies == 0, "Never Pregnant", "Ever Pregnant"),
    AgeGroup = case_when(
      Age < 30 ~ "<30",
      Age >= 30 & Age <= 39 ~ "30-39",
      Age >= 40 & Age <= 49 ~ "40-49",
      Age >= 50 ~ "50+"
    ),
    BMICategory = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    PregnancyGroup = case_when(
      Pregnancies == 0 ~ "0",
      Pregnancies >= 1 & Pregnancies <= 2 ~ "1-2",
      Pregnancies >= 3 & Pregnancies <= 4 ~ "3-4",
      Pregnancies >= 5 ~ "5+"
    )
  ) %>%
  mutate(
    PregnancyStatus = factor(PregnancyStatus, levels = c("Never Pregnant", "Ever Pregnant")),
    AgeGroup = factor(AgeGroup, levels = c("<30", "30-39", "40-49", "50+")),
    BMICategory = factor(BMICategory, levels = c("Underweight", "Normal", "Overweight", "Obese")),
    PregnancyGroup = factor(PregnancyGroup, levels = c("0", "1-2", "3-4", "5+"))
  )

# Summarise the sample

sample_profile <- data.frame(
  sample_size = nrow(diabetes_clean),
  diabetes_count = sum(diabetes_clean$Outcome == 1, na.rm = TRUE),
  no_diabetes_count = sum(diabetes_clean$Outcome == 0, na.rm = TRUE),
  diabetes_percentage = mean(diabetes_clean$Outcome, na.rm = TRUE) * 100
)

write.csv(sample_profile, "outputs/sample_profile.csv", row.names = FALSE)

pregnancy_status_counts <- diabetes_clean %>%
  count(PregnancyStatus)

write.csv(pregnancy_status_counts, "outputs/pregnancy_status_counts.csv", row.names = FALSE)

say("Amount of participants in the sample:", sample_profile$sample_size)
say("Amount of participants with diabetes:", sample_profile$diabetes_count)
say("Amount of participants without diabetes:", sample_profile$no_diabetes_count)
say("This is the percentage of participants diagnosed with diabetes:", round(sample_profile$diabetes_percentage, 2), "%")

say("This is the number of participants who had never been pregnant and those who had been pregnant:")
print(pregnancy_status_counts)

# Calculate descriptive statistics for the required variables

get_mode <- function(x) {
  x <- x[!is.na(x)]
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

descriptive_stats <- function(x) {
  data.frame(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mode = get_mode(x),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
}

age_stats <- descriptive_stats(diabetes_clean$Age)
bmi_stats <- descriptive_stats(diabetes_clean$BMI)
glucose_stats <- descriptive_stats(diabetes_clean$Glucose)
blood_pressure_stats <- descriptive_stats(diabetes_clean$BloodPressure)
pregnancy_stats <- descriptive_stats(diabetes_clean$Pregnancies)

descriptive_table <- bind_rows(
  Age = age_stats,
  BMI = bmi_stats,
  Glucose = glucose_stats,
  BloodPressure = blood_pressure_stats,
  Pregnancies = pregnancy_stats,
  .id = "Variable"
)

write.csv(descriptive_table, "outputs/descriptive_statistics.csv", row.names = FALSE)

descriptive_table_print <- round_numeric_df(descriptive_table, 2)

say("This is the descriptive statistics table for the main variables:")
print(descriptive_table_print)

say("This is the descriptive summary for Age:")
print(round(age_stats, 2))

say("This is the descriptive summary for BMI:")
print(round(bmi_stats, 2))

say("This is the descriptive summary for Glucose:")
print(round(glucose_stats, 2))

say("This is the descriptive summary for Blood Pressure:")
print(round(blood_pressure_stats, 2))

say("This is the descriptive summary for Pregnancies:")
print(round(pregnancy_stats, 2))

# Save histograms and bar charts for the main variables

plot_age_histogram <- ggplot(diabetes_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = colour_blue, colour = "white", linewidth = 0.3) +
  geom_vline(xintercept = mean(diabetes_clean$Age, na.rm = TRUE), colour = colour_coral, linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

ggsave("plots/age_distribution_histogram.png", plot = plot_age_histogram, width = 7, height = 5, dpi = 300)

plot_bmi_histogram <- ggplot(diabetes_clean, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = colour_teal, colour = "white", linewidth = 0.3, na.rm = TRUE) +
  geom_vline(xintercept = mean(diabetes_clean$BMI, na.rm = TRUE), colour = colour_coral, linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of BMI", x = "BMI", y = "Frequency")

ggsave("plots/bmi_distribution_histogram.png", plot = plot_bmi_histogram, width = 7, height = 5, dpi = 300)

plot_glucose_histogram <- ggplot(diabetes_clean, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, fill = colour_amber, colour = "white", linewidth = 0.3, na.rm = TRUE) +
  geom_vline(xintercept = mean(diabetes_clean$Glucose, na.rm = TRUE), colour = colour_coral, linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Glucose", x = "Glucose", y = "Frequency")

ggsave("plots/glucose_distribution_histogram.png", plot = plot_glucose_histogram, width = 7, height = 5, dpi = 300)

plot_blood_pressure_histogram <- ggplot(diabetes_clean, aes(x = BloodPressure)) +
  geom_histogram(binwidth = 5, fill = colour_plum, colour = "white", linewidth = 0.3, na.rm = TRUE) +
  geom_vline(xintercept = mean(diabetes_clean$BloodPressure, na.rm = TRUE), colour = colour_coral, linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Blood Pressure", x = "Blood Pressure", y = "Frequency")

ggsave("plots/blood_pressure_distribution_histogram.png", plot = plot_blood_pressure_histogram, width = 7, height = 5, dpi = 300)

plot_pregnancies_bar <- ggplot(diabetes_clean, aes(x = factor(Pregnancies))) +
  geom_bar(fill = colour_slate, colour = "white", linewidth = 0.3) +
  labs(title = "Distribution of Number of Pregnancies", x = "Pregnancies", y = "Count")

ggsave("plots/pregnancies_bar_chart.png", plot = plot_pregnancies_bar, width = 7, height = 5, dpi = 300)

plot_pregnancy_status_bar <- ggplot(diabetes_clean, aes(x = PregnancyStatus, fill = PregnancyStatus)) +
  geom_bar(colour = "white", linewidth = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c("Never Pregnant" = colour_blue, "Ever Pregnant" = colour_amber)) +
  labs(title = "Pregnancy Status", x = "Pregnancy Status", y = "Count")

ggsave("plots/pregnancy_status_bar_chart.png", plot = plot_pregnancy_status_bar, width = 7, height = 5, dpi = 300)

# Identify outliers and save boxplots

plot_age_box <- ggplot(diabetes_clean, aes(x = "", y = Age)) +
  geom_boxplot(fill = colour_blue, colour = "#1F1F1F", alpha = 0.85) +
  labs(title = "Age Boxplot", x = NULL, y = "Age")

ggsave("plots/age_boxplot.png", plot = plot_age_box, width = 5, height = 5, dpi = 300)

plot_bmi_box <- ggplot(diabetes_clean, aes(x = "", y = BMI)) +
  geom_boxplot(fill = colour_teal, colour = "#1F1F1F", alpha = 0.85, na.rm = TRUE) +
  labs(title = "BMI Boxplot", x = NULL, y = "BMI")

ggsave("plots/bmi_boxplot.png", plot = plot_bmi_box, width = 5, height = 5, dpi = 300)

plot_glucose_box <- ggplot(diabetes_clean, aes(x = "", y = Glucose)) +
  geom_boxplot(fill = colour_amber, colour = "#1F1F1F", alpha = 0.85, na.rm = TRUE) +
  labs(title = "Glucose Boxplot", x = NULL, y = "Glucose")

ggsave("plots/glucose_boxplot.png", plot = plot_glucose_box, width = 5, height = 5, dpi = 300)

plot_blood_pressure_box <- ggplot(diabetes_clean, aes(x = "", y = BloodPressure)) +
  geom_boxplot(fill = colour_plum, colour = "#1F1F1F", alpha = 0.85, na.rm = TRUE) +
  labs(title = "Blood Pressure Boxplot", x = NULL, y = "Blood Pressure")

ggsave("plots/blood_pressure_boxplot.png", plot = plot_blood_pressure_box, width = 5, height = 5, dpi = 300)

plot_insulin_box <- ggplot(diabetes_clean, aes(x = "", y = Insulin)) +
  geom_boxplot(fill = colour_gold, colour = "#1F1F1F", alpha = 0.85, na.rm = TRUE) +
  labs(title = "Insulin Boxplot", x = NULL, y = "Insulin")

ggsave("plots/insulin_boxplot.png", plot = plot_insulin_box, width = 5, height = 5, dpi = 300)

find_outliers <- function(x) {
  x <- x[!is.na(x)]
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr_value <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr_value
  upper_bound <- q3 + 1.5 * iqr_value
  x[x < lower_bound | x > upper_bound]
}

outlier_summary <- data.frame(
  variable = c("Age", "BMI", "Glucose", "BloodPressure", "Insulin"),
  outlier_count = c(
    length(find_outliers(diabetes_clean$Age)),
    length(find_outliers(diabetes_clean$BMI)),
    length(find_outliers(diabetes_clean$Glucose)),
    length(find_outliers(diabetes_clean$BloodPressure)),
    length(find_outliers(diabetes_clean$Insulin))
  )
)

write.csv(outlier_summary, "outputs/outlier_summary.csv", row.names = FALSE)

plot_outlier_counts <- ggplot(outlier_summary, aes(x = reorder(variable, outlier_count), y = outlier_count, fill = variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Age" = colour_blue,
    "BMI" = colour_teal,
    "Glucose" = colour_amber,
    "BloodPressure" = colour_plum,
    "Insulin" = colour_gold
  )) +
  labs(title = "Outlier Counts by Variable", x = "Variable", y = "Outlier Count")

ggsave("plots/outlier_counts.png", plot = plot_outlier_counts, width = 7, height = 5, dpi = 300)

plot_zero_values <- ggplot(clinical_zero_check, aes(x = reorder(variable, zero_count), y = zero_count, fill = variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Glucose" = colour_amber,
    "BloodPressure" = colour_plum,
    "SkinThickness" = colour_teal,
    "Insulin" = colour_coral,
    "BMI" = colour_green
  )) +
  geom_text(aes(label = zero_count), hjust = -0.1) +
  labs(title = "Clinically Implausible Zero Counts", x = "Variable", y = "Zero Count") +
  expand_limits(y = max(clinical_zero_check$zero_count) * 1.1)

ggsave("plots/clinical_zero_counts.png", plot = plot_zero_values, width = 8, height = 5, dpi = 300)

say("This is the number of IQR-defined outliers in each selected variable:")
print(outlier_summary)

# Calculate diabetes rates by grouped categories

age_diabetes_rate <- diabetes_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    Total = n(),
    DiabetesCount = sum(Outcome == 1, na.rm = TRUE),
    DiabetesRate = mean(Outcome, na.rm = TRUE) * 100,
    .groups = "drop"
  )

write.csv(age_diabetes_rate, "outputs/age_group_diabetes_rate.csv", row.names = FALSE)

plot_age_diabetes_rate <- ggplot(age_diabetes_rate, aes(x = AgeGroup, y = DiabetesRate, fill = AgeGroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(DiabetesRate, 1)), vjust = -0.35) +
  scale_fill_manual(values = palette_age) +
  labs(title = "Diabetes Rate by Age Group", x = "Age Group", y = "Diabetes Rate (%)") +
  expand_limits(y = max(age_diabetes_rate$DiabetesRate) * 1.1)

ggsave("plots/diabetes_rate_by_age_group.png", plot = plot_age_diabetes_rate, width = 7, height = 5, dpi = 300)

bmi_diabetes_rate <- diabetes_clean %>%
  filter(!is.na(BMICategory)) %>%
  group_by(BMICategory) %>%
  summarise(
    Total = n(),
    DiabetesCount = sum(Outcome == 1, na.rm = TRUE),
    DiabetesRate = mean(Outcome, na.rm = TRUE) * 100,
    .groups = "drop"
  )

write.csv(bmi_diabetes_rate, "outputs/bmi_category_diabetes_rate.csv", row.names = FALSE)

plot_bmi_diabetes_rate <- ggplot(bmi_diabetes_rate, aes(x = BMICategory, y = DiabetesRate, fill = BMICategory)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(DiabetesRate, 1)), vjust = -0.35) +
  scale_fill_manual(values = palette_bmi) +
  labs(title = "Diabetes Rate by BMI Category", x = "BMI Category", y = "Diabetes Rate (%)") +
  expand_limits(y = max(bmi_diabetes_rate$DiabetesRate) * 1.1)

ggsave("plots/diabetes_rate_by_bmi_category.png", plot = plot_bmi_diabetes_rate, width = 7, height = 5, dpi = 300)

pregnancy_diabetes_rate <- diabetes_clean %>%
  group_by(PregnancyGroup) %>%
  summarise(
    Total = n(),
    DiabetesCount = sum(Outcome == 1, na.rm = TRUE),
    DiabetesRate = mean(Outcome, na.rm = TRUE) * 100,
    .groups = "drop"
  )

write.csv(pregnancy_diabetes_rate, "outputs/pregnancy_group_diabetes_rate.csv", row.names = FALSE)

plot_pregnancy_diabetes_rate <- ggplot(pregnancy_diabetes_rate, aes(x = PregnancyGroup, y = DiabetesRate, fill = PregnancyGroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(DiabetesRate, 1)), vjust = -0.35) +
  scale_fill_manual(values = palette_pregnancy) +
  labs(title = "Diabetes Rate by Pregnancy Group", x = "Pregnancy Group", y = "Diabetes Rate (%)") +
  expand_limits(y = max(pregnancy_diabetes_rate$DiabetesRate) * 1.1)

ggsave("plots/diabetes_rate_by_pregnancy_group.png", plot = plot_pregnancy_diabetes_rate, width = 7, height = 5, dpi = 300)

plot_age_outcome_fill <- ggplot(diabetes_clean, aes(x = AgeGroup, fill = OutcomeFactor)) +
  geom_bar(position = "fill", colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Diabetes Proportion by Age Group", x = "Age Group", y = "Proportion", fill = "Outcome")

ggsave("plots/diabetes_proportion_by_age_group.png", plot = plot_age_outcome_fill, width = 7, height = 5, dpi = 300)

plot_bmi_outcome_fill <- ggplot(diabetes_clean %>% filter(!is.na(BMICategory)), aes(x = BMICategory, fill = OutcomeFactor)) +
  geom_bar(position = "fill", colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Diabetes Proportion by BMI Category", x = "BMI Category", y = "Proportion", fill = "Outcome")

ggsave("plots/diabetes_proportion_by_bmi_category.png", plot = plot_bmi_outcome_fill, width = 7, height = 5, dpi = 300)

say("This is the diabetes rate across age groups:")
print(round_numeric_df(age_diabetes_rate, 2))

say("This is the diabetes rate across BMI categories:")
print(round_numeric_df(bmi_diabetes_rate, 2))

say("This is the diabetes rate across pregnancy groups:")
print(round_numeric_df(pregnancy_diabetes_rate, 2))

# Compare glucose and pregnancies across outcome groups

glucose_by_outcome <- diabetes_clean %>%
  group_by(OutcomeFactor) %>%
  summarise(
    MeanGlucose = mean(Glucose, na.rm = TRUE),
    SDGlucose = sd(Glucose, na.rm = TRUE),
    N = sum(!is.na(Glucose)),
    .groups = "drop"
  )

write.csv(glucose_by_outcome, "outputs/glucose_by_outcome_summary.csv", row.names = FALSE)

t_test_glucose <- t.test(Glucose ~ OutcomeFactor, data = diabetes_clean)
capture.output(t_test_glucose, file = "outputs/t_test_glucose_by_outcome.txt")

plot_glucose_by_outcome <- ggplot(diabetes_clean, aes(x = OutcomeFactor, y = Glucose, fill = OutcomeFactor)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.85) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Glucose by Diabetes Outcome", x = "Outcome", y = "Glucose") +
  theme(legend.position = "none")

ggsave("plots/glucose_by_outcome_boxplot.png", plot = plot_glucose_by_outcome, width = 7, height = 5, dpi = 300)

pregnancies_by_outcome <- diabetes_clean %>%
  group_by(OutcomeFactor) %>%
  summarise(
    MeanPregnancies = mean(Pregnancies, na.rm = TRUE),
    SDPregnancies = sd(Pregnancies, na.rm = TRUE),
    N = sum(!is.na(Pregnancies)),
    .groups = "drop"
  )

write.csv(pregnancies_by_outcome, "outputs/pregnancies_by_outcome_summary.csv", row.names = FALSE)

t_test_pregnancies <- t.test(Pregnancies ~ OutcomeFactor, data = diabetes_clean)
capture.output(t_test_pregnancies, file = "outputs/t_test_pregnancies_by_outcome.txt")

plot_pregnancies_by_outcome <- ggplot(diabetes_clean, aes(x = OutcomeFactor, y = Pregnancies, fill = OutcomeFactor)) +
  geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Pregnancies by Diabetes Outcome", x = "Outcome", y = "Pregnancies") +
  theme(legend.position = "none")

ggsave("plots/pregnancies_by_outcome_boxplot.png", plot = plot_pregnancies_by_outcome, width = 7, height = 5, dpi = 300)

say("This is the mean glucose level for participants with and without diabetes:")
print(round_numeric_df(glucose_by_outcome, 2))

say("This is the t-test result comparing glucose levels by diabetes outcome:")
print(t_test_glucose)

say("This is the mean number of pregnancies for participants with and without diabetes:")
print(round_numeric_df(pregnancies_by_outcome, 2))

say("This is the t-test result comparing pregnancies by diabetes outcome:")
print(t_test_pregnancies)

# Build and save the correlation matrix

continuous_data <- diabetes_clean %>%
  select(Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigreeFunction, Age)

correlation_matrix <- cor(continuous_data, use = "pairwise.complete.obs")
write.csv(correlation_matrix, "outputs/correlation_matrix.csv", row.names = TRUE)

correlation_long <- as.data.frame(as.table(correlation_matrix))
names(correlation_long) <- c("Var1", "Var2", "Correlation")

plot_correlation_heatmap <- ggplot(correlation_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = colour_navy, mid = "white", high = colour_coral, midpoint = 0) +
  labs(title = "Correlation Matrix", x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/correlation_matrix_heatmap.png", plot = plot_correlation_heatmap, width = 8, height = 6, dpi = 300)

say("This is the correlation matrix for the continuous variables:")
print(round(correlation_matrix, 3))

# Run chi-square tests for age group and BMI category

age_outcome_table <- table(diabetes_clean$AgeGroup, diabetes_clean$OutcomeFactor)
write.csv(as.data.frame.matrix(age_outcome_table), "outputs/age_group_outcome_table.csv")

chi_square_age <- chisq.test(age_outcome_table)
capture.output(chi_square_age, file = "outputs/chisq_age_group_outcome.txt")

bmi_outcome_data <- diabetes_clean %>%
  filter(!is.na(BMICategory))

bmi_outcome_table <- table(bmi_outcome_data$BMICategory, bmi_outcome_data$OutcomeFactor)
write.csv(as.data.frame.matrix(bmi_outcome_table), "outputs/bmi_category_outcome_table.csv")

chi_square_bmi <- chisq.test(bmi_outcome_table, simulate.p.value = TRUE)
capture.output(chi_square_bmi, file = "outputs/chisq_bmi_category_outcome.txt")

say("This is the contingency table for Age Group and Diabetes Outcome:")
print(age_outcome_table)

say("This is the chi-square test result for Age Group and Diabetes Outcome:")
print(chi_square_age)

say("This is the contingency table for BMI Category and Diabetes Outcome:")
print(bmi_outcome_table)

say("This is the chi-square test result for BMI Category and Diabetes Outcome:")
print(chi_square_bmi)

# Compare mean glucose across age groups using ANOVA

glucose_by_age_summary <- diabetes_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    MeanGlucose = mean(Glucose, na.rm = TRUE),
    SDGlucose = sd(Glucose, na.rm = TRUE),
    N = sum(!is.na(Glucose)),
    .groups = "drop"
  )

write.csv(glucose_by_age_summary, "outputs/glucose_by_age_group_summary.csv", row.names = FALSE)

anova_glucose_by_age <- aov(Glucose ~ AgeGroup, data = diabetes_clean)
anova_glucose_summary <- summary(anova_glucose_by_age)
tukey_glucose_by_age <- TukeyHSD(anova_glucose_by_age)

capture.output(anova_glucose_summary, file = "outputs/anova_glucose_by_age_group.txt")
capture.output(tukey_glucose_by_age, file = "outputs/tukey_glucose_by_age_group.txt")

plot_glucose_by_age <- ggplot(diabetes_clean, aes(x = AgeGroup, y = Glucose, fill = AgeGroup)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.85) +
  scale_fill_manual(values = palette_age) +
  labs(title = "Glucose by Age Group", x = "Age Group", y = "Glucose") +
  theme(legend.position = "none")

ggsave("plots/glucose_by_age_group_boxplot.png", plot = plot_glucose_by_age, width = 7, height = 5, dpi = 300)

say("This is the mean glucose level in each age group:")
print(round_numeric_df(glucose_by_age_summary, 2))

say("This is the ANOVA result for glucose across age groups:")
print(anova_glucose_summary)

say("This is the Tukey post hoc comparison for glucose across age groups:")
print(tukey_glucose_by_age)

# Fit the multiple linear regression model for glucose

linear_regression_data <- diabetes_clean %>%
  select(Glucose, Age, BMI, Pregnancies, BloodPressure, SkinThickness, Insulin, DiabetesPedigreeFunction) %>%
  drop_na()

linear_model <- lm(
  Glucose ~ Age + BMI + Pregnancies + BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction,
  data = linear_regression_data
)

linear_model_summary <- summary(linear_model)

linear_model_coefficients <- as.data.frame(linear_model_summary$coefficients)
linear_model_coefficients$Term <- rownames(linear_model_coefficients)
rownames(linear_model_coefficients) <- NULL

linear_model_fit <- data.frame(
  R_Squared = linear_model_summary$r.squared,
  Adjusted_R_Squared = linear_model_summary$adj.r.squared,
  Residual_Standard_Error = linear_model_summary$sigma,
  Observations = nrow(linear_regression_data)
)

write.csv(linear_model_coefficients, "outputs/linear_model_coefficients.csv", row.names = FALSE)
write.csv(linear_model_fit, "outputs/linear_model_fit.csv", row.names = FALSE)
capture.output(linear_model_summary, file = "outputs/linear_model_summary.txt")

png("plots/linear_model_diagnostics.png", width = 1200, height = 1200)
par(mfrow = c(2, 2))
plot(linear_model)
dev.off()

linear_plot_data <- linear_model_coefficients %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    LowerCI = Estimate - 1.96 * `Std. Error`,
    UpperCI = Estimate + 1.96 * `Std. Error`,
    Significant = if_else(`Pr(>|t|)` < 0.05, "p < 0.05", "Not significant"),
    Term = factor(Term, levels = rev(Term))
  )

plot_linear_coefficients <- ggplot(linear_plot_data, aes(y = Term, x = Estimate, colour = Significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = colour_grey) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI), width = 0.15, linewidth = 0.8, orientation = "y") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("p < 0.05" = colour_coral, "Not significant" = colour_navy)) +
  labs(
    title = "Linear Regression Coefficients for Predicting Glucose",
    x = "Coefficient Estimate (approx. 95% CI)",
    y = NULL,
    colour = "Significance"
  )

ggsave("plots/linear_model_coefficients_plot.png", plot = plot_linear_coefficients, width = 8, height = 5.5, dpi = 300)

say("This is the number of complete cases used in the linear regression model:", nrow(linear_regression_data))
say("This is the model-fit summary for the linear regression model:")
print(round_numeric_df(linear_model_fit, 4))

say("This is the coefficient table for the linear regression model:")
print(round_numeric_df(linear_model_coefficients, 4))

say("This is the full summary of the linear regression model:")
print(linear_model_summary)

# Fit the logistic regression model for diabetes outcome

logistic_regression_data <- diabetes_clean %>%
  select(Outcome, BMI, Age, Glucose) %>%
  drop_na()

logistic_model <- glm(Outcome ~ BMI + Age + Glucose, data = logistic_regression_data, family = binomial)
logistic_model_summary <- summary(logistic_model)

logistic_model_coefficients <- as.data.frame(logistic_model_summary$coefficients)
logistic_model_coefficients$Term <- rownames(logistic_model_coefficients)
rownames(logistic_model_coefficients) <- NULL

logistic_model_odds_ratios <- data.frame(
  Term = names(coef(logistic_model)),
  OddsRatio = exp(coef(logistic_model)),
  CI_Lower = exp(confint(logistic_model)[, 1]),
  CI_Upper = exp(confint(logistic_model)[, 2])
)

write.csv(logistic_model_coefficients, "outputs/logistic_model_coefficients.csv", row.names = FALSE)
write.csv(logistic_model_odds_ratios, "outputs/logistic_model_odds_ratios.csv", row.names = FALSE)
capture.output(logistic_model_summary, file = "outputs/logistic_model_summary.txt")

logistic_or_plot_data <- logistic_model_odds_ratios %>%
  filter(Term != "(Intercept)") %>%
  mutate(Term = factor(Term, levels = rev(Term)))

plot_logistic_odds_ratios <- ggplot(logistic_or_plot_data, aes(y = Term, x = OddsRatio)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = colour_grey) +
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.15, linewidth = 0.8, colour = colour_navy, orientation = "y") +
  geom_point(size = 3, colour = colour_coral) +
  labs(
    title = "Odds Ratios from Logistic Regression Predicting Diabetes",
    x = "Odds Ratio (95% CI)",
    y = NULL
  )

ggsave("plots/logistic_model_odds_ratios_plot.png", plot = plot_logistic_odds_ratios, width = 8, height = 5, dpi = 300)

say("This is the number of complete cases used in the logistic regression model:", nrow(logistic_regression_data))
say("This is the coefficient table for the logistic regression model:")
print(round_numeric_df(logistic_model_coefficients, 4))

say("This is the odds ratio table for the logistic regression model:")
print(round_numeric_df(logistic_model_odds_ratios, 4))

say("This is the full summary of the logistic regression model:")
print(logistic_model_summary)

# Calculate classification accuracy, sensitivity, and specificity

predicted_probabilities <- predict(logistic_model, type = "response")
predicted_class <- ifelse(predicted_probabilities >= 0.5, 1, 0)

confusion_matrix <- table(Predicted = predicted_class, Actual = logistic_regression_data$Outcome)
write.csv(as.data.frame.matrix(confusion_matrix), "outputs/logistic_confusion_matrix.csv")

true_negative <- ifelse(
  all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)),
  confusion_matrix["0", "0"], 0
)

true_positive <- ifelse(
  all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)),
  confusion_matrix["1", "1"], 0
)

false_negative <- ifelse(
  all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)),
  confusion_matrix["0", "1"], 0
)

false_positive <- ifelse(
  all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)),
  confusion_matrix["1", "0"], 0
)

accuracy <- (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative)
sensitivity <- ifelse((true_positive + false_negative) == 0, NA, true_positive / (true_positive + false_negative))
specificity <- ifelse((true_negative + false_positive) == 0, NA, true_negative / (true_negative + false_positive))

classification_metrics <- data.frame(
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity
)

write.csv(classification_metrics, "outputs/logistic_classification_metrics.csv", row.names = FALSE)

plot_predicted_probabilities <- data.frame(
  PredictedProbability = predicted_probabilities,
  OutcomeFactor = factor(logistic_regression_data$Outcome, levels = c(0, 1), labels = c("No Diabetes", "Diabetes"))
) %>%
  ggplot(aes(x = PredictedProbability, fill = OutcomeFactor)) +
  geom_histogram(position = "identity", alpha = 0.65, bins = 25, colour = "white") +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Predicted Probabilities by Actual Outcome", x = "Predicted Probability", y = "Count", fill = "Outcome")

ggsave("plots/predicted_probabilities_by_outcome.png", plot = plot_predicted_probabilities, width = 7, height = 5, dpi = 300)

say("This is the confusion matrix for the logistic regression model:")
print(confusion_matrix)

say("This is the classification performance of the logistic regression model:")
print(round_numeric_df(classification_metrics, 4))

say("This means the model accuracy is:", round(accuracy, 4))
say("This means the model sensitivity is:", round(sensitivity, 4))
say("This means the model specificity is:", round(specificity, 4))

# Carry out the Hosmer-Lemeshow goodness-of-fit test

hosmer_lemeshow_test <- function(observed, predicted, groups = 10) {
  hl_data <- data.frame(observed = observed, predicted = predicted) %>%
    mutate(group = ntile(predicted, groups)) %>%
    group_by(group) %>%
    summarise(
      observed_1 = sum(observed == 1),
      observed_0 = sum(observed == 0),
      expected_1 = sum(predicted),
      expected_0 = sum(1 - predicted),
      .groups = "drop"
    ) %>%
    mutate(
      chi_component_1 = ifelse(expected_1 == 0, 0, (observed_1 - expected_1)^2 / expected_1),
      chi_component_0 = ifelse(expected_0 == 0, 0, (observed_0 - expected_0)^2 / expected_0)
    )

  hl_statistic <- sum(hl_data$chi_component_1 + hl_data$chi_component_0)
  df <- groups - 2
  p_value <- 1 - pchisq(hl_statistic, df)

  list(
    table = hl_data,
    statistic = hl_statistic,
    df = df,
    p_value = p_value
  )
}

hosmer_lemeshow_result <- hosmer_lemeshow_test(
  logistic_regression_data$Outcome,
  predicted_probabilities,
  groups = 10
)

write.csv(hosmer_lemeshow_result$table, "outputs/hosmer_lemeshow_table.csv", row.names = FALSE)

write.csv(
  data.frame(
    HL_Statistic = hosmer_lemeshow_result$statistic,
    Degrees_of_Freedom = hosmer_lemeshow_result$df,
    P_Value = hosmer_lemeshow_result$p_value
  ),
  "outputs/hosmer_lemeshow_result.csv",
  row.names = FALSE
)

say("This is the Hosmer-Lemeshow grouping table:")
print(round_numeric_df(hosmer_lemeshow_result$table, 4))

say("This is the Hosmer-Lemeshow test statistic:", round(hosmer_lemeshow_result$statistic, 4))
say("This is the degrees of freedom for the Hosmer-Lemeshow test:", hosmer_lemeshow_result$df)
say("This is the p-value for the Hosmer-Lemeshow test:", signif(hosmer_lemeshow_result$p_value, 4))

# Compare the logistic models with and without the BMI by age interaction

logistic_comparison_data <- diabetes_clean %>%
  select(Outcome, BMI, Age, Glucose, Pregnancies) %>%
  drop_na()

logistic_model_1 <- glm(
  Outcome ~ BMI + Age + Glucose + Pregnancies,
  data = logistic_comparison_data,
  family = binomial
)

logistic_model_2 <- glm(
  Outcome ~ BMI * Age + Glucose + Pregnancies,
  data = logistic_comparison_data,
  family = binomial
)

model_comparison <- anova(logistic_model_1, logistic_model_2, test = "Chisq")
model_comparison_data_frame <- as.data.frame(model_comparison)

write.csv(model_comparison_data_frame, "outputs/logistic_model_comparison.csv", row.names = TRUE)

aic_comparison <- data.frame(
  Model = c("Model 1: Main effects", "Model 2: Main effects + BMI×Age"),
  AIC = c(AIC(logistic_model_1), AIC(logistic_model_2))
)

write.csv(aic_comparison, "outputs/logistic_model_aic.csv", row.names = FALSE)

logistic_model_2_summary <- summary(logistic_model_2)

logistic_model_2_coefficients <- as.data.frame(logistic_model_2_summary$coefficients)
logistic_model_2_coefficients$Term <- rownames(logistic_model_2_coefficients)
rownames(logistic_model_2_coefficients) <- NULL

logistic_model_2_odds_ratios <- data.frame(
  Term = names(coef(logistic_model_2)),
  OddsRatio = exp(coef(logistic_model_2)),
  CI_Lower = exp(confint(logistic_model_2)[, 1]),
  CI_Upper = exp(confint(logistic_model_2)[, 2])
)

write.csv(logistic_model_2_coefficients, "outputs/logistic_interaction_model_coefficients.csv", row.names = FALSE)
write.csv(logistic_model_2_odds_ratios, "outputs/logistic_interaction_model_odds_ratios.csv", row.names = FALSE)

capture.output(summary(logistic_model_1), file = "outputs/logistic_model_1_summary.txt")
capture.output(summary(logistic_model_2), file = "outputs/logistic_model_2_summary.txt")
capture.output(model_comparison, file = "outputs/logistic_model_comparison.txt")

plot_aic_comparison <- ggplot(aic_comparison, aes(x = Model, y = AIC, fill = Model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(AIC, 2)), vjust = -0.35) +
  scale_fill_manual(values = c(
    "Model 1: Main effects" = colour_navy,
    "Model 2: Main effects + BMI×Age" = colour_coral
  )) +
  labs(title = "AIC Comparison of Logistic Models", x = NULL, y = "AIC") +
  expand_limits(y = max(aic_comparison$AIC) * 1.05)

ggsave("plots/logistic_model_aic_comparison.png", plot = plot_aic_comparison, width = 8, height = 5, dpi = 300)

representative_ages <- c(25, 35, 45, 55)

interaction_prediction_grid <- expand.grid(
  BMI = seq(
    min(logistic_comparison_data$BMI, na.rm = TRUE),
    max(logistic_comparison_data$BMI, na.rm = TRUE),
    length.out = 150
  ),
  Age = representative_ages,
  Glucose = mean(logistic_comparison_data$Glucose, na.rm = TRUE),
  Pregnancies = median(logistic_comparison_data$Pregnancies, na.rm = TRUE)
)

interaction_prediction_grid$PredictedProbability <- predict(
  logistic_model_2,
  newdata = interaction_prediction_grid,
  type = "response"
)

interaction_prediction_grid$AgeLabel <- factor(
  paste("Age", interaction_prediction_grid$Age),
  levels = c("Age 25", "Age 35", "Age 45", "Age 55")
)

write.csv(interaction_prediction_grid, "outputs/interaction_prediction_grid.csv", row.names = FALSE)

plot_interaction_lines <- ggplot(interaction_prediction_grid,
                                 aes(x = BMI, y = PredictedProbability, colour = AgeLabel)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = palette_age_lines) +
  labs(
    title = "Predicted Diabetes Risk Across BMI at Representative Ages",
    subtitle = "From logistic Model 2; glucose fixed at mean and pregnancies fixed at median",
    x = "BMI",
    y = "Predicted Probability of Diabetes",
    colour = NULL
  )

ggsave("plots/interaction_predicted_probability_lines.png", plot = plot_interaction_lines, width = 8, height = 5, dpi = 300)

interaction_heatmap_grid <- expand.grid(
  BMI = seq(
    min(logistic_comparison_data$BMI, na.rm = TRUE),
    max(logistic_comparison_data$BMI, na.rm = TRUE),
    length.out = 100
  ),
  Age = seq(
    min(logistic_comparison_data$Age, na.rm = TRUE),
    max(logistic_comparison_data$Age, na.rm = TRUE),
    length.out = 100
  ),
  Glucose = mean(logistic_comparison_data$Glucose, na.rm = TRUE),
  Pregnancies = median(logistic_comparison_data$Pregnancies, na.rm = TRUE)
)

interaction_heatmap_grid$PredictedProbability <- predict(
  logistic_model_2,
  newdata = interaction_heatmap_grid,
  type = "response"
)

write.csv(interaction_heatmap_grid, "outputs/interaction_heatmap_grid.csv", row.names = FALSE)

plot_interaction_heatmap <- ggplot(interaction_heatmap_grid,
                                   aes(x = BMI, y = Age, fill = PredictedProbability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = colour_coral) +
  labs(
    title = "Predicted Diabetes Probability by BMI and Age",
    subtitle = "From logistic Model 2; glucose fixed at mean and pregnancies fixed at median",
    x = "BMI",
    y = "Age",
    fill = "Predicted\nProbability"
  )

ggsave("plots/interaction_predicted_probability_heatmap.png", plot = plot_interaction_heatmap, width = 8, height = 6, dpi = 300)

say("This is the model comparison table for the two logistic regression models:")
print(model_comparison)

say("This is the AIC comparison for the two logistic regression models:")
print(round_numeric_df(aic_comparison, 3))

say("This is the coefficient table for the interaction model:")
print(round_numeric_df(logistic_model_2_coefficients, 4))

say("This is the odds ratio table for the interaction model:")
print(round_numeric_df(logistic_model_2_odds_ratios, 4))

# Save the outcome chart and outcome percentages

outcome_counts_plot_data <- diabetes_clean %>%
  count(OutcomeFactor) %>%
  mutate(Percent = n / sum(n) * 100)

plot_outcome_bar <- ggplot(outcome_counts_plot_data, aes(x = OutcomeFactor, y = Percent, fill = OutcomeFactor)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), vjust = -0.35) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Percentage of Participants With and Without Diabetes",
       x = "Outcome",
       y = "Percentage") +
  expand_limits(y = max(outcome_counts_plot_data$Percent) * 1.1)

ggsave("plots/outcome_bar_chart_percent.png", plot = plot_outcome_bar, width = 7, height = 5, dpi = 300)

write.csv(as.data.frame(table(diabetes_clean$OutcomeFactor)), "outputs/outcome_counts.csv", row.names = FALSE)
write.csv(as.data.frame(prop.table(table(diabetes_clean$OutcomeFactor)) * 100), "outputs/outcome_percentages.csv", row.names = FALSE)

# Summarise usable sample sizes for each variable set and model

analysis_sample_sizes <- data.frame(
  Analysis = c(
    "Raw dataset",
    "Cleaned dataset",
    "Non-missing Glucose",
    "Non-missing BloodPressure",
    "Non-missing SkinThickness",
    "Non-missing Insulin",
    "Non-missing BMI",
    "Linear regression sample",
    "Logistic regression sample",
    "Logistic comparison sample"
  ),
  N = c(
    nrow(diabetes),
    nrow(diabetes_clean),
    sum(!is.na(diabetes_clean$Glucose)),
    sum(!is.na(diabetes_clean$BloodPressure)),
    sum(!is.na(diabetes_clean$SkinThickness)),
    sum(!is.na(diabetes_clean$Insulin)),
    sum(!is.na(diabetes_clean$BMI)),
    nrow(linear_regression_data),
    nrow(logistic_regression_data),
    nrow(logistic_comparison_data)
  )
)

write.csv(analysis_sample_sizes, "outputs/analysis_sample_sizes.csv", row.names = FALSE)

plot_analysis_sample_sizes <- ggplot(analysis_sample_sizes,
                                     aes(x = reorder(Analysis, N), y = N, fill = Analysis)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = N), hjust = -0.1) +
  labs(title = "Usable Sample Size by Variable or Model", x = NULL, y = "N") +
  expand_limits(y = max(analysis_sample_sizes$N) * 1.12)

ggsave("plots/analysis_sample_sizes.png", plot = plot_analysis_sample_sizes, width = 8.5, height = 6, dpi = 300)

say("This is the usable sample size for each variable set or model:")
print(analysis_sample_sizes)

# Print and save a final summary of the main results

anova_p_value <- anova_glucose_summary[[1]][["Pr(>F)"]][1]

interaction_model_p_value <- NA
if ("Pr(>Chi)" %in% names(model_comparison_data_frame) && nrow(model_comparison_data_frame) >= 2) {
  interaction_model_p_value <- model_comparison_data_frame$`Pr(>Chi)`[2]
}

key_results_lines <- c(
  "Summary",
  paste("Sample size:", nrow(diabetes_clean)),
  paste("Diabetes cases:", sum(diabetes_clean$Outcome == 1, na.rm = TRUE)),
  paste("Diabetes percentage:", round(mean(diabetes_clean$Outcome, na.rm = TRUE) * 100, 2), "%"),
  "",
  "Data-quality handling",
  paste("Glucose zeros recoded to NA:", sum(diabetes$Glucose == 0, na.rm = TRUE)),
  paste("BloodPressure zeros recoded to NA:", sum(diabetes$BloodPressure == 0, na.rm = TRUE)),
  paste("SkinThickness zeros recoded to NA:", sum(diabetes$SkinThickness == 0, na.rm = TRUE)),
  paste("Insulin zeros recoded to NA:", sum(diabetes$Insulin == 0, na.rm = TRUE)),
  paste("BMI zeros recoded to NA:", sum(diabetes$BMI == 0, na.rm = TRUE)),
  "",
  paste("T-test p-value for glucose by outcome:", signif(t_test_glucose$p.value, 4)),
  paste("T-test p-value for pregnancies by outcome:", signif(t_test_pregnancies$p.value, 4)),
  paste("Chi-square p-value for age group and outcome:", signif(chi_square_age$p.value, 4)),
  paste("Chi-square p-value for BMI category and outcome:", signif(chi_square_bmi$p.value, 4)),
  paste("ANOVA p-value for glucose across age groups:", signif(anova_p_value, 4)),
  "",
  paste("Linear model adjusted R-squared:", round(linear_model_summary$adj.r.squared, 4)),
  paste("Logistic model accuracy:", round(accuracy, 4)),
  paste("Logistic model sensitivity:", round(sensitivity, 4)),
  paste("Logistic model specificity:", round(specificity, 4)),
  paste("Hosmer-Lemeshow p-value:", signif(hosmer_lemeshow_result$p_value, 4)),
  paste("Interaction model comparison p-value:", signif(interaction_model_p_value, 4)),
  "",
  paste("Model 1 AIC:", round(AIC(logistic_model_1), 3)),
  paste("Model 2 AIC:", round(AIC(logistic_model_2), 3))
)

writeLines(key_results_lines, "outputs/key_results_summary.txt")

cat(paste(key_results_lines, collapse = "\n"), "\n")

# Save session information for reference

capture.output(sessionInfo(), file = "outputs/session_info.txt")