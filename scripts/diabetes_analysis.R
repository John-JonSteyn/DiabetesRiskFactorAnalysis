# Install packages
required_packages <- c("tidyverse", "psych")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load packages
library(tidyverse)
library(psych)

# Create folders
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

# Clear previous outputs
unlink(list.files("outputs", full.names = TRUE))
unlink(list.files("plots", full.names = TRUE))

# Set plot theme and colours
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

colour_navy <- "#355C7D"
colour_blue <- "#4C78A8"
colour_teal <- "#3B8EA5"
colour_green <- "#5AA469"
colour_olive <- "#7A9E7E"
colour_amber <- "#D9A441"
colour_gold <- "#E0B04B"
colour_coral <- "#D96C5F"
colour_plum <- "#8E6C8A"
colour_pale <- "#9CCFD8"
colour_slate <- "#577590"

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

# Import data
diabetes <- read_csv("data/diabetes.csv", show_col_types = FALSE)

# Inspect data
dim(diabetes)
nrow(diabetes)
ncol(diabetes)
names(diabetes)
str(diabetes)
head(diabetes)
summary(diabetes)

# Audit values
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

# Clean data
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

# Create grouped variables
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

# Profile sample
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

# Descriptive statistics
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

# Variable distributions
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

# Outlier plots
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

# Outlier counts
find_outliers <- function(x) {
  x <- x[!is.na(x)]
  lower_quartile <- quantile(x, 0.25)
  upper_quartile <- quantile(x, 0.75)
  iqr_value <- upper_quartile - lower_quartile
  lower_bound <- lower_quartile - 1.5 * iqr_value
  upper_bound <- upper_quartile + 1.5 * iqr_value
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

# Zero-value diagnostic
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

# Diabetes rates by groups
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

# Proportion plots
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

# Group comparisons
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

# Additional comparisons
bmi_by_outcome <- diabetes_clean %>%
  group_by(OutcomeFactor) %>%
  summarise(
    MeanBMI = mean(BMI, na.rm = TRUE),
    SDBMI = sd(BMI, na.rm = TRUE),
    N = sum(!is.na(BMI)),
    .groups = "drop"
  )

write.csv(bmi_by_outcome, "outputs/bmi_by_outcome_summary.csv", row.names = FALSE)

t_test_bmi <- t.test(BMI ~ OutcomeFactor, data = diabetes_clean)
capture.output(t_test_bmi, file = "outputs/t_test_bmi_by_outcome.txt")

plot_bmi_by_outcome <- ggplot(diabetes_clean, aes(x = OutcomeFactor, y = BMI, fill = OutcomeFactor)) +
  geom_boxplot(alpha = 0.85, na.rm = TRUE) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "BMI by Diabetes Outcome", x = "Outcome", y = "BMI") +
  theme(legend.position = "none")

ggsave("plots/bmi_by_outcome_boxplot.png", plot = plot_bmi_by_outcome, width = 7, height = 5, dpi = 300)

age_by_outcome <- diabetes_clean %>%
  group_by(OutcomeFactor) %>%
  summarise(
    MeanAge = mean(Age, na.rm = TRUE),
    SDAge = sd(Age, na.rm = TRUE),
    N = sum(!is.na(Age)),
    .groups = "drop"
  )

write.csv(age_by_outcome, "outputs/age_by_outcome_summary.csv", row.names = FALSE)

t_test_age <- t.test(Age ~ OutcomeFactor, data = diabetes_clean)
capture.output(t_test_age, file = "outputs/t_test_age_by_outcome.txt")

plot_age_by_outcome <- ggplot(diabetes_clean, aes(x = OutcomeFactor, y = Age, fill = OutcomeFactor)) +
  geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Age by Diabetes Outcome", x = "Outcome", y = "Age") +
  theme(legend.position = "none")

ggsave("plots/age_by_outcome_boxplot.png", plot = plot_age_by_outcome, width = 7, height = 5, dpi = 300)

diabetes_pedigree_by_outcome <- diabetes_clean %>%
  group_by(OutcomeFactor) %>%
  summarise(
    MeanDPF = mean(DiabetesPedigreeFunction, na.rm = TRUE),
    SDDPF = sd(DiabetesPedigreeFunction, na.rm = TRUE),
    N = sum(!is.na(DiabetesPedigreeFunction)),
    .groups = "drop"
  )

write.csv(diabetes_pedigree_by_outcome, "outputs/dpf_by_outcome_summary.csv", row.names = FALSE)

t_test_diabetes_pedigree <- t.test(DiabetesPedigreeFunction ~ OutcomeFactor, data = diabetes_clean)
capture.output(t_test_diabetes_pedigree, file = "outputs/t_test_dpf_by_outcome.txt")

plot_diabetes_pedigree_by_outcome <- ggplot(diabetes_clean, aes(x = OutcomeFactor, y = DiabetesPedigreeFunction, fill = OutcomeFactor)) +
  geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = palette_outcome) +
  labs(title = "Diabetes Pedigree Function by Outcome", x = "Outcome", y = "Diabetes Pedigree Function") +
  theme(legend.position = "none")

ggsave("plots/dpf_by_outcome_boxplot.png", plot = plot_diabetes_pedigree_by_outcome, width = 7, height = 5, dpi = 300)

# Correlation analysis
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

# Scatter plots
plot_bmi_vs_glucose <- ggplot(diabetes_clean, aes(x = BMI, y = Glucose, colour = OutcomeFactor)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, na.rm = TRUE) +
  scale_colour_manual(values = palette_outcome) +
  labs(title = "BMI and Glucose by Diabetes Outcome", x = "BMI", y = "Glucose", colour = "Outcome")

ggsave("plots/bmi_vs_glucose_by_outcome.png", plot = plot_bmi_vs_glucose, width = 7, height = 5, dpi = 300)

plot_age_vs_glucose <- ggplot(diabetes_clean, aes(x = Age, y = Glucose, colour = OutcomeFactor)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, na.rm = TRUE) +
  scale_colour_manual(values = palette_outcome) +
  labs(title = "Age and Glucose by Diabetes Outcome", x = "Age", y = "Glucose", colour = "Outcome")

ggsave("plots/age_vs_glucose_by_outcome.png", plot = plot_age_vs_glucose, width = 7, height = 5, dpi = 300)

# Association tests
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

# ANOVA
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
capture.output(summary(anova_glucose_by_age), file = "outputs/anova_glucose_by_age_group.txt")
capture.output(TukeyHSD(anova_glucose_by_age), file = "outputs/tukey_glucose_by_age_group.txt")

plot_glucose_by_age <- ggplot(diabetes_clean, aes(x = AgeGroup, y = Glucose, fill = AgeGroup)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.85) +
  scale_fill_manual(values = palette_age) +
  labs(title = "Glucose by Age Group", x = "Age Group", y = "Glucose") +
  theme(legend.position = "none")

ggsave("plots/glucose_by_age_group_boxplot.png", plot = plot_glucose_by_age, width = 7, height = 5, dpi = 300)

# Linear regression
linear_regression_data <- diabetes_clean %>%
  select(Glucose, Age, BMI, Pregnancies, BloodPressure, SkinThickness, Insulin, DiabetesPedigreeFunction) %>%
  drop_na()

linear_model <- lm(
  Glucose ~ Age + BMI + Pregnancies + BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction,
  data = linear_regression_data
)

linear_model_coefficients <- as.data.frame(summary(linear_model)$coefficients)
linear_model_coefficients$Term <- rownames(linear_model_coefficients)
rownames(linear_model_coefficients) <- NULL

linear_model_fit <- data.frame(
  R_Squared = summary(linear_model)$r.squared,
  Adjusted_R_Squared = summary(linear_model)$adj.r.squared,
  Residual_Standard_Error = summary(linear_model)$sigma,
  Observations = nrow(linear_regression_data)
)

write.csv(linear_model_coefficients, "outputs/linear_model_coefficients.csv", row.names = FALSE)
write.csv(linear_model_fit, "outputs/linear_model_fit.csv", row.names = FALSE)
capture.output(summary(linear_model), file = "outputs/linear_model_summary.txt")

png("plots/linear_model_diagnostics.png", width = 1200, height = 1200)
par(mfrow = c(2, 2))
plot(linear_model)
dev.off()

# Logistic regression
logistic_regression_data <- diabetes_clean %>%
  select(Outcome, BMI, Age, Glucose) %>%
  drop_na()

logistic_model <- glm(Outcome ~ BMI + Age + Glucose, data = logistic_regression_data, family = binomial)

logistic_model_coefficients <- as.data.frame(summary(logistic_model)$coefficients)
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
capture.output(summary(logistic_model), file = "outputs/logistic_model_summary.txt")

# Logistic model evaluation
predicted_probabilities <- predict(logistic_model, type = "response")
predicted_class <- ifelse(predicted_probabilities >= 0.5, 1, 0)

confusion_matrix <- table(Predicted = predicted_class, Actual = logistic_regression_data$Outcome)
write.csv(as.data.frame.matrix(confusion_matrix), "outputs/logistic_confusion_matrix.csv")

true_negative <- ifelse(all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)), confusion_matrix["0", "0"], 0)
true_positive <- ifelse(all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)), confusion_matrix["1", "1"], 0)
false_negative <- ifelse(all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)), confusion_matrix["0", "1"], 0)
false_positive <- ifelse(all(c("0", "1") %in% rownames(confusion_matrix)) && all(c("0", "1") %in% colnames(confusion_matrix)), confusion_matrix["1", "0"], 0)

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

# Hosmer-Lemeshow test
hosmer_lemeshow_test <- function(observed, predicted, groups = 10) {
  hosmer_lemeshow_data <- data.frame(observed = observed, predicted = predicted) %>%
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

  hosmer_lemeshow_statistic <- sum(hosmer_lemeshow_data$chi_component_1 + hosmer_lemeshow_data$chi_component_0)
  degrees_of_freedom <- groups - 2
  p_value <- 1 - pchisq(hosmer_lemeshow_statistic, degrees_of_freedom)

  list(
    table = hosmer_lemeshow_data,
    statistic = hosmer_lemeshow_statistic,
    df = degrees_of_freedom,
    p_value = p_value
  )
}

hosmer_lemeshow_result <- hosmer_lemeshow_test(logistic_regression_data$Outcome, predicted_probabilities, groups = 10)

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

# Interaction model
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
  Model = c("Model_1_Main_Effects", "Model_2_Main_Effects_Interaction"),
  AIC = c(AIC(logistic_model_1), AIC(logistic_model_2))
)

write.csv(aic_comparison, "outputs/logistic_model_aic.csv", row.names = FALSE)

logistic_model_2_coefficients <- as.data.frame(summary(logistic_model_2)$coefficients)
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

# Outcome summaries
write.csv(as.data.frame(table(diabetes_clean$OutcomeFactor)), "outputs/outcome_counts.csv", row.names = FALSE)
write.csv(as.data.frame(prop.table(table(diabetes_clean$OutcomeFactor)) * 100), "outputs/outcome_percentages.csv", row.names = FALSE)

# Key results summary
anova_p_value <- summary(anova_glucose_by_age)[[1]][["Pr(>F)"]][1]

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
  paste("T-test p-value for glucose by outcome:", signif(t_test_glucose$p.value, 4)),
  paste("T-test p-value for pregnancies by outcome:", signif(t_test_pregnancies$p.value, 4)),
  paste("T-test p-value for BMI by outcome:", signif(t_test_bmi$p.value, 4)),
  paste("T-test p-value for age by outcome:", signif(t_test_age$p.value, 4)),
  paste("T-test p-value for diabetes pedigree function by outcome:", signif(t_test_diabetes_pedigree$p.value, 4)),
  "",
  paste("Chi-square p-value for age group and outcome:", signif(chi_square_age$p.value, 4)),
  paste("Chi-square p-value for BMI category and outcome:", signif(chi_square_bmi$p.value, 4)),
  paste("ANOVA p-value for glucose across age groups:", signif(anova_p_value, 4)),
  "",
  paste("Linear model adjusted R-squared:", round(summary(linear_model)$adj.r.squared, 4)),
  paste("Logistic model accuracy:", round(accuracy, 4)),
  paste("Logistic model sensitivity:", round(sensitivity, 4)),
  paste("Logistic model specificity:", round(specificity, 4)),
  paste("Hosmer-Lemeshow p-value:", signif(hosmer_lemeshow_result$p_value, 4)),
  paste("Interaction model comparison p-value:", signif(interaction_model_p_value, 4))
)

cat(paste(key_results_lines, collapse = "\n"), "\n")

writeLines(key_results_lines, "outputs/key_results_summary.txt")

# Session info
capture.output(sessionInfo(), file = "outputs/session_info.txt")