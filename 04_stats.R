## Soil nutrient plots 
## Joe Collins 
## 2024-10-05

## 04 STATS ####

source(file = "scripts/02_data.R")

source(file = "scripts/functions/shapiro_wilks.R")
source(file = "scripts/functions/fun_bartlett_test.R")
source(file = "scripts/functions/fun_glm_by_year.R")



dat <- read_excel(path = "data/processed/soil_data.xlsx")

### 01  Normality ####

### 01 SHAPIRO WILK ####

# Load the data
data <- dat

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[7:ncol(data)]

dir.create(path = "stats/normality/")

# Initialize an empty data frame to store results
result_df <- data.frame()

check_normality(data = data, columns_of_interest = columns_of_interest)

write.csv(x = result_df, file = "stats/normality/shap_wilks_by_year.csv", 
          row.names = FALSE)



## 02 BARTLETT TEST ####

# Load the data
data <- dat

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[7:ncol(data)]

dir.create(path = "stats/normality/")

# Initialize an empty data frame to store results
result_df <- data.frame()

check_variance_homogeneity(data = data, columns_of_interest = columns_of_interest)

write.csv(x = bartlett_result_df, file = "stats/normality/bartlett_test_by_year.csv", 
          row.names = FALSE)






### 03 glm ###

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(dat)[7:11]

run_glm_and_pairwise(data = dat, columns_to_run_glm = columns_of_interest)








### bulk density ####


# Define the linear mixed-effects model formula
lm_formula <- `Bulk Density_g_cm3` ~ Treatment * Year + (1 | Block)

# Fit the linear mixed-effects model
lm <- lmer(formula = lm_formula, data = dat)

# Get the summary which includes p-values
summary(lm)

# You can also use anova to get Type III sums of squares and p-values
anova(lm)

# Calculate EMMs for Treatment
emm_results <- emmeans(lm, ~ Treatment | Year)

# Pairwise comparisons
pairwise_results <- pairs(emm_results)
summary(pairwise_results)

# Pairwise comparisons with Tukey adjustment
pairwise_results_tukey <- pairs(emm_results, adjust = "tukey")
summary(pairwise_results_tukey)

# Create directory for saving outputs
dir.create(path = "stats/lm_outputs")

# Capture the output of summary(lm_model) along with the formula
lm_output <- capture.output(
  cat("Linear Mixed-Effects Model Formula: ", deparse(lm_formula), "\n\n"),
  summary(lm)
)

# Write the output to a text file
writeLines(lm_output, "stats/lm_outputs/bulk_density.txt")

# Capture pairwise comparison results
p_comp <- capture.output(pairwise_results_tukey)

# Write the output to a text file
writeLines(p_comp, "stats/lm_outputs/bulk_density_p_comp.txt")






## P ###

# Run the GLM model (assuming a quasipoisson family)
glm_model <- glm(Phosphorus_mg_l ~ Treatment * Year, data = dat, family = quasipoisson)

summary(glm_model)


# Perform pairwise comparisons using emmeans
emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)

summary(emmeans_res)

pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
















