## Soil nutrient plots 
## Joe Collins 
## 2024-10-05

#_________________________________________________####
# STATS ####
#_________________________________________________####


#_________________________________________________####
# Data ####

dat <- read_excel(path = "sym_link_soil_chemistry/data/processed/soil_data.xlsx")


dat$Year <- as.factor(dat$Year)
dat$Treatment <- as.factor(dat$Treatment)
dat$Block <- as.factor(dat$Block)


dat$Treatment <- factor(dat$Treatment, 
                        levels = c("Baseline","Conventional", "Conservation"))


#_________________________________________________####
# Functions  ####


source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")



#_________________________________________________####
# Variables  ####


# ~ P ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Phosphorus_mg_l, na.rm = TRUE),
    sd = sd(Phosphorus_mg_l, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Phosphorus_mg_l, 
                   colour = dat$Phosphorus_mg_l)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_Phosphorus_mg_l.png", width = 10, height = 2.25)

# Find the smallest nonzero value in the dataset
small_const <- min(dat$Phosphorus_mg_l[dat$Phosphorus_mg_l > 0]) * 0.01  # 1% of the smallest value

# Add this small constant to avoid zeros
dat$Phosphorus_mg_l_adj <- dat$Phosphorus_mg_l + small_const

# Fit the GLMM model
glm_model <- glmer(Phosphorus_mg_l_adj ~ Treatment + (1 | Year) + (1 | Block), 
                   data = dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_Phosphorus_mg_l.png", 
       width = 10, height = 3.5)




# ~ K ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Potassium_mg_l, na.rm = TRUE),
    sd = sd(Potassium_mg_l, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Potassium_mg_l, 
                   colour = dat$Potassium_mg_l)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_Potassium_mg_l.png", width = 10, height = 2.25)

# # Find the smallest nonzero value in the dataset
# small_const <- min(dat$Potassium_mg_l[dat$Potassium_mg_l > 0]) * 0.01  # 1% of the smallest value
# 
# # Add this small constant to avoid zeros
# dat$Potassium_mg_l_adj <- dat$Potassium_mg_l + small_const

# Fit the GLMM model
glm_model <- glmer(Potassium_mg_l ~ Treatment + (1 | Year) + (1 | Block), 
                   data = dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_Potassium_mg_l.png", 
       width = 10, height = 3.5)




# ~ Mg ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Magnesium_mg_l, na.rm = TRUE),
    sd = sd(Magnesium_mg_l, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Magnesium_mg_l, 
                   colour = dat$Magnesium_mg_l)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_Magnesium_mg_l.png", width = 10, height = 2.25)

# # Find the smallest nonzero value in the dataset
# small_const <- min(dat$Magnesium_mg_l[dat$Magnesium_mg_l > 0]) * 0.01  # 1% of the smallest value
# 
# # Add this small constant to avoid zeros
# dat$Magnesium_mg_l_adj <- dat$Magnesium_mg_l + small_const

# Fit the GLMM model
glm_model <- glmer(Magnesium_mg_l ~ Treatment + (1 | Year) + (1 | Block), 
                   data = dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_Magnesium_mg_l.png", 
       width = 10, height = 3.5)




# ~ pH ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    sum = sum(pH, na.rm = TRUE),
    sd = sd(pH, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$pH, 
                   colour = dat$pH)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_pH.png", width = 10, height = 2.25)

# # Find the smallest nonzero value in the dataset
# small_const <- min(dat$Magnesium_mg_l[dat$Magnesium_mg_l > 0]) * 0.01  # 1% of the smallest value
# 
# # Add this small constant to avoid zeros
# dat$Magnesium_mg_l_adj <- dat$Magnesium_mg_l + small_const

# Fit the GLMM model
glm_model <- glmer(pH ~ Treatment + (1 | Year) + (1 | Block), 
                   data = dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_pH.png", 
       width = 10, height = 3.5)





# ~ BD ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Bulk_density, na.rm = TRUE),
    sd = sd(Bulk_density, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Bulk_density, 
                   colour = dat$Bulk_density)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_Bulk_density.png", width = 10, height = 2.25)

# # Find the smallest nonzero value in the dataset
# small_const <- min(dat$Magnesium_mg_l[dat$Magnesium_mg_l > 0]) * 0.01  # 1% of the smallest value
# 
# # Add this small constant to avoid zeros
# dat$Magnesium_mg_l_adj <- dat$Magnesium_mg_l + small_const

# Fit the GLMM model
glm_model <- glmer(Bulk_density ~ Treatment + (1 | Year) + (1 | Block), 
                   data = dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_Bulk_density.png", 
       width = 10, height = 3.5)







# ~ SOC ####

names(dat)

r_dat <- filter(.data = dat, dat$Year != 2024)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  filter(dat$Year != 2024) %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(SOC_percent, na.rm = TRUE),
    sd = sd(SOC_percent, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = r_dat, 
                   variable = r_dat$SOC_percent, 
                   colour = r_dat$SOC_percent)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_SOC_percent.png", width = 10, height = 2.25)

# Find the smallest nonzero value in the dataset
small_const <- min(r_dat$SOC_percent[r_dat$SOC_percent > 0]) * 0.01  # 1% of the smallest value

# Add this small constant to avoid zeros
r_dat$SOC_percent_adj <- r_dat$SOC_percent + small_const

# Fit the GLMM model
glm_model <- glmer(SOC_percent_adj ~ Treatment + (1 | Year) + (1 | Block), 
                   data = r_dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_SOC_percent.png", 
       width = 10, height = 3.5)



# ~ N ####

names(dat)

r_dat <- filter(.data = dat, dat$Year != 2024)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  filter(dat$Year != 2024) %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(N_percent, na.rm = TRUE),
    sd = sd(N_percent, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = r_dat, 
                   variable = r_dat$N_percent, 
                   colour = r_dat$N_percent)

ggsave(filename = "sym_link_soil_chemistry/plots/distributions/dist_N_percent.png", width = 10, height = 2.25)

# # Find the smallest nonzero value in the dataset
# small_const <- min(r_dat$N_percent[r_dat$N_percent > 0]) * 0.01  # 1% of the smallest value
# 
# # Add this small constant to avoid zeros
# r_dat$N_percent_adj <- r_dat$N_percent + small_const

# Fit the GLMM model
glm_model <- glmer(N_percent ~ Treatment + (1 | Year) + (1 | Block), 
                   data = r_dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_chemistry/plots/model_diagnostics/diag_N_percent.png", 
       width = 10, height = 3.5)












