## Soil nutrient plots 
## Joe Collins 
## 2024-06-24


## 02 DATA ####


## 02.1 load packages ####

source(file = "scripts/01_packages.R")


## 02.2 load data ####

## NRM data from Y2 and Y3 ####

# Specify the folder path
folder_path <- "data/NRM_txt/"

# List files (ensure the paths are correct)
file_list <- list.files(folder_path, pattern = "\\.TXT$", full.names = TRUE)

y2y3_dat <- data.frame()

for (i in file_list) {
  
  dat <- read.delim(file = i, header = TRUE, sep = ",", skip = 1)
  
  dat <- dat[2:nrow(dat),]
  
  dat <- dat[,1:8]
  
  y2y3_dat <- rbind(y2y3_dat, dat)
}

glimpse(y2y3_dat)

# Load the required package
library(dplyr)
library(stringr)

# Extract 'Year' and 'Sample_number' from the "Sample.1" column
y2y3_dat <- y2y3_dat %>%
  mutate(
    Year = str_extract(SAMPLE.1, "Y[2-3]"), # Extract the year (e.g., Y2, Y3)
    Sample_number = str_extract(SAMPLE.1, "\\d+$") # Extract the trailing number
  )

# Convert to a cleaner format (optional)
y2y3_dat <- y2y3_dat %>%
  mutate(
    Year = str_replace(Year, "Y", ""), # Remove "Y" from the Year column (optional)
    Sample_number = as.numeric(Sample_number) # Convert Sample_number to numeric
  )


y2_dat <- filter(.data = y2y3_dat, Year == "2")
y3_dat <- filter(.data = y2y3_dat, Year == "3")

write.csv(x = y2_dat, file = "data/processed/y2_nrm_processed.csv")
write.csv(x = y3_dat, file = "data/processed/y3_nrm_processed.csv")



### Full data set ####

dat <- read_excel(path = "data/processed/soil_data.xlsx")


## 02.3 factors ####


dat$Year <- as.factor(dat$Year)
dat$Treatment <- as.factor(dat$Treatment)
dat$Block <- as.factor(dat$Block)


dat$Treatment <- factor(dat$Treatment, 
                           levels = c("Baseline","Conventional", "Conservation"))


## 02.4 SUMMARY STATS ####


### P ####

# Calculates mean, sd, se and IC - block
p_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(Phosphorus_mg_l),
    sd = sd(Phosphorus_mg_l)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



### K ####

# Calculates mean, sd, se and IC - block
k_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(Potassium_mg_l),
    sd = sd(Potassium_mg_l)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


### Mg ####

# Calculates mean, sd, se and IC - block
mg_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(Magnesium_mg_l),
    sd = sd(Magnesium_mg_l)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))




### pH ####

# Calculates mean, sd, se and IC - block
ph_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(pH),
    sd = sd(pH)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



### soc ####

# Calculates mean, sd, se and IC - block
soc_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(SOC_percent),
    sd = sd(SOC_percent)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


### N ####

# Calculates mean, sd, se and IC - block
n_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(N_percent),
    sd = sd(N_percent)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


### BD ####

# Calculates mean, sd, se and IC - block
bd_sum <- dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n=n(),
    mean = mean(`Bulk_density`),
    sd = sd(`Bulk_density`)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))















