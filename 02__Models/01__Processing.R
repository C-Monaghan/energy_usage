# Data processing
rm(list = ls())

library(dplyr)
library(lubridate) # For working with dates

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/01__Raw_Data/"

usage_raw <- read.csv(file.path(path_data, "01__Data_Raw.csv"))

# Processing -------------------------------------------------------------------
usage <- usage_raw %>%
  rename_with(~gsub("X", "", .), matches("^X\\d{2}\\.\\d{2}$")) %>% # Renaming columns
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
         Day = wday(Date, label = TRUE),
         Week = week(Date),
         Month = month(Date, label = TRUE)) %>% # Extracting Day & Weeks & Months
  select(Date, Day, Week, Month, everything(), -c(MPRN, Meter.Serial.Number))

# Pivoting to long format
usage <- usage %>%
  tidyr::pivot_longer(cols = !c(Date:Month),
                      names_to = "Time",
                      values_to = "Usage")
  
# Exporting --------------------------------------------------------------------
export_path <- "./01__Data/02__Processed_Data/"

write.csv(file = file.path(export_path, "01__Usage_data.csv"), x = usage)