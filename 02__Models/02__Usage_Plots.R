# Data processing
rm(list = ls())

library(dplyr)
library(ggplot2)

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/02__Processed_Data/"

usage <- read.csv(file.path(path_data, "01__Usage_data.csv"))

# Converting columns to factors and time
usage <- usage %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M"),
         Month = factor(Month, levels = c("Nov", "Dec", "Jan", "Feb", "Mar",
                                          "Apr", "May", "Jun", "Jul")),
         Day = factor(Day, levels = c("Mon", "Tue", "Wed", "Thu", 
                                      "Fri", "Sat", "Sun")))

# Plotting usage over time -----------------------------------------------------
overall_usage <- ggplot(usage, aes(x = Timestamp, y = Usage)) +
  geom_line() +
  labs(title = "Energy Consumption Over Time",
       x = "Timestamp", y = "Energy Consumption") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting mean daily usage (by month) -----------------------------------------
# Calculate the mean usage for each day and month
mean_usage <- aggregate(Usage ~ Day + Month, data = usage, FUN = mean)

mean_daily <- ggplot(data = mean_usage, aes(x = Day, y = Usage, fill = Month)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Month) +
  labs(title ="Mean Daily usage (by month)", y = "Usage (Watts)") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/results"

cowplot::save_plot(file.path(export_path, "01__overall_usage.png"), plot = overall_usage,
                   base_height = 10)
cowplot::save_plot(file.path(export_path, "02__mean_daily_usage.png"), plot = mean_daily,
                   base_height = 10)
