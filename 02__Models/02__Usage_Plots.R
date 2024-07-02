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
usage %>%
  ggplot(aes(x = Timestamp, y = Usage)) +
  geom_line() +
  labs(title = "Energy Consumption Over Time",
       x = "Timestamp", y = "Energy Consumption (Watts)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  ggeasy::easy_center_title()

# Plotting mean daily usage (by month) -----------------------------------------
# Calculate the mean usage for each day and month
mean_usage <- aggregate(Usage ~ Day + Month, data = usage, FUN = mean)

mean_usage %>%
  ggplot(aes(x = Day, y = Usage, fill = Month)) +
  geom_bar(stat = "identity", colour = "black") +
  facet_wrap(~ Month) +
  labs(title ="Mean daily usage (by month)", y = "Usage (Watts)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  colorspace::scale_fill_discrete_qualitative(palette = "Dynamic")


weekly_usage <- aggregate(Usage ~ Week + Month, data = usage, FUN = mean)

# Adjust Week column to week I moved in to the week I left
weekly_usage <- weekly_usage %>%
  mutate(Week = seq(1, length(Week), by = 1))

weekly_usage %>%
  ggplot(aes(x = Week, y = Usage)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Mean Weekly Energy Usage",
       x = "Week",
       y = "Average Energy Consumption (Watts)")  +
  scale_x_continuous(breaks = seq(0, max(weekly_usage$Week), by = 5)) +  # Adjust x-axis breaks
  theme_minimal(base_size = 15) +  
  theme(
    panel.grid.major = element_line(color = "grey80"),  # Add major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  ggeasy::easy_center_title()
