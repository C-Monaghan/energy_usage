rm(list = ls())

# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Theme ------------------------------------------------------------------------
palette_electric <- list(
  line = "#009FDA",
  accent = "#F7CA77",
  bg = "#F9FAFB",
  text = "#2C3E50"
)

theme_electric <- function() {
  theme_minimal(base_family = "sans", base_size = 13) +
    theme(
      plot.background = element_rect(fill = palette_electric$bg, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(
        size = rel(1.25),
        face = "bold",
        colour = palette_electric$text
      ),
      axis.text = element_text(colour = palette_electric$text),
      axis.title = element_text(colour = palette_electric$text),
      panel.border = element_rect(colour = "grey85", fill = NA),
      plot.margin = margin(10, 15, 10, 10)
    )
}

# Text labels ------------------------------------------------------------------
annotations <- tribble(
  ~datetime                         , ~y   , ~label                                                               , ~xend                             , ~yend ,
  as.POSIXct("2025-08-17 12:00:00") , 1500 , "<b style='color:#E85D04;'>✈️ Me and Bebe<br>went to Durham</b>" , as.POSIXct("2025-08-17 12:00:00") ,   500 ,
  as.POSIXct("2025-09-11 00:00:00") , 4500 , "<b style='color:#2E86AB;'>🛁 Bebe's bath arrived</b>"             , as.POSIXct("2025-09-11 00:00:00") ,  2000 ,
)

# Data -------------------------------------------------------------------------
data_raw <- read.csv(here::here("data/raw-data.csv")) |> janitor::clean_names()

# Tidying data -----------------------------------------------------------------
data <- data_raw |>
  select(-c(mprn, meter_serial_number)) |>
  tidyr::pivot_longer(cols = !date, names_to = "time", values_to = "usage") |>
  mutate(
    time = stringr::str_remove(time, "x"),
    time = stringr::str_replace(time, "_", ":"),
    datetime = as.POSIXct(paste(date, time), format = "%d/%m/%Y %H:%M"),
    month = lubridate::month(datetime, label = TRUE),
    usage = usage * 1000,
    cost = (usage * 0.3122) / 1000, # Fixed cost
    cost = cost - (cost * 0.28), # 28% discount
    cost = cost + (cost * 0.6303), # Standing charge
    cost = cost + (cost * 0.09) # VAT
  ) |>
  select(datetime, month, usage, cost)

# Full time series plot
fig_1 <- data |>
  ggplot(aes(x = datetime, y = usage)) +
  geom_line(colour = palette_electric$line, linewidth = 0.4, alpha = 0.7) +
  geom_curve(
    data = annotations,
    aes(x = datetime, y = y, xend = xend, yend = yend),
    curvature = -0.25,
    arrow = arrow(length = unit(0.02, "npc")),
    linewidth = 0.9,
    colour = palette_electric$accent,
    alpha = 0.9
  ) +
  ggtext::geom_richtext(
    data = annotations,
    aes(x = datetime, y = y, label = label),
    fill = "white",
    label.colour = NA,
    label.padding = unit(c(0.2, 0.4, 0.2, 0.4), "lines"),
    size = rel(2.5),
    family = "sans"
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Half hourly energy usage",
    x = NULL,
    y = "Total energy consumption (watts)"
  ) +
  theme_electric()

# Monthly time series plot
fig_2 <- data |>
  group_by(month) |>
  summarise(total_cost = sum(cost)) |>
  ggplot(aes(x = month, y = total_cost)) +
  geom_col(fill = palette_electric$line, colour = "black", alpha = 0.8) +
  geom_text(
    aes(label = paste0("€", round(total_cost, 0))),
    vjust = -0.3,
    size = 3.5,
    colour = palette_electric$text
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "€", suffix = ""),
    expand = expansion(mult = 0.1)
  ) +
  labs(
    title = "Total energy cost (€)",
    x = NULL,
    y = "Approximate total energy cost"
  ) +
  theme_electric()

usage <- patchwork::wrap_plots(fig_1, fig_2)

# Exporting
cowplot::save_plot(
  filename = here::here("results/energy_usage.png"),
  plot = usage,
  base_aspect_ratio = 3
)
