library(httr)
library(tidyverse)

source("env.R")

# Get data from API
url <- "https://visual-crossing-weather.p.rapidapi.com/history"

queryString <- list(
  startDateTime = "2023-12-09T00:00:00",
  aggregateHours = "1",
  location = "Santiago, CL",
  endDateTime = "2023-12-10T23:59:00",
  unitGroup = "metric",
  contentType = "csv",
  shortColumnNames = "0"
)

response <- VERB("GET", url,
  query = queryString,
  add_headers(
    "X-RapidAPI-Key" = rapidapi_key,
    "X-RapidAPI-Host" = "visual-crossing-weather.p.rapidapi.com"
  ),
  content_type("application/octet-stream")
) |>
  content("text") |>
  read_csv()

# Plot data
weather_data <- response |>
  janitor::clean_names() |>
  mutate(
    date_time = mdy_hms(date_time),
    day = weekdays(date_time),
    hour_minute = format(date_time, "%H:%M")
  )

weather_data |>
  ggplot(aes(x = hour_minute, y = temperature, group = day, color = day)) +
  geom_line(
    linewidth = 1,
    alpha = ifelse(weather_data$day == "domingo" | weather_data$day == "sábado", 0.8, 1)
  ) +
  scale_color_manual(values = c(
    "domingo" = "#e63946",
    "sábado" = "#adb5bd"
  )) +
  labs(
    title = "Temperatura en Santiago, CL",
    subtitle = "9 y 10 de diciembre de 2023",
    x = "Hora del día",
    y = "Temperatura (°C)"
  ) +
  theme_minimal() +
  scale_x_discrete() +
  scale_y_continuous(
    breaks = seq(0, max(weather_data$temperature), 10),
    limits = c(0, max(weather_data$temperature))
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  guides(color = guide_legend(title = NULL))

# Save plot
ggsave("plot.jpg", width = 10, height = 6, dpi = 300)
