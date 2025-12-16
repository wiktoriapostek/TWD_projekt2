library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(janitor)
library(zoo)
library(tidyr)

sleep <- read_excel("sleep_wiktoria.xlsx", skip = 1) %>%
  clean_names()

sleep2 <- sleep %>%
  mutate(
    start_dt = ymd_hms(com_samsung_health_sleep_start_time, tz = "Europe/Warsaw"),
    end_dt = ymd_hms(com_samsung_health_sleep_end_time, tz = "Europe/Warsaw"),
    date = as.Date(start_dt),
    sleep_hours = as.numeric(difftime(end_dt, start_dt, units = "hours"))
  ) %>%
  filter(!is.na(date), !is.na(sleep_hours)) %>%
  filter(date >= as.Date("2025-11-01"))


sleep_daily <- sleep2 %>%
  group_by(date) %>%
  summarise(sleep_hours = mean(sleep_hours, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)


sleep_daily2 <- sleep_daily %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%  
  arrange(date) %>%
  mutate(
    sleep_hours_filled = na.approx(sleep_hours, na.rm = FALSE)
  )

ggplot(sleep_daily2, aes(x = date, y = sleep_hours_filled)) +
  geom_col() +
  labs(
    title = "Czas snu (godziny) — wykres słupkowy",
    x = "Data",
    y = "Sen (h)"
  ) +
  theme_minimal()



# -------------
library(dplyr)
library(lubridate)
library(ggplot2)

bedtime_df <- sleep2 %>%
  mutate(
    bedtime_hour = hour(start_dt) + minute(start_dt)/60,
    bedtime_shift = ifelse(bedtime_hour < 4, bedtime_hour + 24, bedtime_hour) # 0-3 -> 24-27
  )

ggplot(bedtime_df, aes(x = date, y = bedtime_shift)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(
    limits = c(20, 27),
    breaks = 20:27,
    labels = c("20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00")
  ) +
  labs(
    title = "Godzina zaśnięcia w czasie (20:00–03:00)",
    x = "Data",
    y = "Godzina zaśnięcia"
  ) +
  theme_minimal()



