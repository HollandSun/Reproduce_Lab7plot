library(tidyverse)
library(zoo)

kansas_daily <- read_csv("kansas_usafacts_jul05_aug03_2020.csv", show_col_types = FALSE)

grouped <- kansas_daily %>%
  filter(!is.na(population), !is.na(daily_new_cases)) %>%
  group_by(mask_mandate, date) %>%
  summarise(
    total_new = sum(daily_new_cases, na.rm = TRUE),
    total_pop = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(per_100k = total_new / total_pop * 100000) %>%
  arrange(mask_mandate, date) %>%
  group_by(mask_mandate) %>%
  mutate(rolling_avg = rollmean(per_100k, k = 7, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(date >= as.Date("2020-07-12"), !is.na(rolling_avg))

mask_df   <- grouped %>% filter(mask_mandate == "Mask")
nomask_df <- grouped %>% filter(mask_mandate == "No Mask")

offset <- 11

p1 <- ggplot() +
  geom_line(data = mask_df,
            aes(x = date, y = rolling_avg),
            color = "darkorange", linewidth = 1.2) +
  geom_line(data = nomask_df,
            aes(x = date, y = rolling_avg + offset),
            color = "steelblue", linewidth = 1.2) +
  scale_y_continuous(
    name = "Mask",
    limits = c(15, 26),
    breaks = seq(15, 25, by = 2),
    sec.axis = sec_axis(~ . - offset,
                        name = "No Mask",
                        breaks = seq(4, 14, by = 2))
  ) +
  scale_x_date(date_breaks = "1 day", date_labels = "%m/%d/%Y") +
  labs(
    title = "Kansas COVID-19 7-Day Rolling Average of Daily Cases/Per 100K Population",
    subtitle = "Mask Counties Vs. No-Mask Mandate Counties (MISLEADING: Dual Y-Axis)",
    caption = "Source: Kansas Department of Health and Environment (via USAFacts)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left  = element_text(color = "darkorange", face = "bold", size = 12),
    axis.text.y.left   = element_text(color = "darkorange"),
    axis.title.y.right = element_text(color = "steelblue", face = "bold", size = 12),
    axis.text.y.right  = element_text(color = "steelblue"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    plot.title         = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle      = element_text(hjust = 0.5, size = 10),
    panel.grid.minor   = element_blank()
  )

print(p1)
ggsave("misleading_plot.png", p1, width = 18, height = 9, dpi = 800)