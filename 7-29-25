library(purrr)
library(ggplot2)
library(forcats)
library(stringr)
library(dplyr)
library(camcorder)
library(ggtext)
library(tidytuesdayR)

camcorder::gg_record()

tt29 <- tt_load_gh("2025-07-29")

ttnew <- tt_download(tt29)

tt_mov <- ttnew$movies
tt_show <- ttnew$shows


tt_mov_cln <- 
  tt_mov |> 
  select(!source:report) |> 
  group_by(views) |> 
  arrange() |> 
  ungroup() |> 
  filter(!is.na(release_date)) |> 
  mutate(
    years_watched = hours_viewed/8760,
    centuries = years_watched/100,
    millenium = years_watched/1000
  ) |> 
  filter(years_watched > 8000, available_globally == "Yes") |> 
  distinct(title, .keep_all = TRUE) |> 
  arrange(desc(years_watched)) |> 
  mutate(rank = rev(row_number()))
  
tt_mov_cln |> 
  ggplot(aes(release_date, years_watched)) +
  geom_line() +
  geom_point() +
  geom_richtext(
    x = as.Date("2025-01-17"),
    y = 39000,
    label = "**Back in Action**<br>*Rotten Tomatoes*: 30%<br>*IMDb*: 5.9 \U2605",
    label.r = unit(.6,"lines")
    ) +
  geom_richtext(
    x = as.Date("2024-06-21"),
    y = 5013.699,
    label.r = unit(.6,"lines"),
    label = "**Trigger Warning**<br>*Rotten Tomatoes*: 20%<br>*IMDb*: 4.6 \U2605"
  ) + 
  theme_minimal() +
  labs(
    y = "Years Watched\n",
    x = "\nMovie Release Date",
    title = "What Are You Watching?",
    subtitle = str_wrap("Total viewing time (in years) for globally released Netflix movies between Jan - Jun 2025. Dashed line (--) represents mean years viewed (~13,713 Years)"),
    caption = "Source: What We Watched: A Netflix Engagement Report 2025 Jan-Jun\nVisual: Dbrock16"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 25, face = "bold", family = "Montserrat"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(face = "italic", hjust = 0),
    panel.grid.major = element_blank(),
    plot.background = element_rect("#f8efe6"),
    axis.text = element_text(color = "black", family = "Fira Sans")
  ) +
  scale_y_continuous(
    breaks = seq(2000, 40000, by = 5000),
    labels = scales::comma,
    limits = c(2000,45000),
    sec.axis = dup_axis()
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = expansion(add = c(20, 20))
  ) +
  geom_hline(
    yintercept = mean(tt_mov_cln$years_watched, na.rm = TRUE),
    linetype = "dashed",
    color = "grey60"
  )
