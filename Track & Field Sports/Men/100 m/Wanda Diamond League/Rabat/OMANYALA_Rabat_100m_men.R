rm(list=ls())

# load libraries
pacman::p_load(ggbump, tidyverse, ggimage, scales, extrafont, ggtext, 
               showtext, htmltools, magick, grid, png, ggpath, glue)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Passion One")
font_add_google(name = "Cabin Sketch")
font_add_google(name = "Rye")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: rabat.diamondleague.com | #DiamondLeague #RabatDL")

# create a data frame with Athlete rankings over time
Rabat_100m_men <- read.csv(file = "Results_Rabat_100m_men.csv")

# Define custom athlete colors for bump plot
athlete_colors <- c(
  "KERLEY Fred" = "#a7f3d0",
  "SIMBINE Akani" = "#a7f3d0",
  "OMANYALA Ferdinand" = "black",
  "TEBOGO Letsile" = "#a7f3d0",
  "BROMELL Trayvon" = "#a7f3d0",
  "BLAKE Yohan" = "#a7f3d0",
  "AZU Jeremiah" = "#a7f3d0",
  "FALL Mouhamadou" = "#a7f3d0",
  "MACHMOUR Chakir" = "#a7f3d0"
)

#  bump plot
Rabat_100m_men_rankings_bump_chart <- Rabat_100m_men |>
  ggplot(aes(Mark, Ranking)) +
  geom_point(aes(col = Athlete), size = 3) +
  geom_bump(aes(col = Athlete), size = 1) +
  geom_text(
    data = Rabat_100m_men |>
      filter(Athlete == "OMANYALA Ferdinand"),
    aes(label = glue("{Time}s"), col = Athlete),
    vjust = 1.8, nudge_x = -1, size = 4, fontface = "bold", family = "Roboto Condensed"
  ) +
  geom_text(
    data = Rabat_100m_men |>
      filter(Mark == 10),
    aes(label = Athlete, col = Athlete),
    hjust = 1, nudge_x = -1.5, size = 4.5, fontface = "bold", family = "Ubuntu Condensed"
  ) +
  geom_text(
    data = Rabat_100m_men |>
      filter(Mark == 100),
    aes(label = Ranking, col = Athlete), hjust = 0, nudge_x = 1, size = 5, 
    fontface = "bold", family = "Roboto Condensed"
  ) +
  geom_text(
    data = Rabat_100m_men |>
      filter(Mark == 100),
    aes(label = glue("{Time}s"), col = Athlete), hjust = 0, nudge_x = 5, size = 4.5, 
    fontface = "bold", family = "Passion One"
  ) +
  annotate(
    "text",
    x = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    y = rep(0.25, 10),
    label = c("10m", "20m", "30m", "40m", "50m", "60m", "70m", "80m", "90m", "Finish"),
    hjust = 0, vjust = 1, size = 4.5, colour = "#313131",
    family = "Passion One"
  ) +
  annotate(
    "text", x = 110, y = 0.25, label = "Time", hjust = 1, vjust = 1, size = 4.5,
    fontface = "bold", colour = "black", family = "Passion One"
  ) +
  
  scale_y_reverse(position = "right", breaks = seq(16, 2, -2)) +
  scale_color_manual(values = athlete_colors) +
  coord_cartesian(xlim = c(-10, 115), ylim = c(10, 0.05), expand = F) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(family = "Cabin Sketch", colour = "#0e0e0b", size = 16, face = "bold",
                                 margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5
    ),
    plot.title = element_text(
      face = "bold", colour = "#0e0e0b", family = "Rye", size = 25, hjust = 0.5
    ),
    plot.caption = element_markdown(colour = '#362FD9', hjust = 0.5, size = 11,
                                    family = 'Rosario', margin = margin(t=5, b=5)),
    plot.margin = margin(b = 10, t = 20, r = 10, l = 20)) +
  labs(
    x = "",
    y = "",
    title = "\n Wanda Diamond League",
    subtitle = "Rabat 2023: OMANYALA Ferdinand 100m Race Analysis",
    caption = cap
  )

# Save plot
ggsave("Omanyala_Rabat_100m_men_rankings_2023.png", width = 11, height = 9, bg = "white")

