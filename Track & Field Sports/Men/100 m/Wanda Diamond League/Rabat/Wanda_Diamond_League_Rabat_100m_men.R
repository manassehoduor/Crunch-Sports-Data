rm(list = ls())

# load libraries
pacman::p_load(tidyverse, showtext, ggtext, ggsvg, glue, hms)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: worldathletics.org | #WandaDiamondLeague #Rabat")

# Extract 'run' SVG
run_url <- "https://www.svgrepo.com/download/431236/run.svg"
run_url2 <- "https://www.svgrepo.com/download/494384/run.svg"

svg_txt <- paste(readLines(run_url), collapse = "\n")
svg_txt2 <- paste(readLines(run_url2), collapse = "\n")

grid::grid.draw( svg_to_rasterGrob(svg_txt))
grid::grid.draw( svg_to_rasterGrob(svg_txt2))

# load data
men_100m <- data.frame(
  Lane = c("Lane 1", "Lane 2", "Lane 3", "Lane 4", "Lane 5", "Lane 6", "Lane 7", "Lane 8", "Lane 9", "WORLD RECORD"),
  Athlete = c("AZU Jeremiah", "BLAKE Yohan", "BROMELL Trayvon", "FALL Mouhamadou", "KERLEY Fred", "MACHMOUR Chakir", "OMANYALA Ferdinand", "SIMBINE Akani", "TEBOGO Letsile", "USAIN BOLT"),
  Nationality = c("GBR", "JAM", "USA", "FRA", "USA", "MAR", "KEN", "RSA", "BOT", "Jamaica"),
  Time_PB = c(10.13, 9.69, 9.76, 10.04, 9.76, 10.23, 9.77, 9.84, 9.91, 9.58),
  Time_SB = c(NA, NA, NA, 10.06, 9.88, NA, 9.84, 9.92, NA, 9.63)
) 

# data wrangle
men_100m <- men_100m |>
  mutate(Nationality = toupper(Nationality)) |>
  arrange(Lane)

# Plot
men_100m |> 
  ggplot(aes(x = Time_PB, y = Lane)) +
  geom_point_svg(aes(Time_PB), svg = svg_txt, size = 20, vjust = 1.3) +
  geom_point_svg(aes(Time_SB), svg = svg_txt2, size = 20, vjust = 1.3) +
  geom_text(data = men_100m|>
              filter(Athlete == "USAIN BOLT"), aes(Time_PB, label = glue("{Athlete}")),  
            family = 'Roboto Condensed', size = 7, fontface = "bold",
            colour = "black", lineheight = 0.8, nudge_x = 0.05, vjust = 6.7) +
  geom_text(aes(Time_PB, label = glue("{Nationality}")),  
            family = 'Roboto Condensed', size = 7,
            colour = "black", lineheight = 0.8, nudge_x = 0.05, vjust = 4) +
  geom_text(aes(Time_PB, label = glue("{Time_PB}")),  
            family = 'Roboto Condensed', size = 6, fontface = "bold",
            colour = "#190439", lineheight = 0.8, nudge_x = 0.02, vjust = 1.3) +
  geom_text(aes(Time_SB, label = glue("{Time_SB}")),  
            family = 'Roboto Condensed', size = 6, fontface = "bold",
            colour = "#120ba4", lineheight = 0.8, nudge_x = 0.01, vjust = 1.3) +
  geom_vline(xintercept = 9.58, linetype = "longdash", size = 2, color = "red") +
  scale_x_continuous(breaks = seq(9.58, 10.40, by = 0.05), expand = c(0.05, 0.05)) +
  scale_y_discrete(labels = c("AZU Jeremiah", "BLAKE Yohan", "BROMELL Trayvon", "FALL Mouhamadou", "KERLEY Fred", "MACHMOUR Chakir", "OMANYALA Ferdinand", "SIMBINE Akani", "TEBOGO Letsile", "WORLD RECORD")) +
  scale_x_reverse() +
  labs(
    x = "Sprinters Winning Time",
    y = "",
    title = "Wanda Diamond League in RABAT",
    subtitle = "100m Men",
    caption = cap
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Roboto Condensed'),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 25, colour = "#000000", vjust = 3),
    axis.title = element_text(size = 30, face = "bold"),
    plot.title = element_markdown(face = "bold", hjust = 0.5, size = 45, colour = '#291a98'),
    plot.subtitle = element_markdown(hjust = 0.5, size = 30, color = "black"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(colour = '#043933'),
    panel.grid.major.y = element_line(colour = '#043933'),
    plot.caption = element_markdown(colour = '#120ba4', hjust = 0.5, size = 20,
                                    family = 'Rosario', margin = margin(t = 10)),
    plot.margin = margin(b = 20, t = 40, r = 5, l = 20)
  ) +
  annotate(
    "text", y = "WORLD RECORD", x = 9.58,
    label = "Personal Best", vjust = -2, hjust = -0.1, colour = "#190439", fontface = "bold",
    family = 'Rosario', size = 7.5
  ) +
  annotate(
    "text", y = "WORLD RECORD", x = 9.63, label = "Season Best",
    vjust = -2, hjust = 0.5, colour = "#120ba4", fontface = "bold", family = 'Rosario', size = 8
  )

# Save plot
ggsave("Rabat_100m_men.png", width = 21, height = 17, bg = "#fffac7")


