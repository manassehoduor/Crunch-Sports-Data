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
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: worldathletics.org | #KipKeinoClassic")

# Extract 'run' SVG
run_url <- "https://www.svgrepo.com/download/431236/run.svg"
run_url2 <- "https://www.svgrepo.com/download/494384/run.svg"

svg_txt <- paste(readLines(run_url), collapse = "\n")
svg_txt2 <- paste(readLines(run_url2), collapse = "\n")

grid::grid.draw( svg_to_rasterGrob(svg_txt))
grid::grid.draw( svg_to_rasterGrob(svg_txt2))

# load data
men_100m <- data.frame(
  Lane = c("Lane 1", "Lane 2", "Lane 3", "Lane 4", "Lane 5", "Lane 6", "Lane 7", "Lane 8", "WORLD RECORD"),
  Athlete = c("Pablo Mateo", "Emmanuel Eseme", "Brandon Carnes", "Kenneth Bednarek", "Ferdinand Omanyala", "Marvin Bracy-Williams", "Jerome Blake", "Steven Odhiambo", "Usain Bolt"),
  Nationality = c("France", "Cameroon", "USA", "USA", "Kenya", "USA", "Canada", "Kenya", "Jamaica"),
  Mark = c(10.18, 10.03, 10.14, 9.98, 9.84, 10.03, 10.20, NA, 9.58)
) 

# data wrangle
men_100m <- men_100m |>
  mutate(Nationality = toupper(Nationality)) |>
  arrange(Lane)

# Plot
men_100m |> 
  ggplot(aes(x = Mark, y = Lane)) +
  geom_point_svg(aes(Mark), svg = svg_txt2, size = 20, vjust = 1.3) +
  geom_text(aes(Mark, label = glue("{Nationality} \n {Athlete}")),  
            family = 'Roboto Condensed', size = 7, fontface = "bold",
            colour = "black", lineheight = 0.8, nudge_x = 0.09, vjust = 1.3) +
  geom_text(aes(Mark, label = glue("{Mark}")),  
            family = 'Roboto Condensed', size = 6, fontface = "bold",
            colour = "#120ba4", lineheight = 0.8, nudge_x = 0.01, vjust = 1.3) +
  geom_vline(xintercept = 9.58, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(9.58, 10.40, by = 0.05), expand = c(0.05, 0.05)) +
  scale_x_reverse() +
  labs(
    x = "Sprinters Winning Time",
    title = "KIP KEINO CLASSIC",
    subtitle = "Men's 100m<br>Wind: -0.5",
    caption = cap
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Roboto Condensed'),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 25, vjust = 3, colour = "#000000"),
    axis.title = element_text(size = 30, colour = "#150aa8"),
    plot.title = element_markdown(face = "bold", hjust = 0.5, size = 45, colour = '#000000'),
    plot.subtitle = element_markdown(hjust = 0, size = 30, color = "#000000"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(colour = '#043933'),
    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 20,
                                    family = 'Rosario', margin = margin(t = 10)),
    plot.margin = margin(b = 20, t = 20, r = 10, l = 10)
  ) +
  annotate(
    "text", y = "WORLD RECORD", x = 9.58,
    label = "World Best", vjust = -2, hjust = -0.1, colour = "#120ba4", fontface = "bold",
    family = 'Rosario', size = 8
  ) +
  annotate(
    "text", y = "Lane 8", x = 10.25, label = "Steven Odhiambo - Disqualified", 
    family = 'Rosario', size = 8, colour = "#1b5611", face = "bold", vjust = 3.5, hjust = -0.1
  )

# Save plot
ggsave("kip_keino_classic_100m_men.png", width = 21, height = 17, bg = "#dbf3fe")


