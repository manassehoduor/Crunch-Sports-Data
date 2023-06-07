
# load libraries
pacman::p_load(tidyverse, viridis, hrbrthemes, glue, ggtext, showtext, extrafont, png)

# Import fonts
font_add_google(name = "Raleway")
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'>  Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Data Source: world.rugby | #Kenya7s")

# create data = men’s Series 2023
k7s_points <- data.frame(
  Series = c("HONG KONG",	"DUBAI", "CAPE TOWN",	"HAMILTON",	"SYDNEY",	"LOS ANGELES", "VANCOUVER", "HONG KONG'",	"SINGAPORE"),
  Points = c(1, 5, 3, 7, 5, 1, 7, 1, 7)
)

k7s_points_df <- gather(k7s_points, key = "Series", value = "Points")

# Calculate the total points
tot_points <- sum(k7s_points_df$Points)

# Kenya flag
kenya_flag <- png::readPNG("C:/Users/ADMIN/Documents/R/#K7s/Flag_of_Kenya.png")

# radial plot
ggplot(k7s_points_df, aes(x = fct_inorder(Series), y = Points, fill = Series)) +
  geom_col(width = 0.5, color = "white", show.legend = FALSE, position = "stack") +
  geom_text(aes(x = Series, y = 0, label = glue("{Series} = {Points}")), 
            hjust = 1.2, size = 3, colour = "#e9ef03", family = "Roboto Condensed") +
  annotate("text", x = 0, y = 0, label = tot_points, size = 14, 
           family = "Roboto Condensed", vjust = 1) +
  scale_y_continuous(limits = c(0, 29), breaks = 1:22, expand = c(0, 0)) +
  coord_polar(theta = "y") +
  scale_fill_viridis(discrete = T) +
  theme_modern_rc() +
  xlab(NULL) +
  ylab(NULL) + 
  theme(
    text = element_text(family = "Roboto Condensed", colour = "white"),
    plot.title = element_markdown(colour = "white",face = "bold",
                                  size = 20, hjust = 0.5, family = "Raleway"),
    plot.subtitle = element_markdown(colour = "white",size = 15,
                                     hjust = 0, family = "Raleway"),
    plot.caption = element_markdown(colour = 'white', hjust = 0.5, size = 10,
                                    family = 'Rosario', margin = margin(t = 10, b=10)),
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank()
    ) +
  labs(
    y = "",
    x = "",
    caption = cap,
    subtitle = "Season 2023",
    title = "<br>KENYA 7S SERIES POINTS") 

ggsave("k7s.png", height=8, width=8)

