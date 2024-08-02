# Load necessary libraries
pacman::p_load(tidyverse, readxl, glue, forcats, scales, ggtext, showtext, ggrepel)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Aladin")
font_add_google(name = "Raleway")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #RugbyKE")

# Load data
rugby7s_christie_2024 <- read_excel("SportpPesa7s_Stats.xlsx", sheet = "Christie7s")

# Data preparation
# No. of games played by each team
games_played <- rugby7s_christie_2024 |>
  select(Team_1, Team_2) |>
  gather(key = "Type", value = "Team") |>
  group_by(Team) |>
  summarise(Games_Played = n())

# Metrics Summation for each team
team_metrics <- rugby7s_christie_2024 |>
  gather(key = "Type", value = "Team", Team_1, Team_2) |>
  gather(key = "Metric", value = "Value", -Type, -Team) |>
  mutate(Metric = gsub("_1", "", Metric),
         Metric = gsub("_2", "", Metric)) |>
  summarise(Total = sum(Value), .by = c(Team, Metric)) |>
  spread(key = Metric, value = Total) 

# Merge
team_stats <- left_join(team_metrics, games_played, by = "Team")

# Compute Metrics 
team_stats <- team_stats |>
  mutate(
    `Contestable Restarts (%)` = round((`Contestable Restarts` / Restarts)*100, 0),
    `Contestable Restarts Regained (%)` = round((`Contestable Restarts Regained` / `Contestable Restarts`)*100, 0),
    `Contestable Receipts Retained (%)` = round((`Contestable Receipts Secured` / `Contestable Restarts`)*100, 0),
    `Non-Contestable Restarts (%)` = round((`Non-Contestable Restarts` / Restarts)*100, 0),
    `Restart Error Rate %` = round((`Restart Errors` / Restarts)*100, 0),
    `Average Restart Errors per Game` = round(`Restart Errors` / `Games_Played`, 1),
    `Scrums (average per game)` = round(`Scrums` / `Games_Played`, 1),
    `Total Scrums Faced` = `Scrums` + `Opposition Scrum Won`,
    `Opposition Scrum Won (%)` = round((`Opposition Scrum Won` / `Total Scrums Faced`)*100, 0),
    `Scrums Retained (%)` = round((`Scrums Retained` / `Scrums`)*100, 0),
    `Lineouts (average per game)` = round(`Lineouts` / `Games_Played`,1),
    `Lineouts Retained (%)` = round((`Lineouts Retained` / `Lineouts`)*100, 0),
    `Total Lineouts Faced` = `Lineouts` + `Opposition Lineouts Won`,
    `Opposition Lineouts Won (%)` = round((`Opposition Lineouts Won` / `Total Lineouts Faced`)*100, 0)
  ) |>
  select(Team, `Games_Played`, everything())

team_stats <- team_stats |>
  select(-Row_ID)

# Calculate overall averages
scrum_avg <- mean(team_stats$`Scrums (average per game)`, na.rm = TRUE)
lineout_avg <- mean(team_stats$`Lineouts (average per game)`, na.rm = TRUE)

## Plot 2
ggplot(team_stats, aes(x = `Scrums (average per game)`, y = `Lineouts (average per game)`)) +
  geom_point(
    aes(color = Team), size = 2, alpha = 0.9) +
  geom_text_repel(
    aes(label = Team),
    family = "Roboto Condensed", size = 3, min.segment.length = 0, seed = 42, box.padding = 1, max.overlaps = Inf,
    arrow = arrow(length = unit(0.008, "npc")), nudge_x = .05, nudge_y = .1, color = "#FFFDE3") +
  geom_vline(xintercept = scrum_avg, linetype = "dashed", color = "white") +  # Vertical line for scrum average
  geom_hline(yintercept = lineout_avg, linetype = "dashed", color = "white") +  # Horizontal line for lineout average
  annotate("text", x = scrum_avg, y = max(team_stats$`Lineouts (average per game)`) + 0.2, 
           label = "Average Scrums", color = "#836FFF", family = "Ubuntu Condensed", vjust = -0.5, hjust = -0.1) +  # Label for scrum average
  annotate("text", x = min(team_stats$`Scrums (average per game)`) - 0.1, y = lineout_avg, 
           label = "Average Lineouts", color = "#836FFF", family = "Ubuntu Condensed", vjust = -0.5, hjust = -0.1) +  # Label for lineout average
  
  theme_void() +
  guides(fill = "none") +
  labs(
    title = "2024 SPORTPESA CHRISTIE 7s <br> SET PIECES: Scrums and Lineouts Averages Per Game",
    x = "Scrum Average",
    y = "Lineout Average",
    subtitle = "Capitalization on opponents' errors",
    caption = cap) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bim, colour = bim),
        #axis.title.x = element_markdown(color = "white"),
        axis.text.y = element_text(size = 10, color = "white"),
        axis.text.x = element_text(size = 10, color = "white"),
        axis.text = element_text(color = "white", size = 15, family = "Roboto Condensed"),
        strip.text.x = element_text(face = "bold", family = "Roboto Condensed", size = 12, colour = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 18, color = "#F9E400", family = "Aladin",
                                      lineheight = 0.8, face = "bold", margin = margin(b=10, t=10, r=10, l=00)),
        plot.subtitle = element_markdown(hjust = 0.5, size = 14, color = "#F8FAE5", family = "Ubuntu Condensed",
                                         lineheight = 1, margin = margin(b=5, t=5, r=0, l=0)),
        plot.caption = element_markdown(hjust = 0.5, margin = margin(t =5, b=5), family = "Rosario",
                                        color = "#864AF9", lineheight = 1.2),
        plot.caption.position = "plot")

ggsave("Avg_Scrums_Lineouts_Christie_2024.png", height = 6, width = 7)
