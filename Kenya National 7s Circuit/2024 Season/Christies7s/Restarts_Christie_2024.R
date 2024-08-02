# Load necessary libraries
pacman::p_load(tidyverse, readxl, ggchicklet, glue, forcats, scales, ggtext, showtext)

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

#writexl::write_xlsx(team_stats, "teamstats.xlsx")

# Reorder
kickoff_data <- team_stats |>
  select(Team, `Contestable Restarts Regained (%)`, `Contestable Receipts Retained (%)`) |>
  mutate(across(c(`Contestable Restarts Regained (%)`, `Contestable Receipts Retained (%)`), as.numeric)) |>
  arrange(`Contestable Receipts Retained (%)`) |>
  mutate(Team = factor(Team, levels = unique(Team)))  # Set factor levels after sorting

# Pivot longer
data_long <- kickoff_data |>
  pivot_longer(cols = c(`Contestable Restarts Regained (%)`, `Contestable Receipts Retained (%)`),
               names_to = "Set Piece",
               values_to = "Value")

# Center Pos for team names
data_label_pos <- data_long |>
  filter(`Set Piece` == "Contestable Receipts Retained (%)") |>
  mutate(label_pos = 0)  # Keep the label position centered

# Colors
bg <- "black"
bim <- "#0F0E0E"

# Text info
subtitle <- "Oilers have a contrasting profile with the lowest contestable restart regaining rate at 8% but the highest retention rate at 75%, <br>
indicating they focus on maintaining possession rather than contesting restarts."

# Plot
ggplot(data_long, aes(x = Team, y = ifelse(`Set Piece` == "Contestable Restarts Regained (%)", Value, -Value), fill = `Set Piece`)) +
  geom_chicklet(aes(color = `Set Piece`), alpha = 8, colour = bg, radius = grid::unit(8, "pt"), width = 0.5) +
  geom_text(aes(label = paste0(abs(Value), "%")), position = position_stack(vjust = 0.5), size = 4, family = "Roboto Condensed") +
  geom_text(data = data_label_pos, aes(x = Team, y = label_pos, label = Team), family = "Ubuntu Condensed",
            size = 4, hjust = 0.5, vjust = -1.8, colour = "#FDFFD2", fontface = "italic", lineheight = 0.3) + 
  scale_fill_manual(values = c("#f0d35b", "#B5CFB7")) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), expand = expansion(mult = c(0.5, 0.5))) + 
  coord_flip() +
  scale_alpha_identity() +
  theme_void() +
  labs(
    title = "2024 SPORTPESA CHRISTIE 7s <br> SET PIECE: KICK-OFF RESTARTS AND RECEIPTS",
    x = "Team", y = "Percentage",
    #subtitle = subtitle,
    caption = cap) +
  guides(fill = guide_legend(title = "Kickoff Set Piece", ticks.linewidth=2, family = "Raleway",
                             barwidth = 3, barheight = 0.4, title.position = "top")) +
  theme(legend.position = "top",
        plot.background = element_rect(fill = bim, colour = bim),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "white"),
        axis.text = element_text(color = "white", size = 15, family = "Roboto Condensed"),
        legend.text = element_text(size = 11, color = "white", family = "Roboto Condensed"),
        legend.title = element_text(face = "bold", family = "Roboto Condensed", size = 15, color = "#FFFDE3", hjust = 0.5),
        strip.text.x = element_text(face = "bold", family = "Roboto Condensed", size = 25, colour = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 20, color = "#F9E400", family = "Aladin",
                                      lineheight = 0.8, face = "bold", margin = margin(b=5, t=5, r=5, l=5)),
        plot.subtitle = element_markdown(hjust = 0.5, size = 15, color = "#80B3FF", family = "Ubuntu Condensed",
                                         lineheight = 1, margin = margin(b=20, t=5, r=0, l=0)),
        plot.caption = element_markdown(hjust = 0.5, margin = margin(t =20, b=20), family = "Rosario",
                                        size = 12, color = "white", lineheight = 1.2),
        plot.caption.position = "plot",
        plot.margin = margin(b = 10, t = 20, r = 20, l = 20))


ggsave("Restarts_Christie_2024_1.png", height = 11, width = 12)
