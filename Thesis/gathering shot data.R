
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("nbastatR")
library(nbastatR)
#install.packages("devtools")
library(devtools)
#devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)
#install.packages("extrafont")
library(extrafont)
#install.packages("cowplot")
library(cowplot)


# Creating court and plotting
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}
# Court Dimensions & lines
width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14
# Court themes
court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray20',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray20"
  )
)
# Function to create court based on given dimensions
plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  # Final plot creation
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'gray20', color = 'gray20'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

#The above code builds the court

#paper structred in introduction, theory, design, results, conclusion
#intro - mini version of the whole paper. The instance in the ways in which institutions change data
#For and against it say, choice quotes from interviews about the topic
#Theroy: what people thoguht would happen with this rule change
#design, how do you make it a heatmap, how do you make scatterplots 
#results, actually start showing some graphs
#conclusion: recap


Sys.setenv("VROOM_CONNECTION_SIZE" = "1000000")

#The code below: gather shot chart data from the 2017 NBA season from Lebron James, 
# steph curry, and kevin durant

#change this to just get the entire team
##grab_data <- function(player, team, seasonyear) {
# Grab team names to type into teams_shots function
##nba_teams() %>% filter(isNonNBATeam == 0)
# Grab team shot data
##team_name <- teams_shots(teams = team, seasons = seasonyear)#, season_types = "Regular")
# Filter shot data for player & clean data to fit court dimensions
##player_name <- team_name %>% filter(namePlayer==player) %>% 
  ##mutate(x = as.numeric(as.character(locationX)) / 10, y = as.numeric(as.character(locationY)) / 10 + hoop_center_y)
# Horizontally flip the data
##player_name$x <- player_name$x * -1 
# Filter shots by game date
#final_durant <- durant %>% filter(dateGame == 20210615)
##player <- player_name
##}

##stephen_curry <- grab_data("Stephen Curry", "Golden State Warriors", 2017)
##kevin_durant <- grab_data("Kevin Durant", "Golden State Warriors", 2017)
##lebron_james <- grab_data("LeBron James", "Cleveland Cavaliers", 2017)

#Now do the same thing, but back when Kevin Garnett, Ray Allen, and Vince Carter played

##kevin_garnett <- grab_data("Kevin Garnett", "Minnesota Timberwolves", 2003)
##vince_carter <- grab_data("Vince Carter", "Toronto Raptors", 2003)
##ray_allen <- grab_data("Ray Allen", "Oklahoma City Thunder", 2003)

#for all seasons for all players

#player <- stephen_curry

plottingthecourt <- function(playerOrTeam) {
plot_court(court_themes$ppt, use_short_three = FALSE) +
  geom_point(
    data = playerOrTeam,
    aes(
      x = x,
      y = y,
      color = isShotMade,
      fill = isShotMade
    ),
    size = 3,
    shape = 21, #(use x and o's for made/miss)
    stroke = .5
  ) +  
  scale_x_continuous(limits = c(-27.5, 27.5)) +
  scale_y_continuous(limits = c(0, 45)) +
  theme(plot.title = element_text(hjust = .5, size = 22, family = "Comic Sans MS", face = "bold", vjust = -4),
        plot.subtitle = element_text(hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = -8),
        legend.position = c(.5, .85),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", colour = "white"),
        plot.caption = element_text(hjust = .5, size = 6, family = "Comic Sans MS", face = "bold", colour = "lightgrey", vjust = 8)) +
  #ggtitle(label = "Kevin Durant vs. Milwaukee",
  #       subtitle = "49 PTS | 17 REB | 10 AST | 4-9 3PT - 6/15/21") +
  #labs(caption = "Tutorial: @DSamangy")
  ggdraw(p1) + theme(plot.background = element_rect(fill="gray20", color = NA))}

print_chart <- function(player) {
  
  p1 <- (plot_court(court_themes$ppt, use_short_three = F) +
    geom_point(data = player, aes(x = x, y = y, color = player$isShotMade, fill = player$isShotMade), 
               size =3, shape = 21, stroke = .5) +  
     scale_x_continuous(limits = c(-27.5, 27.5)) +
    scale_y_continuous(limits = c(0, 45)) +
    theme(plot.title = element_text(hjust = .5, size = 22, family = "Comic Sans MS", face = "bold", vjust = -4),
          plot.subtitle = element_text(hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = -8),
          legend.position = c(.5, .85),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", colour = "white"),
          plot.caption = element_text(hjust = .5, size = 6, family = "Comic Sans MS", face = "bold", colour = "lightgrey", vjust = 8))) +
    
    ggdraw(p1) + theme(plot.background = element_rect(fill="gray20", color = NA))
}

stephen_curry_shot_chart <- print_chart(stephen_curry)
  #ggsave("Durant.png", height = 6, width = 6, dpi = 300)
  
#download save as a csv all of the dataset (get each dataset)
#figure out the heatmap ( split into the paint, midrange, and three points)
#time serioes for #number of players
#split up every script to only
# do a linear regression graph of graph of number of threes/twos shot, number of threes/twos made
# --- baskets in the paint 
# fix up the files to get the shot charts.
#scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
#scale_fill_manual(values = c("green2","gray20"), aesthetics = "fill", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
```