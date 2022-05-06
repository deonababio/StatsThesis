Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

library(nbastatR)
library(ggplot2)
library(dplyr)

# Create visualization
gamedata <- game_logs(seasons = 2020, nest_data = TRUE)

#next time we meet, talk about what we can do spatial data on (rule)
#What is the effect on the number of threes given the 3second rule. What is the rate of 3s' and 2s'
#do the graph in per game level, also do a heatmap, could do before and after

#casual heat map to understand rules
#find shot data

#dataset %>% groupby(A,B) %>% summarize(n_shots = n()) create ggdff
