
#Defensive 3 seconds in the key was created in 2000. 
#Having information from 1997-2003 would be good for analysis
library(readr)
season1997_2003 <- read_csv("Datasets/season1997_2003.csv")


x <- season1997_2003 %>% 
  group_by(yearSeason, nameTeam, idGame, dateGame) %>% 
  summarize(twopointfg = sum(fg2a),
            threepointfg = sum(fg3a)) %>% 
  mutate(idGame = as.character(idGame))

#x$yearSeason <- as.character(x$yearSeason)
#flip_results <- data.frame(Player = factor(column1, levels = c("Player A", "Player B")),
#             Result = column2)
g <-
ggplot(data = x) +
  geom_point(aes(x = dateGame, y = threepointfg, color = nameTeam)) +
  geom_vline(xintercept = as.Date("2001-01-01")) + 
  theme_bw() +
  facet_wrap(~nameTeam, scales = "free_x") #+ 
  #ylim(.3, .4)#+ #scale_y_continuous(0,1) #+
ggsave(filename = "outputs/threepoints.pdf", g, height = 16.5, width = 16.5)

library(estimatr)
test <- 
x %>% 
  filter(yearSeason %in% c(2000, 2001)) %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(threepointfg ~ yearSeason, data = cur_data())))



g2 <- 
ggplot(data = x) +
  geom_line(aes(x = yearSeason, y = twopointfg, group = 1)) +
  geom_vline(xintercept = 2001) + theme_bw()#+ #scale_y_continuous(0,1) #+
ggsave(filename = "outputs/twopoints.pdf", g2, height = 6.5, width = 6.5)

#the rule changes whether or not you take the shot

#group by team


#library(rjson)
# shot data for Stephen Curry
#playerID <- 201939

#shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")

# import from JSON
#shotData <- fromJSON(file = shotURL, method="C")
```