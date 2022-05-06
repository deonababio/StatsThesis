#3 Pointer Analysis of All NBA shots 

#Open file
#library(dplyr)
library(nbastatR)
#library(ggplot2)
library(lubridate)
library(estimatr)
library(tidyverse)

source("/Users/deonababio/Desktop/Thesis/Code/Simplied code/functions to filter data into shot locations.R")
source("/Users/deonababio/Desktop/Thesis/Code/Simplied code/Building Court Dimensions.R")
ALLNBASHOTS2 <- read.csv("/Users/deonababio/Desktop/Thesis/Datasets/TeambyTeam/AllNBAShotsNew.csv")

#source("/Users/deonababio/Desktop/Thesis/Code/Simplied code/gathering shot data for every team.R")
#ALLNBASHOTS2 <- read.csv("/Users/deonababio/Desktop/Thesis/AllNBAShotsNew.csv")
#ALLNBASHOTS2 <- read.csv("/Users/deonababio/Desktop/Thesis/Datasets/TeambyTeam/.AllNBAShots.csv")

#filter the dataset to only 1999,2000,2001 and 2002,2003,2004

#look into how managers felt about this rule, how does the offensive strategy change, how does the defensive strategy chang
#I am going to use a datasets of all shots taken to see if before or after the rule change 


#try to make a line graph of distance verus shots made and shots missed
#put a line for before the rule, and after the rule (can do the 2000 data and then the 2001 data)
shots_99_04 <- ALLNBASHOTS2 %>% filter(between(yearSeason,1998,2004))
#clean the data X and Y locations
shots_99_04 <- cleanfile(shots_99_04)

#rembember, shot was introduced in 

## MODULE A: first graph, number of 3 pointers taken per season in the entire NBA

MODULE_A <-
  shots_99_04 %>%
  group_by(slugSeason, nameTeam, idGame, dateGame) %>%
  summarise(number_of_threes = sum(typeShot == "3PT Field Goal"),
            number_of_threes_made = sum(typeShot == "3PT Field Goal" & isShotMade == TRUE),
            itp_attempted = sum(zoneBasic == "In The Paint (Non-RA)"),
            itp_made = sum(zoneBasic == "In The Paint (Non-RA)"& isShotMade == TRUE)) %>% 
  mutate(date = as.Date(as.character(dateGame), "%Y%m%d"))

g_close <- 
ggplot(data = MODULE_A, aes(x = date, y = itp_attempted, group =MODULE_A$slugSeason)) +
  geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
  geom_vline(xintercept  = as.Date("2001-10-01"),
             linetype = "dotted") +
  stat_smooth(method = "lm_robust") +
  facet_wrap(~nameTeam) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Number of threes taken between 1998-1999 to 2003-2004",
       y = "Number of 3's taken",
       x = "NBA Season")

ggsave("outputs/by_team_NumberofCloseShotsAttempted.pdf", plot = g_close, width = 10, height = 10)
#Next, look at total points

g_three <-
ggplot(data = MODULE_A, aes(x = date, y = number_of_threes, group = slugSeason, colour = slugSeason)) +
  geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
  geom_vline(xintercept  = as.Date("2001-10-01"),
             linetype = "dotted") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Figure 5: Number of threes taken per game between 1998-1999 to 2003-2004") +
  ylab("Number of 3's taken") + xlab("NBA Seasons 1998-1999 to 2003-2004") +
  #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
  theme(#axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 
  #scale_x_continuous(breaks=seq(1,10,by=1))

g_three

#+
  #scale_x_discrete(labels = MODULE_A$slugSeason)
  
  #theme(axis.title.x = element_blank())
  #theme(legend.position = "bottom") #+
  #scale_x_continuous("NBA Season", labels = MODULE_A$slugSeason, breaks = MODULE_A$slugSeason)
  

ggsave("outputs/figure5.pdf", plot = g_three, width = 10, height = 6)


# g_dunk <-
#   ggplot(data = dunk_shots_game,
#          aes(x = date, y = number_ofdunks, group = 1)) +
#   geom_point(alpha = 0.8, stroke = 0, col = MODULE_A$slugSeason) +
#   geom_vline(xintercept  = as.Date("2001-10-01"),
#              linetype = "dotted") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Figure 6: Number of dunks taken per game between 1998-1999 to 2003-2004") +
#   ylab("Number of 3's taken") + xlab("NBA Seasons 1998-1999 to 2003-2004") +
#   #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
#   theme(#axis.ticks.x = element_blank(),
#     axis.text.x = element_blank()) 
# #scale_x_continuous(breaks=seq(1,10,by=1))
# 
# g_dunk

g_dunk <- ggplot(data = dunk_shots_game, aes(x = date, y = number_ofdunks, group = 1)) + 
  geom_point(alpha = 0.4, stroke = 0) +
  geom_vline(xintercept  = as.Date("2001-10-01")) +
  #stat_smooth(method = "lm_robust") +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Figure 6: Number of Dunks taken per game between 1998-1999 to 2003-2004 Per team") + 
  #facet_wrap(~nameTeam) + 
  theme(strip.text.x = element_text(size = 7, colour = "black")) +
  ylab("Number of Dunks taken") + 
  xlab("NBA Season") + 
  theme_bw()

g_dunk


ggsave("outputs/Figure6.pdf", plot = g_dunk, width = 10, height = 6)







g_three_by_team <-
  ggplot(data = MODULE_A, aes(x = date, y = number_of_threes, group = slugSeason)) +
  geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
  geom_vline(xintercept  = as.Date("2001-10-01"),
             linetype = "dotted") + stat_smooth(method = "lm_robust") +
  facet_wrap(~nameTeam) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Figure 7: Number of threes taken per game between 1998-1999 to 2003-2004") +
  ylab("Number of 3's taken") + xlab("NBA Seasons 1998-1999 to 2003-2004") +
  #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
  theme(#axis.ticks.x = element_blank(),
    axis.text.x = element_blank()) 
#scale_x_continuous(breaks=seq(1,10,by=1))

g_three_by_team

ggsave("outputs/figure7.pdf", plot = g_three_by_team, width = 10, height = 10)






## MODULE B: Same as MODULE A, but facet for each time
MODULE_B <-
  shots_99_04 %>%
  group_by(nameTeam, slugSeason) %>%
  filter(typeShot == "3PT Field Goal") %>%
  summarise(number_of_threes = length(isShotAttempted))

ggplot(data = MODULE_B, aes(x = slugSeason, y = number_of_threes, group = 1)) +
  geom_line() + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_B, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Number of threes taken between 1998-1999 to 2003-2004 Per team") + 
  facet_wrap(~nameTeam) + 
  theme(strip.text.x = element_text(size = 7, colour = "black")) +
  ylab("Number of 3's taken") + xlab("NBA Season")
  



## MODULE C: Same as above, but specifycing to specific players

MODULE_C_Duncan <- shots_99_04 %>% group_by(slugSeason) %>% 
  filter(typeShot == "3PT Field Goal", namePlayer == "Tim Duncan") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

gduncline <- ggplot(data = MODULE_C_Duncan, aes(x = slugSeason, y = number_of_threes, group = 1)) +
  geom_line(size = 2, col = "grey") + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted", size = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Number of 3 Pointers taken by Tim Duncan between 1998-2004 Per Season") +
  theme(plot.title = element_text(size=7))

gduncline

ggsave("outputs/tim_duncanlinegraph.pdf", plot = gduncline, width = 4, height = 4)

#######
# MODULE_C_DuncanScatter <- shots_99_04 %>% group_by(slugSeason,idGame) %>% 
#   filter(typeShot == "3PT Field Goal", namePlayer == "Tim Duncan") %>%
#   summarise(number_of_threes = length(isShotAttempted)) %>%
#   mutate(date = as.Date(as.character(dateGame), "%Y%m%d"))
# 
# 
# gduncscatter <- 
#   ggplot(data = MODULE_C_DuncanScatter, aes(x = date, y = number_of_threes, group = MODULE_C_DuncanScatter$slugSeason)) +
#   geom_point(alpha = 0.4, stroke = 0) +
#   theme_bw() + 
#   geom_vline(xintercept  = as.Date("2001-10-01"),
#              linetype = "dotted") +
#   stat_smooth(method = "lm_robust") +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Number of 3 Pointers taken by Tim Duncan between 1998-2004 Per Season") +
#   theme(plot.title = element_text(size=7))
# 
# gduncscatter
# 
# ggsave("outputs/tim_duncanlinegraph.pdf", plot = gduncscatter, width = 10, height = 6)



# g_close <- 
#   ggplot(data = MODULE_A, aes(x = date, y = itp_attempted, group =MODULE_A$slugSeason)) +
#   geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
#   geom_vline(xintercept  = as.Date("2001-10-01"),
#              linetype = "dotted") +
#   stat_smooth(method = "lm_robust") +
#   facet_wrap(~nameTeam) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(title = "Number of threes taken between 1998-1999 to 2003-2004",
#        y = "Number of 3's taken",
#        x = "NBA Season")



MODULE_C_Carter <- shots_99_04 %>% group_by(slugSeason) %>% 
  filter(typeShot == "3PT Field Goal", namePlayer == "Vince Carter") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

# gvince <- ggplot(data = MODULE_C_Carter, aes(x = slugSeason, y = number_of_threes, group = 1)) +
#   geom_line() + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted") +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Number of 3 Pointers taken by Vince Carter between 1998-2004")
# ggsave("outputs/vince_carter.pdf", plot = gvince, width = 12, height = 12)

gvinceline <- ggplot(data = MODULE_C_Carter, aes(x = slugSeason, y = number_of_threes, group = 1)) +
  geom_line(size = 1.2, col = "red") + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted", size = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Number of 3 Pointers taken by Vince Carter between 1998-2004 Per Season") +
  theme(plot.title = element_text(size=7))

gvinceline

ggsave("outputs/vince_carterlinegraph.pdf", plot = gvinceline, width = 4, height = 4)





MODULE_C_Allen <- shots_99_04 %>% group_by(slugSeason) %>% 
  filter(typeShot == "3PT Field Goal", namePlayer == "Ray Allen") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

gAllenLine <- ggplot(data = MODULE_C_Allen, aes(x = slugSeason, y = number_of_threes, group = 1)) +
  geom_line(size = 1.2, col = "green") + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted", size = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Number of 3 Pointers taken by Ray Allen between 1998-2004 Per Season") +
  theme(plot.title = element_text(size=7))

gAllenLine


ggsave("outputs/ray_allenlinegraph.pdf", plot = gAllenLine, width = 4, height = 4)


MODULE_C_Garnett <- shots_99_04 %>% group_by(slugSeason) %>% 
  filter(typeShot == "3PT Field Goal", namePlayer == "Kevin Garnett") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

gGarnettLine <- ggplot(data = MODULE_C_Garnett, aes(x = slugSeason, y = number_of_threes, group = 1)) +
  geom_line(size = 1.2, col = "green") + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted", size = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Number of 3 Pointers taken by Kevin Garnett between 1998-2004 Per Season") +
  theme(plot.title = element_text(size=7))

gGarnettLine


ggsave("outputs/kevin_garnettlinegraph.pdf", plot = gGarnettLine, width = 4, height = 4)


##put everything together

compareplayers <- ggplot() +
  geom_line(data = MODULE_C_Duncan, aes(x = slugSeason, y = number_of_threes, group = 1, col = "Tim Duncan")) +
  geom_line(data = MODULE_C_Allen, aes(x = slugSeason, y = number_of_threes, group = 1, col = "Ray Allen")) +
  geom_line(data = MODULE_C_Carter, aes(x = slugSeason, y = number_of_threes, group = 1, col = "Vince Carter")) +
  geom_line(data = MODULE_C_Garnett, aes(x = slugSeason, y = number_of_threes, group = 1, col = "Kevin Garnett")) +
  scale_colour_discrete("Player's Name") + 
  theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted", size = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Number of 3's taken") + xlab("NBA Season") + ggtitle("Figure 7: Number of 3 Pointers taken by Tim Duncan, Ray Allen, Vince Carter, and Kevin Garnett between 1998-2004 Per Season") +
  theme(plot.title = element_text(size=10)) 

compareplayers

ggsave("outputs/compareplayerslinegraph.pdf", plot = compareplayers, width = 9, height = 7)





#MODULE D: Compare 2's and threes
MODULE_D_dunks <- shots_99_04 %>% 
  #filter(slugSeason %in% c("1999-00", "2000-01")) %>%
  #filter(grepl("dunk",typeAction))
  group_by(slugSeason,nameTeam) %>% 
  summarise(numberofcloseshots = sum(typeAction%in% c("Dunk Shot", "Layup Shot", "Driving Dunk Shot", "Running Layup Shot", "Driving Layup Shot",
                                                      "Finger Roll Shot", "Alley Oop  Dunk Shot", "Driving Finger Roll Shot", "Reverse Layup Shot", "Driving Hook Shot",
                                                      "Running Dunk Shot", "Reverse Dunk Shot", "Turnaround Finger Roll Shot", "Alley Oop Layup shot" ,
                                                      "Follow Up Dunk Shot" , "Running Slam Dunk Shot", "Putback Reverse Dunk Shot" , 
                                                      "Cutting Dunk Shot" ,"Cutting Finger Roll Layup Shot","Running Alley Oop Dunk Shot",
                                                      "Tip Dunk Shot","Running Alley Oop Layup Shot","Driving Reverse Dunk Shot" ,        
                                                      "Running Reverse Dunk Shot")))

MODULE_D_2points <- shots_99_04 %>% group_by(slugSeason) %>% filter(typeShot == "2PT Field Goal") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

MODULE_D_3points <- shots_99_04 %>% group_by(slugSeason) %>% filter(typeShot == "3PT Field Goal") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

g23 <- ggplot() +
  geom_line(data = MODULE_D_dunks, aes(x = slugSeason, y = numberofcloseshots, group = 1, color = "Dunks")) +
  geom_line(data = MODULE_D_3points, aes(x = slugSeason, y = number_of_threes, group = 1, color = "3 Pointers")) + 
  theme_bw() + geom_vline(xintercept  = 4, data = MODULE_A, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "NBA Season", y = "Number of shots taken", color = "Type of shot", 
       title = "Figure 3: The Number of two's versus three's in the NBA per team") 

g23
ggsave("outputs/doublelinegraphof2and3points.pdf", plot = g23, width = 10, height = 6)

#MODULE E: Break down by team
MODULE_E_2points <- shots_99_04 %>% group_by(slugSeason, nameTeam) %>% filter(typeShot == "2PT Field Goal") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

MODULE_E_3points <- shots_99_04 %>% group_by(slugSeason, nameTeam) %>% filter(typeShot == "3PT Field Goal") %>%
  summarise(number_of_threes = length(isShotAttempted)) 

gtwoandthree <- ggplot() +
  geom_line(data = MODULE_D_dunks, aes(x = slugSeason, y = numberofcloseshots, group = 1, color = "Dunks"))+
  #geom_line(data = MODULE_E_2points, aes(x = slugSeason, y = number_of_threes, group = 1, color = "2 Pointers")) +
  geom_line(data = MODULE_E_3points, aes(x = slugSeason, y = number_of_threes, group = 1, color = "3 Pointers")) + 
  theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~nameTeam) +
  labs(x = "NBA Season", y = "Number of shots taken", color = "Type of shot", 
       title = "Figure 3: The Number of two's versus three's in the NBA per team") 

gtwoandthree
ggsave("outputs/twothreepointergraphPerTeam.pdf", plot = gtwoandthree, width = 10, height = 6)
#ggplot(data = MODULE_D_3points, aes(x = slugSeason, y = number_of_threes, group = 1)) +
 # geom_line() + theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted") +
  #theme(axis.text.x = element_text(angle = 90))

#make two separate graphs

figure3 <- ggplot() +
  geom_line(data = MODULE_E_3points, aes(x = slugSeason, y = number_of_threes, group = 1), color = "purple", size = 0.5) + 
  theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~nameTeam) +
  labs(x = "NBA Season", y = "Number of shots taken", color = "Type of shot", 
       title = "Figure 3: The Number of three pointers taken in the NBA per team") 


figure3

ggsave("outputs/figure3.pdf", plot = figure3, width = 10, height = 6)

figure4 <- ggplot() +  
  geom_line(data = MODULE_D_dunks, 
            aes(x = slugSeason, 
            y = numberofcloseshots, group = 1), size = 0.5, color = "blue") +
theme_bw() + geom_vline(xintercept  = 3, data = MODULE_A, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~nameTeam) +
  labs(x = "NBA Season", y = "Number of Dunks taken",
       title = "Figure 4: The Number of dunks in the NBA per team per season") 

figure4

ggsave("outputs/figure4.pdf", plot = figure4, width = 10, height = 6)


  
  ### Run the significance test here

#filter

shots1999_2004 <- ALLNBASHOTS2 %>% 
  filter(yearSeason %in% c(1999,2000,2001,2002))


coef_data <- shots1999_2004 %>%  
  group_by(nameTeam) %>% 
  summarise(stat = tidy(lm_robust(typeShot ~ yearSeason)))

##see how many dunk shots are done


shots_99_04 <- ALLNBASHOTS2 %>% filter(between(yearSeason,1998,2004))
#clean the data X and Y locations
shots_99_04 <- cleanfile(shots_99_04)

#number of dunks
dunk_shots <- shots_99_04 %>%
  group_by(nameTeam, slugSeason) %>%
  summarise(number_ofdunks = sum(typeAction %in% c("Dunk Shot")))


gdunksSeason <- ggplot(data = dunk_shots, aes(x = slugSeason, y = number_ofdunks, group = 1)) +
  geom_line() + theme_bw() + geom_vline(xintercept  = 3, data = dunk_shots, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Figure 6: Number of Dunks taken between 1998-1999 to 2003-2004 Per team Per Season") + 
  facet_wrap(~nameTeam) + 
  theme(strip.text.x = element_text(size = 7, colour = "black")) +
  ylab("Number of Dunks taken") + xlab("NBA Season")

gdunksSeason

ggsave("outputs/dunksperseasonperteam.pdf", plot = gdunksSeason, width = 10, height = 6)

#Same as above, but per game
#number of

dunk_shots_game <- shots_99_04 %>%
  group_by(nameTeam, slugSeason, dateGame) %>%
  summarise(number_ofdunks = sum(typeAction %in% c("Dunk Shot"))) %>%
  mutate(date = as.Date(as.character(dateGame), "%Y%m%d"))


gdunksPerGame <- ggplot(data = dunk_shots_game, aes(x = date, y = number_ofdunks, group = 1)) + 
        geom_point(alpha = 0.4, stroke = 0) +
  geom_vline(xintercept  = as.Date("2001-10-01")) + stat_smooth(method = "lm_robust") +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Figure 8: Number of Dunks taken between 1998-1999 to 2003-2004 Per team") + 
  facet_wrap(~nameTeam) + 
  theme(strip.text.x = element_text(size = 7, colour = "black")) +
  ylab("Number of Dunks taken") + xlab("NBA Season") + theme_bw()

gdunksPerGame

ggsave("outputs/figure8.pdf", plot = gdunksPerGame, width = 9, height = 9)


#number of non-jump shots taken per team

#number of non-jumpShots
nonjump_shots <- shots_99_04 %>%
  group_by(nameTeam, slugSeason) %>%
  summarise(number_ofnonShots = sum(typeAction 
                %in% c("Dunk Shot", "Layup Shot", "Driving Dunk Shot", "Running Layup Shot", "Driving Layup Shot",
                       "Finger Roll Shot", "Alley Oop  Dunk Shot", "Driving Finger Roll Shot", "Reverse Layup Shot", "Driving Hook Shot",
                       "Running Dunk Shot", "Reverse Dunk Shot", "Turnaround Finger Roll Shot", "Alley Oop Layup shot" ,
                       "Follow Up Dunk Shot" , "Running Slam Dunk Shot", "Putback Reverse Dunk Shot" , 
                       "Cutting Dunk Shot" ,"Cutting Finger Roll Layup Shot","Running Alley Oop Dunk Shot",
                       "Tip Dunk Shot","Running Alley Oop Layup Shot","Driving Reverse Dunk Shot" ,        
                        "Running Reverse Dunk Shot")))


gnonshotsperseason <- ggplot(data = nonjump_shots, aes(x = slugSeason, y = number_ofnonShots, group = 1)) +
  geom_line() + theme_bw() + geom_vline(xintercept  = 3, data = nonjump_shots, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Number of non-jumpShots taken between 1998-1999 to 2003-2004 Per team") + 
  facet_wrap(~nameTeam) + 
  theme(strip.text.x = element_text(size = 7, colour = "black")) +
  ylab("Number of shots taken") + xlab("NBA Season")

ggsave("outputs/nonjumpshotsperteamlinegraph.pdf", plot = gnonshotsperseason, width = 4, height = 4)


#Same, but per game

nonjump_shots_pergame <- shots_99_04 %>%
  group_by(nameTeam, slugSeason, dateGame) %>%
  summarise(number_ofnonShots = sum(typeAction 
                                    %in% c("Dunk Shot", "Layup Shot", "Driving Dunk Shot", "Running Layup Shot", "Driving Layup Shot",
                                           "Finger Roll Shot", "Alley Oop  Dunk Shot", "Driving Finger Roll Shot", "Reverse Layup Shot", "Driving Hook Shot",
                                           "Running Dunk Shot", "Reverse Dunk Shot", "Turnaround Finger Roll Shot", "Alley Oop Layup shot" ,
                                           "Follow Up Dunk Shot" , "Running Slam Dunk Shot", "Putback Reverse Dunk Shot" , 
                                           "Cutting Dunk Shot" ,"Cutting Finger Roll Layup Shot","Running Alley Oop Dunk Shot",
                                           "Tip Dunk Shot","Running Alley Oop Layup Shot","Driving Reverse Dunk Shot" ,        
                                           "Running Reverse Dunk Shot"))) %>%
mutate(date = as.Date(as.character(dateGame), "%Y%m%d"))

gnonshotsperseasonperGame <- ggplot(data = nonjump_shots_pergame, aes(x = date, y = number_ofnonShots, group = 1)) +
  geom_point(alpha = 0.4, stroke = 0, col = nonjump_shots_pergame$slugSeason) + theme_bw() + geom_vline(xintercept  = 3, data = nonjump_shots, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Number of non-jumpShots taken between 1998-1999 to 2003-2004 Per team") + 
  facet_wrap(~nameTeam) + 
  theme(strip.text.x = element_text(size = 7, colour = "black")) +
  ylab("Number of shots taken") + xlab("NBA Season")

gnonshotsperseasonperGame

ggsave("outputs/nonjumpshotsperteamscatter.pdf", plot = gnonshotsperseasonperGame, width = , height = 6)



#Last part: coefficent plots (three pointers first)

library(broom)
library(estimatr)

rule_change <- shots_99_04 %>% 
  filter(slugSeason %in% c("1999-00", "2000-01")) %>%
  group_by(nameTeam, slugSeason, dateGame) %>% 
  summarise(numberofthrees = sum(typeShot == "3PT Field Goal"))

  #filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofthrees ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(numberofthrees ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(nameTeam = fct_reorder(nameTeam, estimate))

g_robust_threes <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
  geom_point() +
  geom_vline(xintercept = 0 , linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "Figure 9: Change of three pointer rates between 1999-2000 and 2000-2001 NBA Season",
                    x = "Rate of Change", y = "NBA Team")

ggsave("outputs/LMrobustthrees.pdf", plot = g_robust_threes, width = 10, height = 6)


#Do the same but with players and dunks instead 3 pointers and dunks. grab back to back meetings

#24 x 36 format, use 2 main figures, maybe use shot density chart from before and after, the change of three pointer rates,
#no worries about data cleaning. 

#Send an email,
#dunks
library(stringr)

rule_change <- shots_99_04 %>% 
  filter(slugSeason %in% c("1999-00", "2000-01")) %>%
  #filter(grepl("dunk",typeAction))
  group_by(nameTeam, slugSeason, dateGame) %>% 
  summarise(numberofcloseshots = sum(typeAction%in% c("Dunk Shot", "Layup Shot", "Driving Dunk Shot", "Running Layup Shot", "Driving Layup Shot",
                                                 "Finger Roll Shot", "Alley Oop  Dunk Shot", "Driving Finger Roll Shot", "Reverse Layup Shot", "Driving Hook Shot",
                                                 "Running Dunk Shot", "Reverse Dunk Shot", "Turnaround Finger Roll Shot", "Alley Oop Layup shot" ,
                                                 "Follow Up Dunk Shot" , "Running Slam Dunk Shot", "Putback Reverse Dunk Shot" , 
                                                 "Cutting Dunk Shot" ,"Cutting Finger Roll Layup Shot","Running Alley Oop Dunk Shot",
                                                 "Tip Dunk Shot","Running Alley Oop Layup Shot","Driving Reverse Dunk Shot" ,        
                                                 "Running Reverse Dunk Shot")))

#filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofcloseshots ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(numberofcloseshots ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(nameTeam = fct_reorder(nameTeam, estimate))

g_robust_close_shots <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "Change of close shot attempt rates between 1999-2000 and 2000-2001 NBA Season",
                    x = "Rate of Change", y = "NBA Team")

ggsave("outputs/LMrobustcloseshots.pdf", plot = g_robust_close_shots, width = 10, height = 6)


#Lets look at specific players

#they thought it was going to be like this, this is not what happened tho
#More evidence as to what people think it going to happen

rule_change <- shots_99_04 %>% 
  filter(slugSeason %in% c("1999-00", "2000-01")) %>%
  filter(namePlayer %in% c("Kevin Garnett", "Ray Allen", "Tim Duncan", "Vince Carter")) %>%
  group_by(namePlayer, slugSeason, dateGame) %>% 
  summarise(numberofdunksforthisplayer = sum(typeAction%in% c("Dunk Shot", "Layup Shot", "Driving Dunk Shot",
                                                      "Alley Oop  Dunk Shot",   
                                                      "Running Dunk Shot", "Reverse Dunk Shot",
                                                      "Follow Up Dunk Shot" , "Running Slam Dunk Shot", "Putback Reverse Dunk Shot" , 
                                                      "Cutting Dunk Shot" ,"Running Alley Oop Dunk Shot",
                                                      "Tip Dunk Shot","Driving Reverse Dunk Shot" ,        
                                                      "Running Reverse Dunk Shot")), 
            number_of_threes = sum(typeShot == "3PT Field Goal")) %>%
mutate(date = as.Date(as.character(dateGame), "%Y%m%d"))

#filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofdunksforthisplayer ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(namePlayer) %>% 
  summarise(tidy(lm_robust(numberofdunksforthisplayer ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(namePlayer = fct_reorder(namePlayer, estimate))


g_robust_dunks_player <- ggplot(gg_df, aes(x = estimate, y = namePlayer)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "Figure 11: Difference in dunk attempts between 1999-2000 and 2000-2001 NBA Season",
                    x = "Difference Estimate", y = "Player Name")

ggsave("outputs/figure11.pdf", plot = g_robust_dunks_player, width = 9, height = 6)



AppendixD <- shots_99_04 %>%
  filter(nameTeam == "Portland Trail Blazers", slugSeason == "2002-03") %>%
  group_by(idGame) %>% 
  summarise(shots_taken = sum(isShotAttempted == "TRUE"))
  
  
  #At the game level, number of dunks
  
  #recreate figure 7, which is now figure 9


#figure 9
figure9 <-
  ggplot(data = rule_change, aes(x = date, y = number_of_threes, group = namePlayer) )+
  geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
  geom_vline(xintercept  = as.Date("2001-10-01"),
             linetype = "dotted") +
  facet_wrap(~namePlayer) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Figure 9: Number of threes taken per game between 1998-1999 to 2003-2004") +
  ylab("Number of 3's taken") + xlab("NBA Seasons 1998-1999 to 2003-2004") +
  #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
  theme(#axis.ticks.x = element_blank(),
    axis.text.x = element_blank()) 
#scale_x_continuous(breaks=seq(1,10,by=1))

#scale_x_continuous(breaks=seq(1,10,by=1))

figure9

ggsave("outputs/figure9.pdf", plot = figure9, width = 10, height = 6)



#########

MODULE_fig910 <-
  shots_99_04 %>%
  filter(namePlayer %in% 
           c("Kevin Garnett", "Ray Allen", "Tim Duncan", "Vince Carter")) %>%
  mutate(date = as.Date(as.character(dateGame), "%Y%m%d")) %>%
  group_by(slugSeason, nameTeam, idGame, dateGame,namePlayer,date) %>%
  #mutate(date = as.Date(as.character(dateGame), "%Y%m%d")) %>%
  summarise(numberofdunksforthisplayer = 
              sum(
                typeAction %in% c(
                  "Dunk Shot",
                  "Layup Shot",
                  "Driving Dunk Shot",
                  "Alley Oop  Dunk Shot",
                  "Running Dunk Shot",
                  "Reverse Dunk Shot",
                  "Follow Up Dunk Shot" ,
                  "Running Slam Dunk Shot",
                  "Putback Reverse Dunk Shot" ,
                  "Cutting Dunk Shot" ,
                  "Running Alley Oop Dunk Shot",
                  "Tip Dunk Shot",
                  "Driving Reverse Dunk Shot" ,
                  "Running Reverse Dunk Shot"
                )
              ),
            number_of_threes = sum(typeShot == "3PT Field Goal"))


figure_9 <-
  ggplot(data = MODULE_fig910, aes(x = date, y = number_of_threes, group = slugSeason, colour = slugSeason)) +
  geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
  geom_vline(xintercept  = as.Date("2001-10-01"),
             linetype = "dotted") +
  theme_bw() + facet_wrap(~namePlayer) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Figure 9: Number of threes taken per game per player between 1998-1999 to 2003-2004") +
  ylab("Number of 3's taken") + xlab("NBA Seasons 1998-1999 to 2003-2004") +
  #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
  theme(#axis.ticks.x = element_blank(),
    axis.text.x = element_blank()) 
#scale_x_continuous(breaks=seq(1,10,by=1))

figure_9

ggsave("outputs/figure9.pdf", plot = figure_9, width = 9, height = 6)

#figure 10, same but with dunks

figure_10 <-
  ggplot(data = MODULE_fig910, aes(x = date, y = numberofdunksforthisplayer, group = slugSeason, colour = slugSeason)) +
  geom_point(alpha = 0.4, stroke = 0, col = MODULE_A$slugSeason) +
  geom_vline(xintercept  = as.Date("2001-10-01"),
             linetype = "dotted") +
  theme_bw() + facet_wrap(~namePlayer) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Figure 10: Number of dunks taken per game per player between 1998-1999 to 2003-2004") +
  ylab("Number of dunks taken") + xlab("NBA Seasons 1998-1999 to 2003-2004") +
  #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
  theme(#axis.ticks.x = element_blank(),
    axis.text.x = element_blank()) 
#scale_x_continuous(breaks=seq(1,10,by=1))

figure_10

ggsave("outputs/figure10.pdf", plot = figure_10, width = 9, height = 6)


#figure 11 (which use to be figure 8):

#figure 12

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(number_of_threes ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(namePlayer) %>% 
  summarise(tidy(lm_robust(number_of_threes ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(namePlayer = fct_reorder(namePlayer, estimate))


g_robust_dunks_player <- ggplot(gg_df, aes(x = estimate, y = namePlayer)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "Figure 12: Difference in three point attempts between 1999-2000 and 2000-2001 NBA Season",
                    x = "Difference estimate", y = "Player Name")

ggsave("outputs/figure12.pdf", plot = g_robust_dunks_player, width = 9, height = 6)


#figure 13 (used to be figure 9) and figure 14

# MODULE_fig1314 <-
#   shots_99_04 %>%
#   mutate(date = as.Date(as.character(dateGame), "%Y%m%d")) %>%
#   group_by(slugSeason, nameTeam, idGame, dateGame,date) %>%
#   #mutate(date = as.Date(as.character(dateGame), "%Y%m%d")) %>%
#   summarise(numberofdunks = 
#               sum(
#                 typeAction %in% c(
#                   "Dunk Shot",
#                   "Layup Shot",
#                   "Driving Dunk Shot",
#                   "Alley Oop  Dunk Shot",
#                   "Running Dunk Shot",
#                   "Reverse Dunk Shot",
#                   "Follow Up Dunk Shot" ,
#                   "Running Slam Dunk Shot",
#                   "Putback Reverse Dunk Shot" ,
#                   "Cutting Dunk Shot" ,
#                   "Running Alley Oop Dunk Shot",
#                   "Tip Dunk Shot",
#                   "Driving Reverse Dunk Shot" ,
#                   "Running Reverse Dunk Shot"
#                 )
#               ),
#             number_of_threes = sum(typeShot == "3PT Field Goal"))
# 
# 
# fit_1 <-
#   MODULE_fig1314 %>%
#   ungroup() %>% 
#   summarise(tidy(lm_robust(number_of_threes ~ slugSeason)))
# 
# fit_2 <- 
#   MODULE_fig1314 %>% 
#   group_by(nameTeam) %>% 
#   summarise(tidy(lm_robust(number_of_threes ~ slugSeason)))
# 
# gg_df <- 
#  fit_2 %>%
#   filter(term != "(Intercept)") %>%
#   ungroup() %>% 
#   mutate(nameTeam = fct_reorder(nameTeam, estimate))
# 
# 
# figure13 <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
#   geom_point() +
#   geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
#   theme_bw() + labs(title = "Figure 13: Difference in three point attempts between 1999-2000 and 2000-2001 NBA Season",
#                     x = "Difference Estimate", y = "Team Name")
# 
# figure13
# 
# ggsave("outputs/figure13.pdf", plot = figure13, width = 9, height = 6)
# 

rule_change <- shots_99_04 %>% 
  filter(slugSeason %in% c("1999-00", "2000-01")) %>%
  group_by(nameTeam, slugSeason, dateGame) %>% 
  summarise(numberofthrees = sum(typeShot == "3PT Field Goal"))

#filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofthrees ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(numberofthrees ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(nameTeam = fct_reorder(nameTeam, estimate))

figure13 <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
  geom_point() +
  geom_vline(xintercept = 0 , linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "Figure 13: Change in three pointer attemps between 1999-2000 and 2000-2001 NBA Season",
                    x = "Change estimate", y = "NBA Team")

figure13

ggsave("outputs/figure13.pdf", plot = figure13, width = 10, height = 6)

#same, but with dunks now for figure 14

rule_change <- shots_99_04 %>% 
  filter(slugSeason %in% c("1999-00", "2000-01")) %>%
  group_by(nameTeam, slugSeason, dateGame) %>% 
  summarise(numberofdunks = 
                                   sum(
                                     typeAction %in% c(
                                       "Dunk Shot",
                                       "Layup Shot",
                                       "Driving Dunk Shot",
                                       "Alley Oop  Dunk Shot",
                                       "Running Dunk Shot",
                                       "Reverse Dunk Shot",
                                       "Follow Up Dunk Shot" ,
                                       "Running Slam Dunk Shot",
                                       "Putback Reverse Dunk Shot" ,
                                       "Cutting Dunk Shot" ,
                                       "Running Alley Oop Dunk Shot",
                                       "Tip Dunk Shot",
                                       "Driving Reverse Dunk Shot" ,
                                       "Running Reverse Dunk Shot"
                                     )
                                   ))
                                 
#filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofdunks ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(numberofdunks ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(nameTeam = fct_reorder(nameTeam, estimate))

figure14 <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
  geom_point() +
  geom_vline(xintercept = 0 , linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "Figure 14: Change in dunk attemps between 1999-2000 and 2000-2001 NBA Season",
                    x = "Change estimate", y = "NBA Team")

figure14

ggsave("outputs/figure14.pdf", plot = figure14, width = 9, height = 6)

###########3 Appendix is done right here


rule_change <- ALLNBASHOTS2 %>% 
  filter(slugSeason %in% c("1998-99", "1999-00")) %>%
  group_by(nameTeam, slugSeason, dateGame) %>% 
  summarise(numberofthrees = sum(typeShot == "3PT Field Goal"))

#filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofthrees ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(numberofthrees ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(nameTeam = fct_reorder(nameTeam, estimate))

AppendixA1 <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
  geom_point() +
  geom_vline(xintercept = 0 , linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "AppendixA1: Change in three pointer attemps between 1998-1999 and 1999-2000 NBA Season",
                    x = "Change estimate", y = "NBA Team")

AppendixA1

ggsave("outputs/AppendixA1.pdf", plot = AppendixA1, width = 10, height = 6)

#same, but with dunks now for figure 14

rule_change <- ALLNBASHOTS2 %>% 
  filter(slugSeason %in% c("1998-99", "1999-00")) %>%
  group_by(nameTeam, slugSeason, dateGame) %>% 
  summarise(numberofdunks = 
              sum(
                typeAction %in% c(
                  "Dunk Shot",
                  "Layup Shot",
                  "Driving Dunk Shot",
                  "Alley Oop  Dunk Shot",
                  "Running Dunk Shot",
                  "Reverse Dunk Shot",
                  "Follow Up Dunk Shot" ,
                  "Running Slam Dunk Shot",
                  "Putback Reverse Dunk Shot" ,
                  "Cutting Dunk Shot" ,
                  "Running Alley Oop Dunk Shot",
                  "Tip Dunk Shot",
                  "Driving Reverse Dunk Shot" ,
                  "Running Reverse Dunk Shot"
                )
              ))

#filter(typeShot == "3PT Field Goal") %>% 

fit_1 <-
  rule_change %>%
  ungroup() %>% 
  summarise(tidy(lm_robust(numberofdunks ~ slugSeason)))

fit_2 <- 
  rule_change %>% 
  group_by(nameTeam) %>% 
  summarise(tidy(lm_robust(numberofdunks ~ slugSeason)))

gg_df <- fit_2 %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>% 
  mutate(nameTeam = fct_reorder(nameTeam, estimate))

AppendixA2 <- ggplot(gg_df, aes(x = estimate, y = nameTeam)) +
  geom_point() +
  geom_vline(xintercept = 0 , linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  theme_bw() + labs(title = "AppendixA2: Change in dunk attempts between 1998-1999 and 1999-2000 NBA Season",
                    x = "Change estimate", y = "NBA Team")

AppendixA2

ggsave("outputs/AppendixA2.pdf", plot = AppendixA2, width = 9, height = 6)





