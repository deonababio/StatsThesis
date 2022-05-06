
i = 1997
while(i < 2006) {
  assign(paste("season",i, sep=""), game_logs(seasons = i, nest_data = TRUE))
  i= i + 1
}

season1997_2003 = do.call("rbind", list(season1997, season1998, season1999, season2000, season2001, season2002, season2003, season2004, season2005))

write_csv(season1997_2003, "Datasets/season1997_2003.csv")
