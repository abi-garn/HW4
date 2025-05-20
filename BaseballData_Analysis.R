library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/Users/abigarn/Desktop/DavisWork/Yr1Q3/STA141B/HMW/BaseballData")
db = dbConnect(SQLite(), "lahman_1871-2022.sqlite")






#Question1
df.q1.1 = dbGetQuery(db, "SELECT COUNT(DISTINCT teamID) AS 'Number of Teams'
                      FROM Teams;");df.q1.1 

df.q1.2 = dbGetQuery(db, "SELECT COUNT(DISTINCT name) AS 'Number of Teams in 2022'
           FROM Teams
           WHERE yearID = 2022;");df.q1.2

#Note: The following does not use "name" in its GROUP BY clause
  #TheResult: Two team names are ommitted
df.q1.3 = dbGetQuery(db, "SELECT name, lgID,  divID, W, L
           FROM Teams
           WHERE yearID = 2022
           ORDER BY lgID,  divID, W;");df.q1.3



lines = dbReadTable(db, "Teams")
lines.teamID = lines$teamID
length(unique(lines.teamID))

lines = dbReadTable(db, "Teams")
lines.yearID = lines$yearID
w = lines.yearID == 2022
lines.2022 = lines[w,]; lines.2022
y = unique(lines.2022$name); 
length(unique(y))



lines = dbReadTable(db, "Teams")
relatedLines = subset(lines, lines$yearID == 2022)
resultingDF = relatedLines[order(relatedLines$lgID, relatedLines$divID,relatedLines$W),]
resultingDF = resultingDF[,c("name","lgID", "divID", "W", "L")]
rownames(resultingDF) = NULL

identical(resultingDF, df.q1.3)




#Question2
df.q2 = dbGetQuery(db, "SELECT  name, AVG(attendance) AS avg_attendance, SUM(attendance) AS total_attendance
           FROM Teams
           WHERE yearID >= 2015
           GROUP BY teamID
           ORDER BY avg_attendance DESC
           LIMIT 10;"); df.q2
lines = dbReadTable(db, "Teams")
relatedLines = subset(lines, lines.yearID >= 2015)

relatedLines.names = split(relatedLines$name, relatedLines$teamID)
relatedLines.attendance = split(relatedLines$attendance, relatedLines$teamID)
average_attendance = sapply(relatedLines.attendance, mean)
average_attendance = as.data.frame(average_attendance)
average_attendance = cbind(rownames(average_attendance), average_attendance)

total_attendance = sapply(relatedLines.attendance, sum)

total_attendance = as.data.frame(total_attendance)
View(relatedLines.names)
names = do.call(rbind, relatedLines.names)
names = names[,1]; View(names)
df = cbind(names, average_attendance, total_attendance)

df= df[order(-df$average_attendance),]
df = head(df, 10)
rownames(df) = NULL
df = df[,-2]


names(df) = c("name", "avg_attendance", "total_attendance")
View(df)
identical(df, df.q2)









#Question3
View(dbGetQuery(db, "SELECT playerID, COUNT(DISTINCT yearID) as years_played
           FROM Appearances
           GROUP BY playerID
           ORDER BY years_played DESC
           LIMIT 10;"))
lines = dbReadTable(db, "Appearances")

lines.yearID = lines$yearID; 
lines.playerID = lines$playerID; 
lines.grouped.q3 = split(lines.yearID, lines.playerID); lines.grouped.q3
indexes.OfDuplicated = sapply(lines.grouped.q3, duplicated); indexes.OfDuplicated
#For each list within lines.grouped.q3, only values that do not represent duplicates are included
lines.grouped.q3=mapply(function(x,m) x[!m],lines.grouped.q3,indexes.OfDuplicated);


lines.yearsPlayed = sapply(lines.grouped.q3, length)
head(sort(lines.yearsPlayed, decreasing=TRUE),10); 




View(dbGetQuery(db, "SELECT playerID, SUM(G_all) as total_games
           FROM Appearances
           GROUP BY playerID
           ORDER BY total_games DESC
           LIMIT 10;"))
lines = dbReadTable(db, "Appearances")

lines.playerID = lines$playerID; 
lines.G_all = lines$G_all; 
lines.grouped.q3pt2 = split(lines, lines.playerID);head(lines.grouped.q3pt2,1)
lines.gamesPlayed = sapply(lines.grouped.q3pt2, function(x) sum(x$G_all))
head(sort(lines.gamesPlayed, decreasing=TRUE),10)


#Question4
df.q4 = dbGetQuery(db, "SELECT name, lgID, divID, W, L, G
           FROM Teams
           WHERE WSWin = 'Y' AND yearID = 2022;"); View(df.q4)
lines = dbReadTable(db, "Teams")
View(lines)
lines.yearID = lines$yearID; 
lines.WSWin = lines$WSWin;

relatedLines.2 = subset(lines, lines.yearID == 2022 & lines.WSWin == 'Y')
relatedLines.2[,c("name", "lgID", "divID", "W", "L", "G")]




#Question5
df.q5=(dbGetQuery(db, "SELECT name, lgID, divID, W, L, WSWin, yearID
           FROM Teams
           GROUP BY yearID,WSWin;"))

View(df.q5)
lines = dbReadTable(db, "Teams")

lines = lines[order(lines$yearID, is.na(lines$WSWin), lines$WSWin),]
groupings = paste(lines$yearID, lines$WSWin)


unique = !duplicated(groupings)
df = lines[unique, c("name", "lgID", "divID", "W", "L", "WSWin", "yearID")]
df = df[order(df$yearID, !is.na(df$WSWin), df$WSWin),]
rownames(df) = NULL



df = lapply(df, function(x) trimws(x))
df.q5 = lapply(df.q5, function(x) trimws(x))
identical(df.q5,df)









#Question6
plotData = dbGetQuery(db, "SELECT Teams.name, SUM(Salaries.salary) AS payroll, Salaries.yearID, WSWin
           FROM Salaries
           LEFT JOIN Teams ON Salaries.teamID = Teams.teamID AND Salaries.yearID = Teams.yearID
           GROUP BY Salaries.teamID, Salaries.yearID
            ORDER BY Salaries.teamID
           ;")
View(plotData)
lines.Salaries = dbReadTable(db, "Salaries")
lines.Teams = dbReadTable(db, "Teams")
lines = merge(lines.Salaries, lines.Teams, by = c("teamID", "yearID")); View(lines)
lines.payroll = tapply(lines$salary, list(lines$teamID, lines$yearID), sum)

lines.selected = lines[,c("name", "yearID", "WSWin")]
View(lines.selected)
View(lines.payroll)
lines.selected$yearID
payroll.years = dimnames(lines.payroll)[[2]];
payroll.years = as.list(payroll.years)
length(lines.selected$yearID)
nrow(lines.selected)

ggplot(plotData,aes(x=yearID, y=payroll, color=name, shape=WSWin, group=name)) + geom_point(size=3) + geom_line()



#Question7FINISH
View(dbGetQuery(db, "SELECT Schools.name_full, Schools.city, Schools.state, COUNT(DISTINCT CollegePlaying.playerID) as num_of_players 
           FROM Schools
           INNER JOIN CollegePlaying ON Schools.schoolID = CollegePlaying.schoolID
           WHERE yearID >= 2000
           GROUP BY name_full
           ORDER BY num_of_players DESC
           LIMIT 10;"))




#Question8
q8 = dbGetQuery(db, "SELECT yearID, COUNT(DISTINCT gameID) AS 'Number of Games', COUNT(playerID) AS 'Number of Players'
           FROM AllstarFull
           GROUP BY yearID;")

lines = dbReadTable(db, "AllstarFull")
lines.yearID = lines$yearID; 
lines.gameID = lines$gameID; 
lines.playerID = lines$playerID; 
lines.grouped = split(lines, c(lines.yearID));
lines.numberOfPlayers = lapply(lines.grouped, function(x) length(x$playerID)); head(lines.numberOfPlayers,30)
df.numberOfPlayers = do.call(rbind, lines.numberOfPlayers)


indexes.OfDuplicated = lapply(lines.grouped, function(x) x[!duplicated(x$gameID),]); 

lines.numberOfGames = lapply(indexes.OfDuplicated, function(x) sum(!is.na(x$gameID))); head(lines.numberOfGames,30)
df.numberOfGames = do.call(rbind, lines.numberOfGames)


resultingDF = (cbind(df.numberOfGames,df.numberOfPlayers)); tail(resultingDF)
resultingDF = cbind(rownames(resultingDF),resultingDF)
rownames(resultingDF) = NULL
resultingDF = as.data.frame(resultingDF)
names(resultingDF) = c("yearID", "Number of Games", "Number of Players")


resultingDF = lapply(resultingDF, function(x) trimws(x))
q8 = lapply(q8, function(x) trimws(x))

identical(resultingDF, q8)








#Question9
df.sql = dbGetQuery(db,  "SELECT playerID, COUNT(GP) AS allstar_games_played, GROUP_CONCAT(DISTINCT teamID) AS teams, 
GROUP_CONCAT(DISTINCT yearID) AS years, MIN(DISTINCT yearID) AS first_year, MAX(DISTINCT yearID) AS last_year
          FROM AllstarFull
          GROUP BY playerID
          HAVING allstar_games_played >= 15;")
View(df.sql)
lines = dbReadTable(db, "AllstarFull")
lines.grouped = split(lines, lines$playerID);

lines.allstar_games_played = lapply(lines.grouped, function(x) length(x$GP)); head(lines.numberOfPlayers,30)
df.allstar_games_played = do.call(rbind, lines.allstar_games_played)

indexes.OfDuplicated = lapply(lines.grouped, function(x) paste(unique(as.numeric(x$yearID)),collapse=","));
indexes.OfDuplicated.forCalculations = lapply(lines.grouped, function(x) unique(as.numeric(x$yearID)));
lines.dates = do.call(rbind, indexes.OfDuplicated);
lines.dates.2 = do.call(rbind, indexes.OfDuplicated.forCalculations);


min=apply(lines.dates.2,1,min); View(min)
min = data.frame(first_year = min)
rownames(min) = NULL

max=apply(lines.dates.2,1,max); 
max = data.frame(last_year = max)
rownames(max) = NULL


lines.dates.forCalculations = do.call(rbind, indexes.OfDuplicated.forCalculations);
View(lines.dates.forCalculations)

indexes.OfDuplicated = lapply(lines.grouped, function(x) paste(unique((x$teamID)),collapse=","));
lines.teamID  = do.call(rbind, indexes.OfDuplicated);
View(lines.teamID)

df = cbind(as.numeric(df.allstar_games_played),lines.teamID,lines.dates);
df = cbind(rownames(df),df)
rownames(df) = NULL

df2 = cbind(df, min,max); 
df2 = subset(df2, df.allstar_games_played>=15)

names(df2) = c("playerID", "allstar_games_played", "teams", 
             "years", "first_year", "last_year")
df2 = subset(df2, df2$allstar_games_played >= 15)
rownames(df2) = NULL

df2 = lapply(df2, function(x) trimws(x))
df.sql = lapply(df.sql, function(x) trimws(x))

identical(df2,df.sql)








#Question10
q10 = dbGetQuery(db, "SELECT playerID, COUNT(yearID) AS number_of_playoff_years, MIN(yearID) AS first_year, MAX(yearID) AS last_year, COUNT(DISTINCT teamID) AS number_of_teams 
           FROM (
                SELECT playerID, yearID, teamID FROM BattingPost
                UNION 
                SELECT playerID, yearID, teamID FROM FieldingPost
                UNION 
                SELECT playerID, yearID, teamID FROM PitchingPost
           )as combo
           GROUP BY playerID
           ORDER BY number_of_playoff_years DESC
           LIMIT 10;
           ")
View(q10)
#Breaking q10 apart (To allow for an accurate comparison)
q10 = lapply(q10, function(x) trimws(x))

lines1 = dbReadTable(db, "BattingPost")
lines1 = lines1[,c("playerID", "yearID", "teamID")]
lines2 = dbReadTable(db, "FieldingPost")
lines2 = lines2[,c("playerID", "yearID", "teamID")]
lines3 = dbReadTable(db, "PitchingPost")
lines3 = lines3[,c("playerID", "yearID", "teamID")]
lines = rbind(lines1, lines2, lines3); 
lines.grouped = split(lines, lines$playerID); 

indexes.OfDuplicated = lapply(lines.grouped, function(x) x[!duplicated(x$teamID),]); 
lines.number_of_teams = lapply(indexes.OfDuplicated, function(x) length(x$teamID)); 
df.number_of_teams = do.call(rbind, lines.number_of_teams); 

indexes.OfDuplicated = lapply(lines.grouped, function(x) x[!duplicated(x$yearID),]);
lines.number_of_years = lapply(indexes.OfDuplicated, function(x) length(x$yearID)); 
df.number_of_years = do.call(rbind, lines.number_of_years); 

lines.first_year = lapply(lines.grouped, function(x) min(x$yearID)); 
df.first_year = do.call(rbind, lines.first_year);

lines.last_year = lapply(lines.grouped, function(x) max(x$yearID)); 
df.last_year = do.call(rbind, lines.last_year); 

resultingDF = (cbind(df.number_of_years, df.first_year, df.last_year,df.number_of_teams)); 
names(resultingDF) = c("number_of_playoff_years", "first_year", "last_year", "number_of_teams")

resultingDF = resultingDF[order(resultingDF[,1],rownames(resultingDF), decreasing=TRUE),]


resultingDF = cbind(rownames(resultingDF),resultingDF)
rownames(resultingDF) = NULL
resultingDF = as.data.frame(resultingDF)


names(resultingDF) = c("playerID", "number_of_playoff_years", "first_year", "last_year", "number_of_teams")
resultingDF = head(resultingDF,10)

resultingDF = lapply(resultingDF, function(x) trimws(x))

identical(resultingDF, q10)

