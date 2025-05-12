library(RSQLite)
setwd("/Users/abigarn/Desktop/DavisWork/Yr1Q3/STA141B/HMW/BaseballData")
fileList = list.files("baseball-archive-sqlite-2022", pattern = "\\.sqlite", full.names = TRUE); fileList[1]
c=dbConnect(SQLite(), fileList[2])
tables = dbListTables(c)
class(c)
class(tables)
dbReadTable(c,tables[1])
print("Saving")

connect = lapply(fileList, function(x) dbConnect(SQLite(), x));connect[2]
a=lapply(fileList, function(x) print(x))
tables = lapply(connect, function(x) dbListTables(x)); tables[2]
connect = unlist(connect)
class(connect[[1]])
sapply(connect, class)

# a = unlist(connect[[1]]); a
# b = unlist(tables[1])
# dbReadTable(a,b)

connect = unlist(connect); sapply(tables,class)
tables = unlist(tables); sapply(tables,class)
readingInfo = mapply((function(x,y) dbReadTable(x,y)), connect, tables)










library(RSQLite)
setwd("/Users/abigarn/Desktop/DavisWork/Yr1Q3/STA141B/HMW/BaseballData")
fileList = list.files("baseball-archive-sqlite-2022", pattern = "\\.sqlite", full.names = TRUE); fileList[1]
c=lapply(fileList, function(x) dbConnect(SQLite(), x)); 
tables = lapply(c, function(x) dbListTables(x)); tables[4]
readLines = lapply(tables, function(x) dbReadTable(x))

print(fileList)
file.2022 = fileList[4];
class(file.2022)
dbGetQuery(file.2022, "SELECT COUNT(*) FROM 'Teams'")