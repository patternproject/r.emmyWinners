## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
# Emmy Award Winners
# 2016 Jul 21

# Difference of Avg Viewers for Emmy Winners 
# across years, across "Comedy and Drama" category
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,ggplot2,tidyr,scales,grid,stringr,rvest)
pacman::p_load(rjson)
pacman::p_load(XML)
pacman::p_load(htmltab)
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Scrap Data  ####
## `````````````````````````````````````````````

# read list of winners from 2015 back to 2005
setwd("D:/2. Bianca/1. Perso/43. Emmy Winners")

## df.winners ####
df.winners = read.csv(
  "2. Data/winnerlist.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

## v.underscore ####
# append underscore to series names"
v.underscore = 
  str_trim(df.winners$name) %>%
  str_to_lower() %>%
  str_replace_all(" ","_") 

# veep is special case
# capitalize it again 
v.underscore[2] = "Veep"
  
## v.underscore.1 ####
v.underscore.1 = sapply(v.underscore, function(x) {paste0("_",x,"_")}, simplify=TRUE, USE.NAMES=FALSE)

## base and tail URL ####
base.url="https://en.wikipedia.org/wiki/List_of"
tail.url = "episodes"

## v.url ####
v.url = sapply(v.underscore.1, function(x) {paste0(base.url,x,tail.url)}, simplify=TRUE, USE.NAMES=FALSE)

## test valid url ####
browseURL(sample(v.url,1))

## vector of means ####
# src: 
# http://stackoverflow.com/questions/22235809/append-value-to-empty-vector-in-r
v.mean = numeric(NROW(df.winners))

# counter var
i.count = 0

# download table from wikipedia
# SRC
# http://stackoverflow.com/questions/7407735/importing-wikipedia-tables-in-r
# PROBLEM: We do not know which number of tables
# or season # is the table #

# for game of thrones, season 5 is the winning season
df.2015.d = htmltab(v.url[1],5)

# calculating mean of the col containing viewers
i.mean = 
  df.2015.d %>%
  select(contains("viewers")) %>%
  as.matrix() %>%
  as.vector() %>%
  as.numeric() %>%
  mean() %>%
  round(.,2)

# storing the mean
v.mean = append(v.mean,i.mean,icount)
i.count = i.count + 1


# for veep, season is the winning season
df.2015.c = htmltab(v.url[2],5)

# for breaking bad
browseURL(v.url[3])
df.2014.d = htmltab(v.url[3],6)


browseURL(v.url[2])


## 

.mw-headline


## `````````````````````````````````````````````