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

# wd ####
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
## clean up ####
rm(base.url)
rm(tail.url)

## vector of means ####
# src: 
# http://stackoverflow.com/questions/22235809/append-value-to-empty-vector-in-r
# <<- global assignment
v.mean <<- numeric(NROW(df.winners))
# counter var ####
i.count = 0
# url var ####
i.url = 1
# download table from wikipedia ####
# SRC
# http://stackoverflow.com/questions/7407735/importing-wikipedia-tables-in-r
# PROBLEM: We do not know which number of tables
# or season # is the table #

# function approach does not work
## function get.mean() ####
get.mean = function(i.url, i.table)
{
  df.temp = htmltab(v.url[i.url],i.table)
  
  # remove row with description
  v.t = 
    df.temp %>%
    select(contains("viewers")) 
  
  # assuming non-text row has length less than 5
  v.t = v.t[(sapply(v.t,str_length))<5,]
  

  # calculating mean of the col containing viewers
  i.mean = 
    v.t %>%
    as.matrix() %>%
    as.vector() %>%
    as.numeric() %>%
    mean() %>%
    round(.,2)
  
  return(i.mean)
  
} # end get.mean

# 2015 d ####
# for game of thrones, season 5 is the winning season
i.table = 5
i.mean = get.mean(i.url,i.table)

# storing the mean
v.mean <- append(v.mean,i.mean,i.count)

# updating counters
i.url = i.url + 1
i.count = i.count + 1

# 2015 c ####
# for veep, season 4 is the winning season
i.table = 5
i.mean = get.mean(i.url, i.table)
# storing the mean
v.mean <- append(v.mean,i.mean,i.count)

# updating counters
i.url = i.url + 1
i.count = i.count + 1

# 2014 d ####
# for breaking bad
#browseURL(v.url[3])
i.table = 6
i.mean = get.mean(i.url, i.table)
# storing the mean
v.mean <- append(v.mean,i.mean,i.count)

# updating counters
i.url = i.url + 1
i.count = i.count + 1

# 2014 c ####
# for modern family 
# browseURL(v.url[i.url])
i.table = 6
i.mean = get.mean(i.url, i.table)
# storing the mean
v.mean <- append(v.mean,i.mean,i.count)

# updating counters
i.url = i.url + 1
i.count = i.count + 1



## `````````````````````````````````````````````