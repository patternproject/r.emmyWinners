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
pacman::p_load(ggalt)
pacman::p_load(rjson)
pacman::p_load(XML)
pacman::p_load(htmltab)
pacman::p_load(extrafont)
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Scrap Data  ####
## `````````````````````````````````````````````

### creating a df of wikipedia URL for all winners ####
# 1. wd ####
# read list of winners from 2015 back to 2005
setwd("D:/2. Bianca/1. Perso/43. Emmy Winners")
# 2. df.winners ####
df.winners = read.csv(
  "2. Data/winnerlist.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)
# 3. v.underscore ####
# append underscore to series names"
v.underscore = 
  str_trim(df.winners$name) %>%
  str_to_lower() %>%
  str_replace_all(" ","_") 
# 4. special cases ####
# a. veep 
# capitalize it again 
v.underscore[2] = "Veep"
# b. the office
# https://en.wikipedia.org/wiki/List_of_The_Office_(U.S._TV_series)_episodes
v.underscore[20] = "The_Office_(U.S._TV_series)"  
# c.everybody loves raymond
# capitalize again
v.underscore[22] = "Everybody_Loves_Raymond"
# https://en.wikipedia.org/wiki/List_of_everybody_loves_raymond_episodes
# https://en.wikipedia.org/wiki/List_of_Everybody_Loves_Raymond_episodes
# 5. v.underscore.1 ####
v.underscore.1 = sapply(v.underscore, function(x) {paste0("_",x,"_")}, simplify=TRUE, USE.NAMES=FALSE)
# 6. base and tail URL ####
base.url="https://en.wikipedia.org/wiki/List_of"
tail.url = "episodes"
# 7. v.url ####
v.url = sapply(v.underscore.1, function(x) {paste0(base.url,x,tail.url)}, simplify=TRUE, USE.NAMES=FALSE)
# 8. test valid url ####
browseURL(sample(v.url,1))
# 9. clean up ####
rm(base.url)
rm(tail.url)
rm(v.underscore)
rm(v.underscore.1)

### looping mechanism for going through all urls in df.winners ####
# 1.vector of means ####
# src: 
# http://stackoverflow.com/questions/22235809/append-value-to-empty-vector-in-r
# <<- global assignment
v.mean <<- numeric(NROW(df.winners))
# 2.counter var ####
# not required any more
# i.count = 0
# 3.url var ####
i.url = 1
# 4.reset i.mean ####
i.mean = 0
# 5.download table from wikipedia ####
## SRC
## http://stackoverflow.com/questions/7407735/importing-wikipedia-tables-in-r
## PROBLEM: We do not know which number of tables
## or season # is the table #

# helper function
## function get.mean() ####
get.mean = function(i.url, i.table)
{
  # if(i.url == 21)
  #   browser()

  df.temp = htmltab(v.url[i.url],i.table)
  
  # remove row with description
  v.t = 
    df.temp %>%
    select(contains("viewers")) 
  
  # assuming non-text row has length less than 6 (2 decimal places 10.88 is length 5)
  v.t = v.t[(sapply(v.t,str_length))<6,]
  
  # dump v.t for later
  f.name = paste0(i.url,".csv")
  f.name = paste0("4. Scraped Data/",f.name)
  
  # storing Title Name 
  s.url = sprintf("The url is %s",v.url[i.url])
  write.table(s.url,file=f.name, row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  
  # Storing Table Number
  s.table = sprintf("The table is %s",i.table)
  write.table(s.table,file=f.name, row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  
  # Storing Table Values
  s.value = sprintf("The values are")
  write.table(s.value,file=f.name, row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  write.table(v.t,file=f.name, row.names=T,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  
  # calculating mean of the col containing viewers
  i.mean = 
    v.t %>%
    as.matrix() %>%
    as.vector() %>%
    as.numeric() %>%
    mean(.,na.rm=TRUE) %>%
    round(.,2)

  return(i.mean)
  
} # end get.mean

### scrap one by one data for each winner ####

# 2015 d ####
# for game of thrones, season 5 is the winning season
i.table = 5
i.mean = get.mean(i.url,i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2015 c ####
# for veep, season 4 is the winning season
i.table = 5
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2014 d ####
# for breaking bad
#browseURL(v.url[3])
i.table = 6
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2014 c ####
# for modern family 
# browseURL(v.url[i.url])
i.table = 6
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2013 d ####
# for breaking bad
#browseURL(v.url[3])
i.table = 5
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2013 c ####
# for modern family 
# browseURL(v.url[i.url])
i.table = 5
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2012 d ####
# for homeland
#browseURL(v.url[3])
i.table = 3
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2012 c ####
# for modern family 
# browseURL(v.url[i.url])
i.table = 4
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2011 d ####
# for mad men
#browseURL(v.url[3])
i.table = 5
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2011 c ####
# for modern family 
# browseURL(v.url[i.url])
i.table = 3
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2010 d ####
# for mad men
#browseURL(v.url[3])
i.table = 4
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2010 c ####
# for modern family 
# browseURL(v.url[i.url])
i.table = 2
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2009 d ####
# for mad men
#browseURL(v.url[3])
i.table = 3
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2009 c ####
# for 30 Rock 
# browseURL(v.url[i.url])
i.table = 4
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2008 d ####
# for mad men
#browseURL(v.url[3])
i.table = 2
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2008 c ####
# for 30 Rock 
# browseURL(v.url[i.url])
i.table = 3
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2007 d ####
# for the sopranos
#browseURL(v.url[3])
i.table = 7
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2007 c ####
# for 30 Rock 
# browseURL(v.url[i.url])
i.table = 2
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2006 d ####
# for 24
#browseURL(v.url[3])
i.table = 5
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2006 c ####
# for The Office 
# browseURL(v.url[i.url])
i.table = 3
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2005 d ####
# for Lost
#browseURL(v.url[3])
i.table = 3
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2005 c ####
# for Everybody Loves Ramond 
# browseURL(v.url[i.url])
i.table = 10
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2004 d ####
# for The Sopranos
#browseURL(v.url[3])
i.table = 5
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

# 2004 c ####
# for Arrested Devleopment 
# browseURL(v.url[i.url])
i.table = 2
i.mean = get.mean(i.url, i.table)

# storing the mean
v.mean[i.url] = i.mean

# updating counters
i.url = i.url + 1

## `````````````````````````````````````````````

#### Visulization  ####
## `````````````````````````````````````````````

## setup ####

## appending the mean to the df
df.winners$mean.us.viewers.m = v.mean

# making genre a factor
df.winners$genre = as.factor(df.winners$genre)

## clean up
#rm(i.count)
rm(i.mean)
rm(i.table)
rm(i.url)
rm(v.mean)
rm(v.url)


# for plotting setting Year as Date Type (else treated as integer)
df.winners$year2 = as.Date(as.character(df.winners$year),format="%Y")

## color codes ####
col.grey = "#707070"
col.teal = "#368C8C"
col.blue = "#4682B4"
col.mid.green = "#98EFC1"
col.lig.green = "#B8FFD1"
col.dark.red = "darkred"


## g.1 ####

g.1 = ggplot(data=df.winners) + theme_minimal()

g.1 = g.1 + geom_line(data = filter(df.winners, grepl('COMEDY', genre)),
                      aes(x = year2, y = mean.us.viewers.m),alpha=0.8,size=0.5,col=col.lig.green)

g.1 = g.1 + geom_point(data = filter(df.winners, grepl('COMEDY', genre)),
                      aes(x = year2, y = mean.us.viewers.m),alpha=0.6,size=2,col=col.blue)

g.1 = g.1 + geom_line(data = filter(df.winners, grepl('DRAMA', genre)),
                      aes(x = year2, y = mean.us.viewers.m),alpha=0.8,size=0.5,col=col.lig.green)

g.1 = g.1 + geom_point(data = filter(df.winners, grepl('DRAMA', genre)),
                      aes(x = year2, y = mean.us.viewers.m),alpha=0.4,size=2,col=col.dark.red)

#g.1 = g.1 + geom_point(aes(x=year2,y=mean.us.viewers.m, col=genre),size=2)

g.2 = g.1 + theme (
  panel.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.x = element_blank(),
  #axis.title.y = element_blank()
  axis.title.y = element_text(size = 8)#,


)

g.2 = g.2 +
  labs(
    y = NULL,
    title = "How Viewership varies across the years for Emmy Winners",
    subtitle = "Counts are in Millions",
    caption = "Source: Wikipedia"
  ) 
  
g.2 = g.2 +
  scale_x_date(
    breaks = seq(as.Date("2004/1/1"), as.Date("2016/1/1"), by = "years"),
    labels = c("2004","","06","","08","","10","","12","","14","","16")
  ) 

g.2 = g.2 + scale_y_continuous(limits=c(0,20)) 

g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                       aes(x=as.Date("2005/12/1"), y=19, label="Cross-over \n Point 1"))

g.2 = g.2 + geom_point(data=data.frame(),aes(x=as.Date("2005/7/1"), y=19),alpha=0.1,size=8,col=col.grey)


g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2007/9/1"), y=4.3, label="Cross-over \n Point 2"))

g.2 = g.2 + geom_point(data=data.frame(),aes(x=as.Date("2007/12/1"), y=5.7),alpha=0.1,size=8,col=col.grey)

g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2014/1/1"), y=5.3, label="Cross-over \n Point 3"))

g.2 = g.2 + geom_point(data=data.frame(),aes(x=as.Date("2015/1/1"), y=5.7),alpha=0.1,size=8,col=col.grey)

g.2