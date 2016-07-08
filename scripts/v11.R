## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
# Emmy Award Winners
# 2016 Jul 21

# Difference of Avg Viewers for Emmy Winners 
# across years, across "Comedy and Drama" category
# v11: comparing GDP Per Capita Change with Viewership Change, Year on Year
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````

# install devtools
install.packages('devtools')

# dev version of ggplot
# main branch of development
devtools::install_github("hadley/ggplot2")

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

## instead of scraping data, lets read the scraped data
# wd
setwd("D:/2. Bianca/1. Perso/temp/r.emmyWinners")
# df.master ####
df.master = read.csv(
  "data/winners mean.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

df.winners = df.master

# setup for g.3 plot (yearly change)
df.winners = 
  df.winners %>%
  select(-X) #removing row index 

# calculating change or rate from last year
df.winners.1 = 
  df.winners %>%
  arrange(genre,year) %>%
  group_by(genre) %>%
  mutate(us.viewers.change = mean.us.viewers.m - lag(mean.us.viewers.m)) 

# removing missing value from the first row
df.winners.1 = 
  df.winners %>%
  na.omit()

## reading in gdp data
# df.gdp ####
df.gdp = read.csv(
  "data/UsaGdp v2.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# fixing names
names(df.gdp) = tolower(names(df.gdp))

# dropping non-reqd cols
df.gdp.1 =
  df.gdp %>%
  select(year,gdp.per.capita.growth...) %>%
  rename(gdp.p.capita.change = gdp.per.capita.growth...)

# adding gdp change values
df.winners.1 = 
  left_join(df.winners.1,df.gdp.1,by="year")

# rearranging cols
df.winners.1 = 
  df.winners.1 %>%
  select(year,year2,gdp.p.capita.change,genre,mean.us.viewers.m,name)

# for plotting setting Year as Date Type (else treated as integer)
df.winners.1$year2 = as.Date(as.character(df.winners.1$year),format="%Y")

i.offset = 0.1

# if any gdp change value for last year equal to this year, change it to ensure graph looks ok
df.winners.1 = 
  df.winners.1 %>%
  arrange(genre) %>%
  mutate(
    gdp = ifelse(
      gdp.p.capita.change - lag(gdp.p.capita.change) == 0, # test
      gdp.p.capita.change - i.offset, # yes both same, change it
      gdp.p.capita.change # not same, no fixing required
    )
  )

# add gdp value for comedy genre, year 2015 as 1.6
df.winners.1 = 
  df.winners.1 %>%
  mutate(gdp = replace(gdp, which(genre == 'COMEDY' & year == 2015), 1.6))
  


## writing out the csv file
# write.csv(df.winners.1,"winnersListWithGdp.csv")

# clean up
rm(df.gdp)
rm(df.gdp.1)
rm(df.master)
rm(df.winners)


#### Visulization  ####
## `````````````````````````````````````````````

## color codes ####
col.grey = "#707070"
col.teal = "#368C8C"
col.blue = "#4682B4"
col.mid.green = "#98EFC1"
col.lig.green = "#B8FFD1"
col.dark.red = "darkred"


col.l.grey = "#D3D0CB"
col.m.grey = "#9FB1BC"
col.d.grey = "#6E8898"
col.d.blue = "#2E5266"
col.d.yellow = '#E2C044'



## g.1 ####

g.1 = ggplot(data=df.winners.1) + theme_minimal()

g.1 = g.1 + facet_grid(. ~ genre)

g.1 = g.1 + geom_line(aes(x = year2, y = gdp),alpha=0.8,size=0.5,col=col.d.grey)

g.1 = g.1 + geom_point(aes(x = year2, y = gdp),alpha=0.6,size=2,col=col.d.blue)

g.1 = g.1 + geom_line(aes(x = year2, y = mean.us.viewers.m),alpha=0.6,size=0.5,col=col.d.yellow)

g.1 = g.1 + geom_point(aes(x = year2, y = mean.us.viewers.m),alpha=0.4,size=2,col=col.d.yellow)

g.1 = g.1 + geom_ribbon(aes(x = year2, ymin=gdp, ymax=mean.us.viewers.m),alpha=0.2,fill=col.l.grey)


g.2 = g.1 + theme (
  panel.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  #plot.caption=element_text(size=8, margin=margin(t=24),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  #axis.title.x = element_blank(margin=margin(t=-24)),
  #axis.title.y = element_blank()
  axis.title.y = element_text(size = 8),
  #axis.title.x = element_text(margin=margin(t=-20)),
  axis.title.x = element_text(margin=margin(0,0,-10,0), vjust=-5),

  axis.text.x = element_text(margin=margin(-10,0,-10,0)),
  axis.text.y = element_text(margin=margin(0,-10,0,-10)), # perfect
  
  # src:
  # https://rud.is/b/2016/06/16/your-data-vis-spidey-sense-the-need-for-a-robust-utility-belt/
  plot.subtitle=element_text(size=9.5, margin=margin(b=10)),
  plot.caption=element_text(size=7, margin=margin(t=13)),
  # margin around entire plot ('unit' with the sizes of the top, right, bottom, and left
  # margins)
  plot.margin=margin(10,10,10,10)

)

g.2 = g.2 +
  labs(
    x = NULL,
    y = NULL,
    title = "Does \"GDP\" impact \"Viewership for Emmy Winners\"",
    subtitle = "Mean of \"US Viewers Per Episode\", in Millions across the years (1) \nGDP Per Capita Change at current PPP $ (2)",
    caption = "Source: 1.Wikipedia & 2.Knoema.com"
  ) 
  
g.2 = g.2 +
  scale_x_date(
    breaks = seq(as.Date("2004/1/1"), as.Date("2016/1/1"), by = "years"),
    labels = c("2004","","06","","08","","10","","12","","14","","16")
  ) 

#g.2 = g.2 + scale_y_continuous(limits=c(0,20)) 


# Big bold line at y=0
# src
# http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-3-bar-plots.html
g.2 = g.2 + geom_hline(yintercept=0,size=1.2,colour="#535353") 

g.2


ggsave(filename="538.png",dpi=600)


# Inspirations
# http://www.r-bloggers.com/visualizing-survey-data-comparison-between-observations/
# http://www.r-bloggers.com/?s=ggplot+custom+icon
# https://rud.is/b/2016/06/19/a-call-to-armslist-data-analysis/
# https://rud.is/b/2016/06/16/your-data-vis-spidey-sense-the-need-for-a-robust-utility-belt/
# https://rud.is/b/page/2/

 