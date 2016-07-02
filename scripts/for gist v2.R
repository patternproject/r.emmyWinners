#### Load Data ####
df.winners = read.csv(
  "winnersWithMean.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")#,
  # sep="\t"
)

# dput for df.winners
# at the end of the code

#### Manipulate Data ####

# making genre a factor
df.winners$genre = as.factor(df.winners$genre)

# for plotting setting Year as Date Type (else treated as integer)
df.winners$year2 = as.Date(as.character(df.winners$year),format="%Y")

## color codes ####
col.grey = "#707070"
col.teal = "#368C8C"
col.blue = "#4682B4"
col.mid.green = "#98EFC1"
col.lig.green = "#B8FFD1"
col.dark.red = "darkred"


#### Visulaize Data ####

## g.1 ##

g.1 = ggplot(data=df.winners) + theme_minimal()

g.1 = g.1 + geom_line(data = filter(df.winners, grepl('COMEDY', genre)),
                      aes(x = year2, y = mean.us.viewers.m),alpha=0.8,size=0.5,col=col.lig.green)

g.1 = g.1 + geom_point(data = filter(df.winners, grepl('COMEDY', genre)),
                       aes(x = year2, y = mean.us.viewers.m),alpha=0.6,size=2,col=col.blue)

g.1 = g.1 + geom_line(data = filter(df.winners, grepl('DRAMA', genre)),
                      aes(x = year2, y = mean.us.viewers.m),alpha=0.8,size=0.5,col=col.lig.green)

g.1 = g.1 + geom_point(data = filter(df.winners, grepl('DRAMA', genre)),
                       aes(x = year2, y = mean.us.viewers.m),alpha=0.4,size=2,col=col.dark.red)

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
  
  # src:
  # https://rud.is/b/2016/06/16/your-data-vis-spidey-sense-the-need-for-a-robust-utility-belt/
  plot.subtitle=element_text(size=9.5, margin=margin(b=10)),
  
  # SOLUTION for caption overlap (changed -10 to 10)
  
  plot.caption=element_text(size=7, margin=margin(t=10)),
  # margin around entire plot ('unit' with the sizes of the top, right, bottom, and left
  # margins)
  plot.margin=margin(10,10,10,10)
  
)

g.2 = g.2 +
  labs(
    x = NULL,
    y = NULL,
    title = "How viewership varies for Emmy Winners",
    subtitle = "Mean of \"US Viewers Per Episode\", in Millions across the years (Wikipedia)",
    caption = "Source: Wikipedia"
  ) 

# SOLUTION for X is to manually set the scale limits when using expand

g.2 = g.2 +
  scale_x_date(
    breaks = seq(as.Date("2004/1/1"), as.Date("2016/1/1"), by = "years"),
    labels = c("2004","","06","","08","","10","","12","","14","","16"),
    expand = c(0,0), limits=as.Date(c("2004-01-01", "2016-01-21"))
  ) 

# SOLUTION for Y is to manually set larger limits as well

g.2 = g.2 + scale_y_continuous(limits = c(0,21.5),
                               expand = c(0,0))

# label for genre DRAMA
g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2004/2/1"), y=10, label="DRAMA Genre"),col=col.dark.red,alpha=0.4,fontface = "bold")

# label for genre COMEDY
g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2004/2/1"), y=5, label="COMEDY Genre"),col=col.blue,alpha=0.6,fontface = "bold")


# label for cross over point 1
g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2005/12/1"), y=19, label="Cross-over \n Point 1"))

# highlight circle 1 for cross over point 1
g.2 = g.2 + geom_point(data=data.frame(),aes(x=as.Date("2005/7/1"), y=19),alpha=0.1,size=8,col=col.grey)

# label for cross over point 2
g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2007/9/1"), y=4.3, label="Cross-over \n Point 2"))

# highlight circle 1 for cross over point 2
g.2 = g.2 + geom_point(data=data.frame(),aes(x=as.Date("2007/12/1"), y=5.7),alpha=0.1,size=8,col=col.grey)

# label for cross over point 3
g.2 = g.2 + geom_text(data=data.frame(), hjust=0, size=2,
                      aes(x=as.Date("2014/1/1"), y=5.3, label="Cross-over \n Point 3"))

# highlight circle 1 for cross over point 3
g.2 = g.2 + geom_point(data=data.frame(),aes(x=as.Date("2015/1/1"), y=5.7),alpha=0.1,size=8,col=col.grey)

# Big bold line at y=0
# src
# http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-3-bar-plots.html
g.2 = g.2 + geom_hline(yintercept=0,size=1.2,colour="#535353") 

# I'd proably also add:

g.2 = g.2 + geom_label(data=data.frame(), hjust=0, size=2, 
                       label.size=0, label.padding=unit(0, "null"),
                       aes(x=as.Date("2004/01/01"), y=21, label="Viewers per ep (millions)"))

# (i know said "label" is in the subtitle but in eye tracking studies, most 
# ppl look at the chart first)

g.2 = g.2 + theme(axis.text.x=element_text(hjust=c(0, rep(0.5, 11), 1),
                                           margin=margin(t=-10)))
g.2 = g.2 + theme(axis.text.y=element_text(vjust=c(0, rep(0.5, 4)),
                                           margin=margin(r=-10)))

g.2


ggsave(filename="538.png",dpi=600)
