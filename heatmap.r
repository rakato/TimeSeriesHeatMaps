
library(quantmod)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(plotly)

getSymbols("VLO",src="yahoo")

dat<-data.frame(date=index(VLO),VLO)

#setting year, month, weekday
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
dat$weekday = as.POSIXlt(dat$date)$wday

dat$weekdayf<-factor(dat$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)

dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)
dat$week <- as.numeric(format(dat$date,"%W"))
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

# Now for the plot
z<- ggplot(dat, aes(monthweek, weekdayf, fill = VLO.Close)) +
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="red", high="yellow")
   
z

ggplotly(z)



