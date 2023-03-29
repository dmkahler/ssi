# pull the DWS monthly data
# compare with the averaged raw data from sort_discharge.R the DWS internal data pull
# table starts with October

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(latex2exp)
library(devtools)
install_github("LimpopoLab/hydrostats", force = TRUE)
library(hydrostats)
library(SPEI) # from Mxolisi & Ndumiso, https://cran.r-project.org/web/packages/SPEI/index.html

##################################### Monthly data from DWS website
# Data from: https://www.dws.gov.za/Hydrology/Verified/HyDataSets.aspx?Station=G1H020&SiteDesc=RIV

x <- read_csv("G1H020monthly.csv")
y <- array(NA, dim = (12*nrow(x)))
yea <- y
mon <- y
start <- y
end <- y
NyearTotal <- array(NA, dim = c(nrow(x),2))
for (i in 1:nrow(x)) {
     for (j in 1:12){
          pos <- j+(i-1)*12
          if (j<=3) {
               yea[pos] <- x$start[i]
               mon[pos] <- 9 + j
          } else {
               yea[pos] <- x$end[i]
               mon[pos] <- j - 3
          }
          y[pos] <- x[i,2+j][[1]]
          start[pos] <- as.numeric(ymd(paste0(yea[pos],"-",mon[pos],"-01")))
          if (mon[pos]==12) {
               end[pos] <- as.numeric(ymd(paste0(as.numeric(yea[pos]+1),"-01-01")))
          } else {
               end[pos] <- as.numeric(ymd(paste0(yea[pos],"-",(mon[pos]+1),"-01")))
          }
     }
     NyearTotal[i,1] <- x$end[i]
     NyearTotal[i,2] <- x$Total[i]
}

z <- data.frame(yea,mon,y,start,end)
DWSmonth <- z %>%
     mutate(dt=ymd(paste0(yea,"-",mon,"-","15"))) %>%
     rename(year=yea,month=mon) %>%
     mutate(volume=y*1e6) %>%
     mutate(days=end-start) %>%
     mutate(Q=volume/(days*24*3600)) %>% # this will be average m^3/s
     select(year,month,dt,volume,Q)
rm(x,y,z,NyearTotal,end,mon,pos,start,yea)

##################################### Averaged monthly data from "primary data" on DWS website
# Data from: https://www.dws.gov.za/Hydrology/Verified/HyDataSets.aspx?Station=G1H020&SiteDesc=RIV

x <- read_csv("G1H020scrape.csv", col_names = FALSE)
y <- array(NA, dim = c(nrow(x),ncol(x)+2))
j <- 1
for (i in 1:nrow(x)) {
     if (is.na(x$X2[i])==FALSE) {
          y[j,1] <- x$X1[i] # in SAST, but says UTC
          y[j,2] <- x$X2[i] # UTC
          y[j,3] <- x$X3[i]
          y[j,4] <- x$X4[i]
          y[j,5] <- x$X5[i]
          y[j,6] <- x$X6[i]
          y[j,7] <- hyd.mo(with_tz(as_datetime(x$X2[i]), tzone = "Africa/Johannesburg"), h="S")
          y[j,8] <- hyd.yr(with_tz(as_datetime(x$X2[i]), tzone = "Africa/Johannesburg"), h="S")
          j <- j + 1
     }
}
y <- y[1:(j-1),1:8]
y <- data.frame(y)

z <- y %>%
     mutate(month=month(with_tz(as_datetime(X2), tzone = "Africa/Johannesburg"))) %>%
     mutate(year=year(with_tz(as_datetime(X2), tzone = "Africa/Johannesburg"))) %>%
     rename(dt=X2,height=X3,discharge=X5,hydroMonth=X7,hydroYear=X8) %>%
     select(dt,year,month,height,hydroYear,hydroMonth,height,discharge)

DWSraw <- z %>%
     mutate(yearmo=100*year+month) %>%
     group_by(yearmo) %>%
     summarize(monthAverage=mean(discharge, na.rm = TRUE),monthSTD=sd(discharge, na.rm = TRUE)) %>%
     mutate(yea=floor(yearmo/100),mon=yearmo-100*floor(yearmo/100)) %>%
     mutate(decYear=yea+(mon-0.5)/12) %>%
     mutate(dt=ymd(paste0(yea,"-",mon,"-","15")))

# building comparison data frames.
y1 <- min(c(min(DWSmonth$year),min(DWSraw$yea)))
y2 <- max(c(max(DWSmonth$year),max(DWSraw$yea)))
monDat <- array(NA, dim = c((y2-y1+1),12))
rawDat <- monDat

for (i in 1:nrow(DWSmonth)) {
     monDat[(DWSmonth$year[i]-y1+1),DWSmonth$month[i]] <- DWSmonth$Q[i]
}
monDat <- data.frame(monDat)
monDat$year <- c(y1:y2)
monDat2 <- monDat %>%
     rename(Jan=X1,Feb=X2,Mar=X3,Apr=X4,May=X5,Jun=X6,Jul=X7,Aug=X8,Sep=X9,Oct=X10,Nov=X11,Dec=X12) %>%
     pivot_longer(cols = c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec),names_to = "month",values_to = "Monthly")
for (i in 1:nrow(DWSraw)) {
     rawDat[(DWSraw$yea[i]-y1+1),DWSraw$mon[i]] <- DWSraw$monthAverage[i]
}
rawDat <- data.frame(rawDat)
rawDat$year <- c(y1:y2)
rawDat2 <- rawDat %>%
     rename(Jan=X1,Feb=X2,Mar=X3,Apr=X4,May=X5,Jun=X6,Jul=X7,Aug=X8,Sep=X9,Oct=X10,Nov=X11,Dec=X12) %>%
     pivot_longer(cols = c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec),names_to = "month",values_to = "Raw")
monDat2$Raw <- rawDat2$Raw

ggplot(monDat2) +
     geom_point(aes(x=Raw,y=Monthly)) +
     labs(title="Comparison of monthly average discharge at G1H020", 
          x=TeX('Discharge $(m^3/s)$ from raw data'), 
          y=TeX('Discharge $(m^3/s)$ from monthly data')) +
     xlim(c(0,150)) +
     ylim(c(0,150)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

monDat3 <- monDat2 %>%
     mutate(dt=ymd(paste0(year,"-",month,"-","15"))) %>%
     pivot_longer(cols = c(Monthly,Raw),names_to = "Source",values_to = "Discharge")
ggplot(monDat3) +
     geom_line(aes(x=dt,y=Discharge,color=Source)) +
     labs(title="Time series comparison of monthly average discharge at G1H020",
          x="Date",
          y=TeX('Discharge $(m^3/s)$ from monthly data')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(legend.position = "right") +
     theme(legend.background = element_rect(fill = "white", colour = "black")) +
     theme(legend.key = element_rect(fill = "white")) +
     theme(axis.text = element_text(face = "plain", size = 12))



