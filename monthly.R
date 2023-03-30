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
     select(year,month,dt,Q) %>%
     mutate(measurement="monthly")
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
          y[j,7] <- hyd.mo(with_tz(as_datetime(x$X2[i]), tzone = "Africa/Johannesburg"), "S")
          y[j,8] <- hyd.yr(with_tz(as_datetime(x$X2[i]), tzone = "Africa/Johannesburg"), "S")
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
     mutate(year=floor(yearmo/100),mon=yearmo-100*floor(yearmo/100)) %>%
     mutate(month=yearmo-100*year) %>%
     #mutate(decYear=yea+(mon-0.5)/12) %>%
     mutate(dt=ymd(paste0(year,"-",month,"-","15"))) %>%
     rename(Q=monthAverage) %>%
     select(year,month,dt,Q) %>%
     mutate(measurement="primary")
rm(x,y,z)

##################################### Monthly data from internal DWS server
# 

x <- read_csv("G1H020dws.csv")

DWSint <- x %>%
     mutate(dn=dmy_hm(date)) %>%
     mutate(y=year(dn),m=month(dn)) %>%
     mutate(yearmo=100*y+m) %>%
     group_by(yearmo) %>%
     summarize(Q=mean(`Discharge(cumecs)`)) %>%
     mutate(year=floor(yearmo/100),mon=yearmo-100*floor(yearmo/100)) %>%
     mutate(month=yearmo-100*year) %>%
     mutate(dt=ymd(paste0(year,"-",month,"-","15"))) %>%
     select(year,month,dt,Q) %>%
     mutate(measurement="internal")
rm(x)


long <- rbind(DWSraw,DWSmonth,DWSint)
long <- rename(long, Source=measurement)
ts <- ggplot(long) +
     geom_line(aes(x=dt,y=Q,color=Source)) +
     xlab("Date (monthly)") +
     ylab(TeX('Mean Discharge $(m^3/s)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14)) +
     theme(legend.key = element_blank()) +
     theme(legend.background = element_rect(fill = "white", colour = "black"))
ggsave("timeseries.eps", ts, device = "eps", dpi = 300)

wide <- long %>%
     pivot_wider(names_from = "Source",values_from = "Q")
mon_pri <- ggplot(wide) +
     geom_point(aes(x=primary,y=monthly)) +
     xlim(c(0,300)) +
     ylim(c(1,300)) +
     labs(title = (TeX('Monthly Mean Discharge $(m^3/s)$ at G1H020')), 
          x = "Website Primary",
          y = "Website Monthly") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
gmonpri <- ggplotGrob(mon_pri)
mon_int <- ggplot(wide) +
     geom_point(aes(x=internal,y=monthly)) +
     xlim(c(0,300)) +
     ylim(c(1,300)) +
     labs(x = "Internal Server",
          y = "Website Monthly") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
gmonint <- ggplotGrob(mon_int)
grid::grid.newpage()
comparison <- grid::grid.draw(cbind(gmonpri,gmonint))
ggsave("source_comparison.eps", comparison, device = "eps", dpi = 300)

# building comparison data frames.
y1 <- min(c(min(DWSmonth$year),min(DWSraw$year),min(DWSint$year)))
y2 <- max(c(max(DWSmonth$year),max(DWSraw$year),max(DWSint$year)))




