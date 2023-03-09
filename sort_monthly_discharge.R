# pull the DWS monthly data
# table starts with October
# DWS data source: https://www.dws.gov.za/Hydrology/Verified/hymain.aspx

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

x <- read_csv("G1H020_monthly.csv") # read in data from the monthly download from DWS

# PREALLOCATION
y <- array(NA, dim = (12*nrow(x))) # preallocate list for data
yea <- y # year
mon <- y # month
start <- y # the start year
end <- y # the end year, of the hydrologic year
NyearTotal <- array(NA, dim = c(nrow(x),2)) # list for the annual total

# SORT
# and 
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

ggplot(DWSmonth) +
     geom_line(aes(x=dt,y=Q)) +
     labs(x="Date",
          y=TeX('Discharge $(m^3/s)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.5) +
     theme(axis.text = element_text(face = "plain", size = 12))

annum <- data.frame(NyearTotal)
annum <- rename(annum,year=X1,volume=X2) # here, volume is in million cubic meters, year is OCTOBER hydrologic year
ggplot(annum) +
     geom_line(aes(x=year,y=volume))





