# pull the DWS monthly data
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

x <- read_csv("G1H020_monthly.csv")
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

ggplot(DWSmonth) +
     geom_line(aes(x=dt,y=Q)) +
     labs(x="Date",
          y=TeX('Discharge $(m^3/s)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.5) +
     theme(axis.text = element_text(face = "plain", size = 12))


