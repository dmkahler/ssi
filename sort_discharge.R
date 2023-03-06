# Sort data
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

x <- read_csv("G1H020.csv", col_names = FALSE)

month_trend <- x %>%
     mutate(mon=month(with_tz(as_datetime(X2), tzone = "Africa/Johannesburg"))) %>%
     rename(dt=X1,unix=X2,height=X3,discharge=X5) %>%
     select(dt,unix,mon,height,discharge) %>%
     group_by(mon) %>%
     summarize(monthAverage=mean(discharge, na.rm = TRUE),monthSTD=sd(discharge, na.rm = TRUE)) %>%
     filter(is.na(mon)==FALSE)
month_trend$label <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_trend$label <- factor(month_trend$label, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(month_trend) +
     geom_col(aes(x=label,y=monthAverage)) +
     #geom_errorbar(aes(x=mon,ymin=monthAverage-monthSTD,ymax=monthAverage+monthSTD), width = 0.3) + # Standard deviation is very large
     xlab("Month") +
     ylab(TeX('Mean Discharge $(m^3/s)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

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




