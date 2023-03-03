# Sort data
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(latex2exp)

x <- read_csv("G1H020.csv", col_names = FALSE)

month_trend <- x %>%
     mutate(mon=month(with_tz(as_datetime(X2), tzone = "Africa/Johannesburg"))) %>%
     rename(dt=X1,unix=X2,height=X3,discharge=X5) %>%
     select(dt,unix,mon,height,discharge) %>%
     group_by(mon) %>%
     summarize(monthAverage=mean(discharge, na.rm = TRUE)) %>%
     filter(is.na(mon)==FALSE)

ggplot(month_trend) +
     geom_col(aes(x=factor(mon),y=monthAverage)) +
     xlab("Month") +
     ylab(TeX('Mean Discharge $(m^3/s)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
