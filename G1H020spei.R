# Calculate SPEI stats - code from Ndumiso

library(tidyverse)
library(lubridate)
library(SPEI)
library(writexl)

x <- read_csv("G1H020dws.csv")

G1H020 <- G1H020[1:667,]

G1H020.1 <- separate(G1H020, "date", c('day', "month", 'year'))

G1H020 <- G1H020.1[, c(3,2,4)]

write_xlsx(G1H020,"C:\\Users\\MasilelaNd\\Documents\\Drought Projects\\River Flow\\G1H020\\G1H020.xlsx")

is.na(G1H020$`Discharge(cumecs)`)


G1H020.ts <- ts(G1H020[,-c(1,2)], start = c(1966, 4), end = (2021), frequency = 12)
plot.ts(G1H020.ts, ylab = 'Discharge(cumecs)', main = 'Station G1H020 Time series')


G1H020.SPI3 <- spi(G1H020.ts, 3, distribution = "Gamma")
plot.spei(G1H020.SPI3, main = 'Station G1H020 SSI3 Gamma')

G1H020.SPI6 <- spi(G1H020.ts, 6, distribution = "Gamma")
plot.spei(G1H020.SPI6, main = 'Station G1H020 SSI6 Gamma')

G1H020.SPI12 <- spi(G1H020.ts, 12, distribution = "Gamma")
plot.spei(G1H020.SPI12, main = 'Station G1H020 SSI12 Gamma')

G1H020.SPI24 <- spi(G1H020.ts, 24, distribution = "Gamma")
plot.spei(G1H020.SPI24, main = 'Station G1H020 SSI24 Gamma')


G1H020.SPI3.P3 <- spi(G1H020.ts, 3, distribution = "PearsonIII")
plot.spei(G1H020.SPI3.P3, main = 'Station G1H020 SSI3 PearsonIII')

G1H020.SPI6.P3 <- spi(G1H020.ts, 6, distribution = "PearsonIII")
plot.spei(G1H020.SPI6.P3, main = 'Station G1H020 SSI6 PearsonIII')

G1H020.SPI12.P3 <- spi(G1H020.ts, 12, distribution = "PearsonIII")
plot.spei(G1H020.SPI12.P3, main = 'Station G1H020 SSI12 PearsonIII')

G1H020.SPI24.P3 <- spi(G1H020.ts, 24, distribution = "PearsonIII")
plot.spei(G1H020.SPI24.P3, main = 'Station G1H020 SSI24 PearsonIII')



G1H020.SPI3.Lg <- spi(G1H020.ts, 3, distribution = "log-Logistic")
plot.spei(G1H020.SPI3.Lg, main = 'Station G1H020 SSI3 log-Logistic')

G1H020.SPI6.Lg <- spi(G1H020.ts, 6, distribution = "log-Logistic")
plot.spei(G1H020.SPI6.Lg, main = 'Station G1H020 SSI6 log-Logistic')

G1H020.SPI12.Lg <- spi(G1H020.ts, 12, distribution = "log-Logistic")
plot.spei(G1H020.SPI12.Lg, main = 'Station G1H020 SSI12 log-Logistic')

G1H020.SPI24.Lg <- spi(G1H020.ts, 24, distribution = "log-Logistic")
plot.spei(G1H020.SPI24.Lg, main = 'Station G1H020 SSI24 log-Logistic')