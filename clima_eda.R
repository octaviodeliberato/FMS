library(xts)
library(TSstudio)
clima_dia <- read.csv("~/FMS/data-raw/clima_dia.csv",
                      stringsAsFactors = F) # arquivo do Stelzer
View(clima_dia)
clima_dia$date <- as.Date(clima_dia$date)
round(colSums(is.na(clima_dia))/nrow(clima_dia) * 100, 0) # % NAs
fNames <- c("Date", "Amambai", "Anaurilandia", "Campo.Grande", "Dourados",
            "Ivinhema", "Maracaju", "Rio.Brilhante", "Sao.Grabiel.do.Oeste",
            "Sidrolandia")
names(clima_dia) <- fNames
clima_dia$Anaurilandia <- NULL # me livro dessa cidade, pois tem muitos NAs
clima_dia <- na.omit(clima_dia)
View(clima_dia)

clima.dia.xts <- xts(clima_dia[, -1], order.by = clima_dia$Date)
clima.qrt.xts <- apply.quarterly(clima.dia.xts, mean)
ts_plot(clima.qrt.xts, type = "single")
# ts_seasonal(clima.qrt.xts$Sidrolandia, type = "normal")
# ts_seasonal(clima.qrt.xts$Sidrolandia, type = "cycle")
# ts_seasonal(clima.qrt.xts$Sidrolandia, type = "box")
# ts_seasonal(clima.qrt.xts$Sidrolandia, type = "all")
# ts_heatmap(clima.qrt.xts$Sidrolandia)
# ts_surface(clima.qrt.xts$Sidrolandia)
# ts_polar(clima.qrt.xts$Sidrolandia)
periodicity(clima.qrt.xts)
frequency(clima.qrt.xts)

library(corrplot)
clima.cor <- cor(clima.qrt.xts)
corrplot.mixed(clima.cor)
ts_plot(clima.qrt.xts[, c("Rio.Brilhante", "Ivinhema")])

library(lubridate)
clima.qrt <- as.data.frame(clima.qrt.xts)
clima.qrt$Date <- as.Date(rownames(clima.qrt))
rownames(clima.qrt) <- NULL
clima.qrt$quarter <- quarter(clima.qrt$Date)
clima.qrt$harvest <- year(clima.qrt$Date)

library(dplyr)
clima.qrt <- clima.qrt %>% select(harvest, quarter, everything(), -Date)
clima.qrt$rain.mm <- rowMeans(clima.qrt[, 3:ncol(clima.qrt)])
clima.qrt <- clima.qrt %>% select(harvest, quarter, rain.mm)

library(Hmisc)
rain.level <- cut2(clima.qrt$rain.mm, g = 4)
levels(rain.level) <- c("norain", "low", "med", "high")
clima.qrt$rain.level <- as.character(rain.level)
clima.qrt$rain <- paste0('q', clima.qrt$quarter, '.', clima.qrt$rain.level)
clima.qrt$rain <- factor(clima.qrt$rain, levels = c(
  'q1.norain', 'q1.low', 'q1.med', 'q1.high',
  'q2.norain', 'q2.low', 'q2.med', 'q2.high',
  'q3.norain', 'q3.low', 'q3.med', 'q3.high',
  'q4.norain', 'q4.low', 'q4.med', 'q4.high'))
library(caret)
dummies <- dummyVars(harvest ~ rain, data = clima.qrt)
clima.qrt.dum <- predict(dummies, newdata = clima.qrt)
clima.qrt.dum <- as.data.frame(clima.qrt.dum)
clima.qrt.final <- cbind(harvest = clima.qrt$harvest, clima.qrt.dum) %>% 
  group_by(harvest) %>% summarise_if(is.numeric, max)

library(readr)
write_csv(clima.qrt.final, "clima.csv")
