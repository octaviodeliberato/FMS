library(dplyr)
library(magrittr)
library(readxl)
library(readr)
library(stringr)

# Preparação dos dados de milho
crop_df <- read_csv("crop_df.csv")
crop_df %<>% filter(species == "corn")
crop_df$species <- NULL

crop <- read_csv("data-raw/crop.csv")
crop %<>% filter(species == "corn")
crop$id <- NULL
crop %<>% select(material, site, safra=harvest, productivity)

crop_df %<>% arrange(desc(productivity))
crop %<>% arrange(desc(productivity))
identical(crop_df$productivity, crop$productivity)
crop_df$safra <- crop$safra
rm(crop)
crop_df %<>% arrange(c(safra))
crop_df %<>% select(material, safra, everything(), productivity)
hist(crop_df$productivity)
rug(crop_df$productivity)

# Agora, vem a etapa de dataprep dos dados de solo enviados pela FMS
solo_milho <- read_excel("data-raw-2/solo_fms.xlsx", na = "-")
solo_milho %>% glimpse()
solo_milho$SATPOR_K <- as.numeric(solo_milho$SATPOR_K)
solo_milho <- solo_milho %>% filter(Cultura == "Milho")
solo_milho$Safra <- solo_milho$Safra %>% 
  str_replace_all(pattern = "/", replacement = "-")
solo_milho$Local <- solo_milho$Local %>% 
  str_replace_all("[^[:alnum:]]", "")
solo_milho$Local <- solo_milho$Local %>% 
  str_replace_all("[[:punct:]]", "")
solo_milho$Local <- solo_milho$Local %>% tolower()
solo_milho$Cultura <- NULL # sei que é somente milho
solo_milho <- solo_milho %>% rename(site = Local, 
                                  safra = Safra, 
                                  depth = Profundidade)
solo_milho %>% glimpse()
names(solo_milho)[13] <- "H_Al_CMOLCDM3"
names(solo_milho)[29] <- "ARGILA"
summary(solo_milho)

round(colMeans(is.na(solo_milho)) * 100)

library(mice)
imp <- mice(solo_milho, m = 1, method = "pmm", printFlag = F)
imp$imp$PH_CACL2
solo_milho <- mice::complete(imp)

round(colMeans(is.na(solo_milho)) * 100)
rio::export(x = solo_milho, file = "solo_milho.csv")

milho <- merge(x = crop_df, y = solo_milho,
              by.x = c("site", "safra"), by.y = c("site", "safra"),
              all = FALSE)
milho %<>% select(site, material, depth, everything())
milho %<>% select(-productivity, everything(), productivity)
milho$safra <- NULL
milho %<>% mutate_if(is.character, factor)
milho %<>% mutate_at(vars(matches("rain_")), factor)
milho %>% glimpse()
rio::export(x = milho, file = "milho_final.csv")

library(caret)
## Not run (this may take some time to complete):
# Random Forest
ctrl <- trainControl(method = "cv", selectionFunction = 'oneSE')
grid <- expand.grid(.mtry = c(6, 10, 14, 18, 22), .splitrule = c("extratrees"),
                    .min.node.size = c(1, 3, 5, 7))
milho$material <- as.factor(milho$material %>% substr(1, 1))
set.seed(300)

m.rf <- train(productivity ~ ., data = milho, method = "ranger",
              metric = "RMSE", trControl = ctrl, tuneGrid = grid)


m.rf
yhat.rf <- predict(m.rf, milho)
plot(milho$productivity, yhat.rf)
abline(lm(yhat.rf ~ milho$productivity), col = 'darkred', lwd = 2)
cor(milho$productivity, yhat.rf)^2
saveRDS(m.rf, 'milho_final.rds')
## End(Not run)

# Comparação com a fase anterior
m.rf <- readRDS('m_rf.rds')
m.rf
crop_df <- read.csv("crop_df.csv", encoding = "UTF-8")
crop_df %<>% filter(species == "corn")
crop_df$material <- substr(crop_df$material, 1, 1)
crop_df[, 1:14] <- crop_df[, 1:14] %>% lapply(as.factor)
yhat.rf <- predict(m.rf, crop_df)
plot(crop_df$productivity, yhat.rf,
     main = "Random Forest",
     xlab = "Produtividade, sacas / ha",
     ylab = "Produtividade calculada, sacas / ha")
abline(lm(yhat.rf ~ crop_df$productivity), col = 'darkred', lwd = 2)
r2.old <- cor(crop_df$productivity, yhat.rf)^2
r2.old
# text(20, 80, paste0("R2 = ", round(r2.old, 3)))