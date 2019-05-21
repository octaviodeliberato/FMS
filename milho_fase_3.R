library(dplyr)
library(magrittr)
library(readxl)
library(readr)
library(stringr)

crop_df <- read_csv("crop_df.csv")
crop_df %<>% filter(species == "corn")
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

DataExplorer::create_report(solo_milho)

milho <- merge(x = crop_df, y = solo_milho,
              by.x = c("site", "safra"), by.y = c("site", "safra"),
              all = FALSE)
milho <- milho %>% select(-productivity, everything(), productivity)
milho %>% glimpse()
milho$depth %<>% factor()

solo_milho_grp <- solo_milho %>% group_by(depth, safra)
solo_milho_grp$site <- NULL
solo_milho_summ <- solo_milho_grp %>% summarise_all(.funs = mean)
View(solo_milho_summ)
plot_bar(solo_milho_summ)
plot_density(filter(solo_milho_summ, depth == "00-20"))
plot_density(filter(solo_milho_summ, depth == "20-40"))