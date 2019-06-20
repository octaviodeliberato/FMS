#######################
## FMS Fase 4 - Soja ##
#######################
library(tidyverse)
library(magrittr)

unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 
                      'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 
                      'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 
                      'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 
                      'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 
                      'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 
                      'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 
                      'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 
                      'ô'='o', 'õ'='o','ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 
                      'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')

# Dados 1 - clima (mm de precipitação)
clima_qrt <- read_csv("clima_qrt.csv")
View(clima_qrt)
clima.summ <- clima_qrt %>% group_by(harvest) %>% 
  summarise(rain.min = min(rain.mm), rain.max = max(rain.mm),
            rain.avg = mean(rain.mm), 
            rain.rng = max(rain.mm) - min(rain.mm))

# Dados 2 - produtividade, arquivo do Stelzer
crop <- read.csv("data-raw/crop.csv", encoding="UTF-8", stringsAsFactors = F)
str(crop)
summary(crop)

crop.df <- merge(crop, clima.summ, by = 'harvest')
crop.df %<>% filter(species == 'soy')
crop.df %<>% DataExplorer::drop_columns(c('id', 'species'))
crop.df$site %<>% str_replace_all(" ", "")
crop.df$site <- chartr(paste(names(unwanted_array), collapse=''),
                           paste(unwanted_array, collapse=''),
                           crop.df$site)
crop.df %<>% select(harvest, site, material, everything(), productivity)
crop.df$material %<>% str_remove_all(" ")
crop.df$material <- chartr(paste(names(unwanted_array), collapse=''),
                           paste(unwanted_array, collapse=''),
                           crop.df$material)
crop.df %<>% select(everything(), -productivity, productivity)

# Dados 3 - ciclo e época da soja
prod_soja <- readxl::read_excel("data-raw-2/prod_soja.xls",
                                col_types = c("numeric", "numeric", "text",
                                              "text", "text", "text", "text"))
colnames(prod_soja) <- c("prod", "ciclo", "material", "safra", "site", 
                         "epoca", "periodo")
prod_soja$site <- str_replace_all(prod_soja$site, "[^[:alnum:]]", "")
prod_soja$site <- str_replace_all(prod_soja$site, "[[:punct:]]", "")
prod_soja$site %<>% tolower()
prod_soja$periodo <- NULL
prod_soja$epoca <- substr(prod_soja$epoca, start = 1, stop = 1)
prod_soja$safra <- prod_soja$safra %>% gsub(pattern = "/", replacement = "-")
prod_soja <- prod_soja %>% mutate_if(is.character, factor)

# set.seed(300)
# imp <- mice::mice(prod_soja, m = 1, method = "rf")
# saveRDS(imp, "imp_prod_soja.rds")
imp <- readRDS("imp_prod_soja.rds")
prod_df <- mice::complete(imp)
prod_df$material %<>% tolower()
prod_df$material %<>% str_remove_all(" ")
prod_df$harvest <- prod_df$safra %>% substr(1, 4) %>% as.numeric()
prod_df$safra <- NULL
prod_df %<>% mutate(productivity = prod)
prod_df %<>% select(harvest, site, material, everything(), -prod)
prod_df$site <- chartr(paste(names(unwanted_array), collapse=''),
                           paste(unwanted_array, collapse=''),
                           prod_df$site)
prod_df$material <- chartr(paste(names(unwanted_array), collapse=''),
                           paste(unwanted_array, collapse=''),
                           prod_df$material)

# Dados 4 - solo
solo_soja <- read_csv("solo_soja.csv")
solo_soja$site <- chartr(paste(names(unwanted_array), collapse=''),
                       paste(unwanted_array, collapse=''),
                       solo_soja$site)
solo_soja$harvest <- solo_soja$safra %>% substr(1, 4)
solo_soja$safra <- NULL

# Dataset final
crop <- merge(crop.df, prod_df, by = c('harvest', 'site', 'material'))
crop$productivity.x <- NULL
crop %<>% mutate(productivity = productivity.y)
crop$productivity.y <- NULL
soja <- merge(crop, solo_soja, by = 'site')
soja %<>% DataExplorer::drop_columns(c('harvest.x', 'harvest.y'))
soja %<>% select(everything(), -productivity, productivity)                    
soja$rain.rng <- NULL
View(soja)
soja <- soja %>% group_by(site, material, epoca, depth) %>% summarise_all(mean)
soja %<>% DataExplorer::update_columns(c('site', 'material', 'depth'), factor)
saveRDS(soja, "soja_df.rds")
DataExplorer::create_report(soja)
