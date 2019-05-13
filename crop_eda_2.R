library(tidyverse)
library(magrittr)
library(mice)

prod_soja <- read_excel("data-raw-2/prod_soja.xls",
                        col_types = c("numeric", "numeric", "text",
                                      "text", "text", "text", "text"))
colnames(prod_soja) <- c("prod", "ciclo", "material", "safra", "site", 
                         "epoca", "periodo")
prod_soja$material <- str_replace_all(prod_soja$material, "[^[:alnum:]]", "")
seeds <- substr(prod_soja$material, 1, 2)
tbl.seeds <- table(seeds)
sum(tbl.seeds/sum(tbl.seeds)*100) # check sum=100
barplot(tbl.seeds/sum(tbl.seeds)*100) # types of materials
prod_soja$site <- str_replace_all(prod_soja$site, "[^[:alnum:]]", "")
prod_soja$site <- str_replace_all(prod_soja$site, "[[:punct:]]", "")
prod_soja$site %<>% tolower(.)
prod_soja$material <- tolower(seeds)
prod_soja$periodo <- NULL
prod_soja$epoca <- substr(prod_soja$epoca, start = 1, stop = 1)
prod_soja$safra <- prod_soja$safra %>% gsub(pattern = "/", replacement = "-")
prod_soja <- prod_soja %>% mutate_if(is.character, factor)
imp <- mice(prod_soja, m = 1, method = "rf")
prod_df <- complete(imp)


crop_df <- read_csv("crop_df.csv")
crop_df %<>% filter(species == "soy")
crop_df$ciclo <- prod_df$ciclo
crop_df$safra <- prod_df$safra
crop_df$epoca <- prod_df$epoca
crop_df %<>% select(material:species, ciclo, safra, epoca, 
                    everything(), -species)
rio::export(crop_df, file = "soy_df.csv") #  somente soja
