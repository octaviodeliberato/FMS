crop <- read.csv("~/FMS/data-raw/crop.csv", encoding="UTF-8", 
                 stringsAsFactors = F) # arquivo do Stelzer
library(tidyverse)
round(table(crop$site)/nrow(crop)*100, 1) # posso me livrar de Anaurilândia
# crop <- crop %>% filter(site != "anaurilândia")
length(unique(crop$material))

crop$harvest <- factor(crop$harvest)
crop$id <- NULL

boxplot(productivity ~ harvest, data = subset(crop, species == 'soy'),
        main = 'Produtividade da Soja')
boxplot(productivity ~ harvest, data = subset(crop, species == 'corn'),
        main = 'Produtividade do Milho')
table(crop$harvest)/nrow(crop)*100
cumsum(table(crop$harvest)/nrow(crop)*100) # até 2015 para treinamento, resto para teste

g <- ggplot(data = subset(crop, species == 'soy'), 
            aes(x = harvest, y = productivity)) + geom_boxplot() + 
  facet_wrap(~ site, ncol = 4) + labs(title = 'Produtividade da Soja')
g
g1 <- ggplot(data = subset(crop, species == 'corn'), 
            aes(x = harvest, y = productivity)) + geom_boxplot() + 
  facet_wrap(~ site, ncol = 4) + labs(title = 'Produtividade do Milho')
g1
crop.sum <- crop %>% group_by(species, site) %>% 
  summarise(avgProd = mean(productivity), sdProd = sd(productivity)) %>%
  arrange(desc(avgProd))
crop.sum
g2 <- ggplot(crop, aes(x = productivity, fill = species)) +
  geom_density(alpha = 0.5)
g2
summary(crop.sum$avgProd)
g3 <- ggplot(data = crop.sum, aes(x = site, y = avgProd, color = species)) +
  geom_point(size = 2)
g3

clima <- read_csv("clima.csv")
clima$harvest <- factor(clima$harvest)
crop.df <- merge(crop, clima, by = 'harvest')

crop.df$material <- str_replace_all(crop.df$material, "[^[:alnum:]]", "")
seeds <- substr(crop.df$material, 1, 2)
tbl.seeds <- table(seeds)
sum(tbl.seeds/sum(tbl.seeds)*100) # check sum=100
barplot(tbl.seeds/sum(tbl.seeds)*100) # types of materials
crop.df$site <- str_replace_all(crop.df$site, "[^[:alnum:]]", "")
crop.df$site <- str_replace_all(crop.df$site, "[[:punct:]]", "")
crop.df$material <- seeds
crop.df <- crop.df %>% select(material, site, species, contains('rain.q'), 
                              productivity)

write_csv(crop.df, 'crop.df.csv')
