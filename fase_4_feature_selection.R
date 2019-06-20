library(tidyverse)
library(magrittr)
soja_df <- readRDS(file = "soja_df.rds") %>% ungroup()
materiais <- soja_df %>% group_by(material) %>% summarise(nobs=n()) %>% 
  arrange(desc(nobs)) %>% ungroup()

cutoff <- 0.05 * sum(materiais$nobs) # nenhuma obs atende a esse critério
hist(materiais$nobs)
boxplot(materiais$nobs)
mean(materiais$nobs)
median(materiais$nobs)
summary(materiais$nobs)

avg.obs <- mean(materiais$nobs)
materiais %<>% filter(nobs >= avg.obs)

ind <- soja_df$material %in% materiais$material
table(ind)

#####################
## Dataframe final ##
#####################
soja <- soja_df[ind, ]

library(Boruta)
set.seed(1)
feat.sel <- Boruta(productivity ~ ., data = soja, doTrace = 1)
saveRDS(feat.sel, "feat_sel.rds")
table(feat.sel$finalDecision)

library(caret)
modelLookup("rf")
grid <- expand.grid(.mtry=seq(6, 34, length.out = 5))

library(parallel)
library(doParallel)
cl <- makeCluster(detectCores() - 1) # 1 núcleo fica pro SO
registerDoParallel(cl)
ctrl <- trainControl(method = "cv", number = 5, 
                     selectionFunction = "oneSE", allowParallel = TRUE)
rf <- train(productivity ~ ., data = soja, method = "rf", 
            tuneGrid = grid, trControl = ctrl)

stopCluster(cl)
registerDoSEQ()

saveRDS(rf, "rf_soja4.rds")

rf <- read_rds("rf_soja4.rds")
prod.pred <- predict(rf)
plot(prod.pred ~ soja$productivity,
     xlab = "Produtividade medida, sacas/ha",
     ylab = "Produtividade prevista, sacas/ha",
     main = "Resultados do treinamento do modelo de produtividade de soja")
R2 <- cor(prod.pred, soja$productivity) ^ 2
abline(lm(prod.pred ~ soja$productivity), lwd = 2, col = "darkred")
text(20, 80, paste0("R2 = ", round(R2, 2)))
MAE <- function(actuals, pred) {
  mean(abs(actuals - pred))
}
dma <- MAE(soja$productivity, prod.pred)
dma
randomForest::varImpPlot(rf$finalModel)

# Importância das variáveis - modelo geral
library(iml)
y = soja$productivity
X = soja[-which(names(soja) == "productivity")]
mod = Predictor$new(rf, data = X, y = y)
# Compute feature importances as the performance drop in mean absolute error
# imp = FeatureImp$new(mod, loss = "mae")
imp <- readRDS("feature_imp_full.rds")
# Plot the results directly
plot(imp)
# saveRDS(imp, "feature_imp_full.rds")

# Importância das variáveis para cada semente
seeds <- length(unique(soja$material))
seeds
seeds <- unique(soja$material) %>% as.character()
head(seeds)

FeatImpPerSeed <- function(seed, plt = F) {
  if (!Xmisc::is.package.loaded("rpart")) {
    require(rpart)
  }
  data <- filter(soja, material == seed)
  data$material <- NULL
  tree <- rpart(productivity ~ ., data = data)
  y <- data$productivity
  X <- data[-which(names(data) == "productivity")]
  mod <- Predictor$new(tree, data = X, y = y)
  # Compute feature importances as the performance drop in mean absolute error 
  imp <- FeatureImp$new(mod, loss = "mae")
  # Plot the results directly 
  if (plt) plot(imp)
  return(imp$results)
}

imp <- lapply(seeds, FeatImpPerSeed)
# saveRDS(imp, "feature_imp_per_seed.rds")

for (i in seq_along(seeds)) {
  imp[[i]]$material <- seeds[i]
}
imp[[1]]
saveRDS(imp, "feature_imp_per_seed.rds")
imp <- readRDS("feature_imp_per_seed.rds")
var_imp_sementes <- rlist::list.stack(imp)
var_imp_sementes %<>% select(material, everything())
View(var_imp_sementes)
