library(tidyverse)
library(Hmisc)
library(MLmetrics)
crop.df <- read_csv("~/FMS/crop.df.csv")
crop.df$material <- substr(crop.df$material, 1, 1)
barplot(table(crop.df$material))
write_csv(crop.df, 'crop.kn.csv')

crop.df[, 1:19] <- lapply(crop.df[, 1:19], as.factor)
library(caret)
# Random Forest
ctrl <- trainControl(method = "cv", selectionFunction = 'oneSE')
grid <- expand.grid(.mtry = c(6, 8, 10))
set.seed(300)
dim(crop.df)
nzv <- nearZeroVar(crop.df)
filt.crop <- crop.df[, -nzv]
dim(filt.crop)
m.rf <- train(productivity ~ ., data = filt.crop, method = "rf",
              metric = "RMSE", trControl = ctrl, tuneGrid = grid)
m.rf
yhat.rf <- predict(m.rf, filt.crop)
plot(filt.crop$productivity, yhat.rf)
abline(lm(yhat.rf ~ filt.crop$productivity), col = 'darkred', lwd = 2)
R2(filt.crop$productivity, yhat.rf)
saveRDS(m.rf, 'm.rf.rds')

# MARS
library(earth)
set.seed(1)
earth.fit <- earth(productivity ~ ., data = filt.crop,
                   pmethod = "cv",
                   nfold = 10,
                   ncross = 1,
                   degree = 2,
                   minspan = -1,
                   glm=list(family=gaussian)
                  )
summary(earth.fit)
plotmo(earth.fit)
plot(earth.fit)
evimp(earth.fit)
yhat.earth <- predict(earth.fit, filt.crop)
plot(filt.crop$productivity, yhat.earth)
abline(lm(yhat.earth ~ filt.crop$productivity), col = 'darkred', lwd = 2)
R2(filt.crop$productivity, yhat.rf)
saveRDS(earth.fit, 'earth.fit.rds')
