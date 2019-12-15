load("C:/James Laptop/M.S. Applied Statistics/MAT 8480 Data Mining and Analytics - Dr. Zhang/Project/Phase 2/tour_clean_bin.RData")
library(caTools)
library(caret)
library(pROC)
library(randomForest)
library(dplyr)

minor <- unname(summary(tour.imp$Book_12Mo[split])[2])

# Double loop
m <- seq(1, 31, by=1)
tree <- seq(101, 2001, by=50)

fscore.table <- setNames(data.frame(matrix(ncol = length(tree), nrow = length(m))), tree)
auc.table <- setNames(data.frame(matrix(ncol = length(tree), nrow = length(m))), tree)
  
for(i in 1:length(m)){
  for(j in 1:length(tree)){
  set.seed(40703)
  rf2 <- randomForest(Book_12Mo ~., data=tour.imp[split,],
                      ntree = tree[j], 
                      strata= tour.imp$Book_12Mo[split], 
                      sampsize=c(minor,minor),
                      importance = FALSE,
                      mtry=m[i])
  rf2.prob <- predict(rf2, newdata=tour.imp[split.valid,],type="prob")[,2]
  rocCurve.rf2 <- roc(tour.imp[split.valid,]$Book_12Mo, rf2.prob, quiet=TRUE)
  rf2Thresh <- coords(rocCurve.rf2, x = "best", best.method = "closest.topleft", transpose = FALSE)
  rf2.class <- as.factor(ifelse(rf2.prob >= rf2Thresh$threshold, 1, 0))
  fscore.table[i,j] <- confusionMatrix(table(rf2.class,tour.imp[split.valid,]$Book_12Mo),
                                   positive = "1")$byClass["F1"]
  auc.table[i,j] <- auc(rocCurve.rf2)
}
}

View(fscore.table)
max(fscore.table, na.rm=T)

View(auc.table)
max(auc.table, na.rm=T)

max.indx <- sapply(fscore.table, which.max)
max.val.vec <- numeric()
for (i in 1:length(tree)){
  max.val.vec[i] <- fscore.table[max.indx[i], i]
}

max.val <- max(max.val.vec, na.rm=T)
max.val

max.val.location <- which.max(max.val.vec)
tree.best <- max.indx[max.val.location]
tree.best

m.best <- unname(tree.best)
m.best

# Final model
tour.imp <- tour.imp %>%
  mutate(Age_num.NA = as.factor(0),
         State.binned.NA = as.factor(0),
         Domestic_Arrival_Time.NA = as.factor(0),
         TourCode.binned.NA = as.factor(0))

set.seed(40703)
RF <- randomForest(Book_12Mo ~., data=tour.imp[split,],
                   ntree = 1301, 
                   strata= tour.imp$Book_12Mo[split], 
                   sampsize=c(minor,minor),
                   importance = FALSE,
                   mtry=5)

# F1 and misclassification rate
RF.class <- predict(RF, newdata=tour.imp[split.valid,], type="response")
fscore <- confusionMatrix(table(RF.class, tour.imp[split.valid,]$Book_12Mo),
                          positive = "1")$byClass["F1"]  
fscore
misc <- 1 - confusionMatrix(table(RF.class, tour.imp[split.valid,]$Book_12Mo),
                    positive = "1")$overall["Accuracy"]
unname(misc)


RF.prob <- predict(RF, newdata=tour.imp[split.valid,],type="prob")[,2]
rocCurve.RF <- roc(tour.imp[split.valid,]$Book_12Mo, RF.prob, quiet=TRUE)
RFThresh <- coords(rocCurve.RF, x = "best", best.method = "closest.topleft", transpose = FALSE)
RF.class <- as.factor(ifelse(RF.prob >= RFThresh$threshold, 1, 0))
fscore <- confusionMatrix(table(RF.class, tour.imp[split.valid,]$Book_12Mo),
                          positive = "1")$byClass["F1"]  
fscore
misc <- 1 - confusionMatrix(table(RF.class, tour.imp[split.valid,]$Book_12Mo),
                            positive = "1")$overall["Accuracy"]
unname(misc)
