library(ggplot2)
library(dplyr)
library(readr)
library(RGtk2)
library(caret)
library(fBasics)
library(caTools)
library(partykit)
library(ROCR)
library(rpart)

##########
### Basic tree
set.seed(40703)
tree <- rpart(formula = Book_12Mo ~ ., data = tour.imp[split,])
tree.class <- predict(tree, tour.imp[split.valid,], type="class")
cm1 <- table(tour.imp[split.valid,]$Book_12Mo, tree.class)
plot(as.party(tree))
tree.misc1 <- (cm1[1,2]+cm1[2,1])/sum(cm1)
tree.misc1

# F1 value for basic tree
y.valid <- tour.imp[split.valid,]$Book_12Mo
y.valid <- as.factor(y.valid)
tree.f1 <- confusionMatrix(data = tree.class, 
                           reference = y.valid,
                           positive = "1",     
                           mode= "everything")
tree.f1[["byClass"]][["F1"]]

# ROC and AUC
library(gmodels)
library(pROC)
tree.prob <- predict(tree, tour.imp[split.valid,], type = "prob")[,2]
rocCurve.tree <- roc(tour.imp[split.valid,]$Book_12Mo, tree.prob)
auc(rocCurve.tree)

##########
### Tree with cp set at 0.001 and pruning
set.seed(40703)
tree2 <- rpart(formula = Book_12Mo ~ ., data = tour.imp[split,], control=rpart.control(cp=0.001))
cp.seq <- tree2$cptable[,1]
misc <- numeric()
for (i in 1:length(cp.seq)) {
  tree.predict = predict(prune(tree2, cp=cp.seq[i]), tour.imp[split.valid,], type="class") 
  cm=table(tour.imp[split.valid,]$Book_12Mo, tree.predict)
  misc[i]=(cm[1,2]+cm[2,1])/sum(cm)
}
plot(tree2$cptable[,'nsplit']+1,misc, type="o", xlab="Number of Leaves", ylab="Misclassification Rate")

# Optimal tree
treeoptimal <- prune(tree2,cp=cp.seq[misc==min(misc)])
tree.class <- predict(treeoptimal, tour.imp[split.valid,], type="class")
tree.prob <- predict(treeoptimal, tour.imp[split.valid,], type = "prob")[,2]
min(misc)

# Lift graph
evaluate.prob <- predict(treeoptimal, tour.imp[split.valid,], type = "prob")[,2]
pred.eva <- prediction(evaluate.prob, tour.imp[split.valid,]$Book_12Mo)
perf.eva <- performance(pred.eva,"lift","rpp")
plot(perf.eva, col= 'red', type="b",main="Lift Curve")

# F1 value
tree.class <- as.factor(tree.class)
tree.f1 <- confusionMatrix(data = tree.class, 
                           reference = y.valid,
                           positive = "1",     
                           mode= "everything")
tree.f1[["byClass"]][["F1"]]

# ROC and AUC
rocCurve.tree <- roc(tour.imp[split.valid,]$Book_12Mo, tree.prob)
auc(rocCurve.tree)

##########
### Decision tree with alternative cut-off threshold to address imbalance
set.seed(40703)
prop <- 1/prop.table(table(tour.imp$Book_12Mo))
costMatrix <- matrix(c(0,prop[2],prop[1],0), nrow=2) 
tree3 <- rpart(formula = Book_12Mo ~ ., data = tour.imp[split,],
              parms=list(loss=costMatrix), 
              control=rpart.control(cp=0.001))
cp.seq <- tree3$cptable[,1]
fscore <- numeric(0)
fscore[1] <- 0
for (i in 2:length(cp.seq)){
  tree.prob = predict(prune(tree3, cp=cp.seq[i]), tour.imp[split.valid,],type="prob")[,2]
  rocCurve.tree <- roc(tour.imp[split.valid,]$Book_12Mo, tree.prob, quiet=TRUE)
  treeThresh <- coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1, 0))
  fscore[i] <- confusionMatrix(table(tree.class, tour.imp[split.valid,]$Book_12Mo),
                             positive = "1")$byClass["F1"]
}
plot(tree3$cptable[,'nsplit']+1, fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")
tree.final <- prune(tree3, cp=cp.seq[fscore==max(fscore)])

# F1 value
tree.fscore <- max(fscore)
tree.fscore

# Misclassification
tree.final.misc <- 1 - confusionMatrix(table(tree.class, tour.imp[split.valid,]$Book_12Mo),
                positive = "1")$byClass["Balanced Accuracy"]
unname(tree.final.misc)

# ROC and AUC
tree.prob <- predict(tree.final, tour.imp[split.valid,],type="prob")[,2]
rocCurve.tree <- roc(tour.imp[split.valid,]$Book_12Mo, tree.prob)
auc(rocCurve.tree)

# Lift graph
evaluate.prob <- predict(tree3, tour.imp[split.valid,], type = "prob")[,2]
pred.eva <- prediction(evaluate.prob, tour.imp[split.valid,]$Book_12Mo)
perf.eva <- performance(pred.eva,"lift","rpp")
plot(perf.eva, col= 'red', type="b",main="Lift Curve")
