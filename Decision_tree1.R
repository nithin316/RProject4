install.packages("ISLR")
library("ISLR")
attach(Carseats)
dim(Carseats)
write.csv(Carseats, "F:/Data Science/Week6/Carseats1.csv")

High<-ifelse(Sales<=8, "No", "Yes")
dim(Carseats)
str(High)

Carseats<-data.frame(Carseats, High)
dim(Carseats)
names(Carseats)

# Build tree
install.packages("tree")
library("tree")
tree.carseats<-tree(High~. -Sales, Carseats)
summary(tree.carseats)

# Plot
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

# Train amd test
set.seed(4)
train<-sample(1:nrow(Carseats), 200)
Carseats.test<-Carseats[-train, ]
High.test<-High[-train]

# Growing a tree
tree.carseats1<-tree(High ~ . -Sales, Carseats, subset = train)
tree.pred<-predict(tree.carseats1, Carseats.test, type = "class")
table(tree.pred, High.test)
(89+52)/200  #OAA=70.5%

summary(tree.carseats1)

# Pruning a tree
set.seed(5)
cv.carseats<-cv.tree(tree.carseats1, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

# Plotting error
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b", col = "red", lwd = 2)
plot(cv.carseats$k, cv.carseats$dev, type = "b", col = "red", lwd = 2)

# Prune the tree to 9 classes
prune.carseats<-prune.misclass(tree.carseats1, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred1<-predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred1,High.test)
150/200  #OAA=75%

##  Bagging  ##
install.packages("randomForest")
library("randomForest")
set.seed(1)
bag.carseats<-randomForest(High ~ . -Sales, Carseats, subset = train, mtry = 10, importance = TRUE)
bag.carseats
dim(Carseats)
importance(bag.carseats)
varImpPlot(bag.carseats, col = "red", pch = 10, cex = 1.25)
test.pred.bag<-predict(bag.carseats, newdata = Carseats[-train, ], type = "class")
table(test.pred.bag, High.test)
161/200  #OAA=80.5%

#  Random Forest #
set.seed(7)
rf.carseats<-randomForest(High ~ . -Sales, Carseats, subset = train, mtry = 3, importance = TRUE)
rf.carseats
importance(rf.carseats)
varImpPlot(rf.carseats, col = "red", pch = 10, cex = 1.25)
test.pred.rf<-predict(rf.carseats, newdata = Carseats[-train, ], type = "class")
table(test.pred.rf, High.test)
157/200  #OAA=78.5%
