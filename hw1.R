library(ISLR)
library(MASS)
library(class)
summary(Weekly)
dim(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
attach(Weekly)
plot(Year, Volume)
plot(Volume, Lag1)


#Logistic Regression 1
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep ("Down",1089)
glm.pred[glm.probs >.5]="Up"
table(glm.pred, Direction)
mean(glm.pred== Direction)



#Logistic Regression 2
train = (Year < 2009)
Weekly.heldOut = Weekly[!train, ]
Direction.heldOut = Direction[!train]
glm.fit=glm(Direction ~ Lag2 + I(Lag2^2), data=Weekly,family=binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.heldOut, type = "response")
contrasts(Direction)
glm.pred=rep ("Down",104)
glm.pred[glm.probs >.5]="Up"
table (glm.pred , Direction.heldOut)
mean(glm.pred== Direction.heldOut)


#LDA
lda.fit=lda(Direction~Lag2 + I(Lag2^2),data=Weekly ,subset =train)
lda.fit
plot(lda.fit)
lda.pred=predict (lda.fit , Weekly.heldOut)
lda.class = lda.pred$class
table(lda.class, Direction.heldOut)
mean(lda.class==Direction.heldOut)

#KNN with k = 1
train.X = cbind(cbind(Lag2)[train, ])
test.X = cbind(cbind(Lag2)[!train, ])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=21)
table(knn.pred, Direction.heldOut)
mean(knn.pred==Direction.heldOut)
