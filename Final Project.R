X1PercentSample = read.csv("C:\\Users\\tyler\\Desktop\\Econ 128\\Final Project\\1PercentSample.csv")
require(glmnet)
require(datasets)
require(randomForest)

### Task 1
#### Cleaning the Data

# removing 0's from Year Built
ladata = X1PercentSample[X1PercentSample$YearBuilt!=0,]

# creating data set with only variables I want to use
data = subset(ladata,select=c(TotalValue, YearBuilt, SQFTmain, Bedrooms, 
                         Bathrooms, Units, CENTER_LAT, CENTER_LON))
data = na.omit(data) # NA values in LAT and LON, need this to clean it out

### Task 2
#### Split data into train and test, assigning 90% to train and 10% to test
x = model.matrix(TotalValue~. ,  data)[,-1]
y = data$TotalValue

set.seed(128)
train = sample(1:nrow(x), .9*nrow(x))
test = (-train)
y.test = y[test]

### Task 3 and 4
#### Predict Total Value of a property with 3 different models

### Lasso
lasso.mod = glmnet(x[train,], y[train], alpha=1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)

bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx = x[test,])

mean((lasso.pred-y.test)^2)
# MSE = 2.9896 * 10^12

mean((mean(y[train])-y.test)^2) #what it would be if we did not predict
# MSE = 3.835 * 10^12
out = glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out, type='coefficients', s=bestlam)
lasso.coef # shows what coefficients it would get rid of since lasso elims

### Ridge
cv.out=cv.glmnet(x[train,],y[ train],alpha=0)
ridge.mod=glmnet (x,y,alpha=0, lambda=grid)

ridge.mod=glmnet(x[train ,],y[ train],alpha=0, lambda =grid,
                 thresh =1e-12)
bestlam =cv.out$lambda.min
bestlam

ridge.pred=predict (ridge.mod,s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)
# MSE = 2.9907 * 10^12

out=glmnet(x,y,alpha=0)
predict (out ,type="coefficients",s=bestlam)

### Random Forests and Bagging
bag.la = randomForest(TotalValue~., data=data, subset = train, 
                      mtry=7, ntree=25)
bag.la

yhat.bag = predict(bag.la, newdata = data[-train,])
plot(yhat.bag, y.test)
abline(0,1)
mean((yhat.bag - y.test)^2)
# MSE = 2.784 * 10^12

rf.la = randomForest(TotalValue~., data = data, subset = train,
                     mtry = 3, importance  =TRUE)
yhat.rf = predict(rf.la, newdata = data[-train,])
mean((yhat.rf - y.test)^2)
# MSE = 2.546 * 10^12

### GLM
glm.fits = glm(TotalValue~., data = data, subset=train)
summary(glm.fits)
glm.pred=predict (glm.fits,newx=data[test,])
summary(glm.pred)
mean((glm.pred -y.test)^2)
# MSE = 6.453607e+12








