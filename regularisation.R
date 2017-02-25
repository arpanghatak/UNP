# load the package
library(glmnet)
# load data

x <- as.matrix(boston[,1:13])
y <- as.matrix(boston[,14])
lam <- 10.50
fitols <- glmnet(x, y, family="gaussian", alpha=0, lambda=0)
# summarize the fit
summary(fitols)
# make predictions
predictionsols <- predict(fitols, x, type="link")
# summarize accuracy
mseols <- mean((y - predictionsols)^2)# fit model


fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=lam)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
mse <- mean((y - predictions)^2)


lassofit <- glmnet(x, y, family="gaussian", alpha=1, lambda=lam)
# summarize the fit
summary(lassofit)
# make predictions
predictionslasso <- predict(lassofit, x, type="link")
# summarize accuracy
mselasso <- mean((y - predictionslasso)^2)


elastfit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=lam)
# summarize the fit
summary(elastfit)
# make predictions
predictionselast <- predict(elastfit, x, type="link")
# summarize accuracy
mseelast <- mean((y - predictionselast)^2)

print(mse)


print(mselasso)
print(mseelast)

comp <- c(lam, mseols, mse, mselasso, mseelast)
print(comp)