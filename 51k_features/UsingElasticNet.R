library('glmnet')
library('doMC')

# Load inputs
x <- as.data.frame(read.csv("inputs.txt", sep = " ", header = FALSE))
y <- as.data.frame(read.csv("outputs.txt", sep = " ", header = FALSE))

xy <- as.data.frame(c(x[1:50982], y[1]))

# ---- splitting the data to subsets: 80% 20% ---- #

bound <- floor(nrow(xy) * 0.8)  # boundary between training+cv set and test set

xy <- xy[sample(nrow(xy)), ]

x.traincv <- xy[1:bound, 1:50982]              # Training and cross-validation inputs
x.test    <- xy[(bound+1):nrow(xy), 1:50982]   # Test inputs

y.traincv <- xy[1:bound, 50983]                # Training and cross-validation output
y.test    <- xy[(bound+1):nrow(xy), 50983]     # Test output

mxtcv <- as.matrix(x.traincv)
mxtst <- as.matrix(x.test)
mytcv <- as.matrix(y.traincv)
mytst <- as.matrix(y.test)

# --- fitting linear regression model with elastic net regularization --- #

foldid = sample(1:10, size=length(y.traincv), replace=TRUE)

# Let's try different values of alpha
registerDoMC(cores=4)

cv1  = cv.glmnet(mxtcv, mytcv, foldid = foldid, alpha=1,    parallel = TRUE)
cv.8 = cv.glmnet(mxtcv, mytcv, foldid = foldid, alpha=.8,   parallel = TRUE)
cv.6 = cv.glmnet(mxtcv, mytcv, foldid = foldid, alpha=.6,   parallel = TRUE)
cv.4 = cv.glmnet(mxtcv, mytcv, foldid = foldid, alpha=.4,   parallel = TRUE)
cv.2 = cv.glmnet(mxtcv, mytcv, foldid = foldid, alpha=.2,   parallel = TRUE)
cv0  = cv.glmnet(mxtcv, mytcv, foldid = foldid, alpha=0,    parallel = TRUE)

# Plot curves
#par(mfrow=c(3,2))

#plot(cv1); plot(cv.8); plot(cv.6);
#plot(cv.4); plot(cv.2); plot(cv0);

png('cvxall.png')

plot(log(cv1$lambda), cv1$cvm, pch=19, col="red", xlab="log(Lambda)", ylab=cv1$name)
points(log(cv.8$lambda), cv.8$cvm, pch=19, col="purple")
points(log(cv.6$lambda), cv.6$cvm, pch=19, col="green")
points(log(cv.4$lambda), cv.4$cvm, pch=19, col="blue")
points(log(cv.2$lambda), cv.2$cvm, pch=19, col="grey")
points(log(cv0$lambda), cv0$cvm, pch=19, col="black")
legend("topleft",legend=c("alpha= 1","alpha= .8", "alpha= .6", 
                          "alpha= .4", "alpha= .2", "alpha 0"),
       pch=19,col=c("red","purple","green", "blue", "grey", "black"))

dev.off()

# The better performance is when alpha = 1
# Use corresponding object to make predictions and estimate the error on test set
mytcv.predicted    <- predict(cv1, newx = mxtcv, s = "lambda.min")
mytst.predicted    <- predict(cv1, newx = mxtst, s = "lambda.min")

rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))

error.traincv <- rmse(mytcv, mytcv.predicted)
error.test    <- rmse(mytst, mytst.predicted)

# Print 
cat(sprintf("Mean squared error on Train+CV set = %e\n", error.traincv))
cat(sprintf("Mean squared error on Test set     = %e\n", error.test))

# The End



