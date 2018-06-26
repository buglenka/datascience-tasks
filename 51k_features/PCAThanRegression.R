library('ggplot2')

# Load inputs
inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

# Main set (50983 features, 1954 examples)
df <- as.data.frame(c(inputs[1:50982], outputs[1]))

# Feature names
names(df) <- c(paste0("x", 1:50982), "y")

# ---- splitting the data to subsets: 60% 20% 20% ---- #

tcvbound <- floor(nrow(df) * 0.6)  # boundary between training and cv set
cvtbound <- floor(nrow(df) * 0.8)  # boundary between cv and test set

df <- df[sample(nrow(df)), ]  

df.train <- df[1:tcvbound, ]               # Training set 
df.cv    <- df[(tcvbound+1):cvtbound, ]    # Cross-validation set
df.test  <- df[(cvtbound+1):nrow(df), ]    # Test set

# ---- performing PCA ---- #

x.train <- as.data.frame(df.train[1:50982])
x.cv    <- as.data.frame(df.cv[1:50982])
x.test  <- as.data.frame(df.test[1:50982])

y.train <- as.data.frame(df.train[50983])
y.cv    <- as.data.frame(df.cv[50983])
y.test  <- as.data.frame(df.test[50983])

# find principal components 
train.pca <- prcomp(x.train, center = TRUE, scale. = TRUE, retx = TRUE)

# new train data (PCA components) is in train.pca$x[ ... ] 

# summarize
summary(train.pca)

png("pcaplot.png")

# plot method
plot(train.pca, type = "l")

dev.off()

# ---- PCA for CV data ---- #
cv.pca <- predict(train.pca, newdata = x.cv)

# ---- PCA for test data ---- #
test.pca <- predict(train.pca, newdata = x.test)

# ---- choose optimal count of PCA components and fitting multiple regression ---- #
PCACountFit <- NULL

SmallestByTrain <- data.frame(PCACount = 0, Error = .Machine$integer.max)
SmallestByCv <- data.frame(PCACount = 0, Error = .Machine$integer.max)

cmax <- ncol(train.pca$x)
cmin <- floor(cmax*0.95)

# Error calculation
rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))

for (pca.count in cmin:cmax) {
  # new train data
  newdf.train <- as.data.frame(c(train.pca$x[1:pca.count, 1:cmax], y.train[1]))
  
  names(newdf.train) <- c(paste0("x", 1:pca.count), "y")
  
  cat(sprintf("newdf.train: [1,1] = %e, nrow = %d, ncol = %d\n", 
                       newdf.train[1,1], nrow(newdf.train), ncol(newdf.train)))

  # fit linear regression
  model.train <- lm(y~., data = newdf.train)
  
  summary(model.train)
  print(model.train)
  
  # predict y for train and cv data
  train.y.predicted <- predict(model.train)
  cv.y.predicted    <- predict(model.train, newdata = cv.pca[1:pca.count,])
  
  # calculate the error
  train.error <- rmse(y.train, train.y.predicted)
  cv.error    <- rmse(y.cv,    cv.y.predicted)
  
  # store valued
  PCACountFit <- rbing(PCACountFit, as.data.frame(PCACount = pca.count, 
                                                  error_type = factor(c("training", "validation"), 
                                                  levels=c("validation", "training")),
                                                  error=c(train.error, cv.error)))
  
  # Store the smalles errors
  if (SmallestByTrain$Error > train.error) {
    SmallestByTrain$PCACount <- pca.count; 
    SmallestByTrain$Error <- train.error; 
    }
  if (SmallestByCv$Error > cv.error) {
    SmallestByCv$PCACount <- pca.count; 
    SmallestByCv$Error <- cv.error; 
    }
}

png("PCACount.png")

# Plot the dependance mean squared error from PCA count
ggplot(PCACountFit, aes(x=PCACount, y=error, linetype=error_type)) + 
  geom_line(size=1, col="blue") + xlab("pca components count") + geom_hline(y=10, linetype=3)

dev.off()

# Look at the best values
cat(sprintf("The smallest MSE on train set is %e with %d PCA count.\n", 
            SmallestByTrain$Error, SmallestByTrain$PCACount))
cat(sprintf("The smallest MSE on cv set is %e with %d PCA count.\n", 
            SmallestByCv$Error, SmallestByCv$PCACount))

# The best decision is PCA count with the smallest error value on CV set

# Make predictions on test set
# new train data
newdf.train <- as.data.frame(c(train.pca$x[1:SmallestByCv$PCACount,], y.train))

# fit linear regression
model.train <- lm("y~.", newdf.train)

# predict y for test
test.y.predicted    <- predict(model.train, newdata = test.pca[1:SmallestByCv$PCACount,])

# calculate the error
test.error <- rmse(y.test, test.y.predicted)

# Print the result error
cat(sprintf("The MSE on test set is %e.\n", test.error))

# The End
