library('Matrix')

# Error calculation
rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))

# ---------------------------- LOADING INPUTS ---------------------------------- #

datafile <- "PCAEnv.RData"
timefile <- "PCATimes.out"

if(!file.exists(datafile)) {
  
  x <- read.csv("inputs.txt", sep = " ", header = FALSE)
  y <- read.csv("outputs.txt", sep = " ", header = FALSE)
  
  # Feature count and exapmles count
  p <- ncol(x) 
  m <- nrow(x)
  
  xy <- cbind2(x, y)
  
  # Feature names
  names(xy) <- c(paste0("x", 1:p), "y")
  
  # -------------------------- PREPARING MAIN SETS ------------------------------- #
  
  tcvbound <- floor(m * 0.6)  # boundary between training and cv set
  cvtbound <- floor(m * 0.8)  # boundary between cv and test set
  
  xy <- xy[ sample(m), ]  
  
  xy.train <- xy[1:tcvbound, ]               # Training set 
  xy.cv    <- xy[(tcvbound+1):cvtbound, ]    # Cross-validation set
  xy.test  <- xy[(cvtbound+1):m,        ]    # Test set
  
  x.train <- xy.train[1:p]
  x.cv    <- xy.cv[1:p]
  x.test  <- xy.test[1:p]
  
  y.train <- xy.train$y
  y.cv    <- xy.cv$y
  y.test  <- xy.test$y
  
  save.image(datafile)
} else {
  load(datafile)
}

# --------------------------- DECREASE DIMENTIONS VIA PCA ---------------------- #

Rprof(timefile) # -------------------------------------------------------------- #
train.pca <- prcomp(x.train, center = TRUE, scale. = TRUE, retx = TRUE)
Rprof(NULL) # ------------------------------------------------------------------ #

# new train data (PCA components) is in train.pca$x[ ... ] 

# summarize
summary(train.pca)

cv.pca <- predict(train.pca, newdata = x.cv)     # PCA for CV data
test.pca <- predict(train.pca, newdata = x.test) # PCA for test data

newp <- ncol(train.pca$x) # new features count

# -------------------------------- MODEL FITTING ------------------------------- #
PCACountFit <- NULL

SmallestByTrain <- data.frame(PCACount = 0, Error = .Machine$integer.max)
SmallestByCv    <- data.frame(PCACount = 0, Error = .Machine$integer.max)

# Cicle model fitting for [ minp; newp ] features
minp <- floor(newp * 0.8) 

for (pca.count in minp:newp) {
  
  # new data
  newx.train  <- data.frame(train.pca$x[ ,1:pca.count ])
  newx.cv     <- data.frame(cv.pca[ ,1:pca.count ])
  newx.test    <- data.frame(test.pca[ ,1:pca.count ])
  
  newxy.train <- cbind2(newx.train, y.train)
  
  # fit linear regression
  model.train <- glm("y~.", family = gaussian(), newxy.train)
  
  #summary(model.train)
  #print(model.train)
  
  # predict y for train and cv data
  y.train.predicted <- predict(model.train, newdata = newx.train)
  y.cv.predicted    <- predict(model.train, newdata = newx.cv)
  y.test.predicted  <- predict(model.train, newdata = newx.test)
  
  # calculate the error
  train.error <- rmse(y.train, y.train.predicted)
  cv.error    <- rmse(y.cv,    y.cv.predicted)
  test.error  <- rmse(y.test,  y.test.predicted)
  
  # Print
  cat(sprintf("(PC = %d) RMSE [train = %e] [cv = %e] [test = %e].\n", 
              pca.count, train.error, cv.error, test.error))

  # store valued
#  PCACountFit <- rbind2(PCACountFit, data.frame(PCACount = pca.count, 
#                        error_type = factor(c("training", "validation"), 
#                        levels=c("validation", "training")),
#                        error=c(train.error, cv.error)))
#  
  
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

#png("PCACount.png")

# Plot the dependance mean squared error from PCA count
#ggplot(PCACountFit, aes(x=PCACount, y=error, linetype=error_type)) + 
#  geom_line(size=1, col="blue") + xlab("pca components count") + geom_hline(y=10, linetype=3)

#dev.off()

# Look at the best values
cat(sprintf("The smallest RMSE on train set is %e with %d PCA count.\n", 
            SmallestByTrain$Error, SmallestByTrain$PCACount))
cat(sprintf("The smallest RMSE on cv set is %e with %d PCA count.\n", 
            SmallestByCv$Error, SmallestByCv$PCACount))

# The best decision is PCA count with best error value on CV set

# ------------------------------------------------------------------------------ #
summaryRprof(timefile, memory = "none")
