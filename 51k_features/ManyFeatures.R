#library('glmnet')
#library('doMC')
#library('e1071')
library('Matrix')

#registerDoMC()

rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))

# ---------------------------- LOADING INPUTS ---------------------------------- #

datafile <- "ManyFeaturesEnv.RData"
timefile <- "ManyFeaturesTimes.out"

if(!file.exists(datafile)) {
  
  x <- read.table("inputs.txt", sep = " ", header = FALSE)
  y <- read.table("outputs.txt", sep = " ", header = FALSE)
  
  p <- ncol(x) # feature count
  m <- nrow(x) # examples count
  
  # names
  colnames(x) <- paste0("x", 1:p)
  colnames(y) <- "y"
  
  # -------------------------- PREPARING MAIN SETS ------------------------------- #
  
  bound <- floor(m * 0.8)  # boundary between train+cv (80%) and test (20%) sets
  
  x.traincv <- x[1:bound, ]        # Training and cross-validation inputs
  x.test    <- x[(bound+1):m,  ]   # Test inputs
  
  y.traincv <- y[1:bound, ]         # Training and cross-validation output
  y.test    <- y[(bound+1):m, ]     # Test output
  
  mx.traincv <- as.matrix(x.traincv)
  mx.test    <- as.matrix(x.test)
  
  my.traincv <- as.matrix(y.traincv)
  my.test    <- as.matrix(y.test)
  
  xy.traincv <- cbind2(x.traincv, y.traincv)
  xy.test    <- cbind2(x.test, y.test)
  
  save.image(datafile)
} else {
  load(datafile)
}
quit()
# ----------------- FEATURE SELECTION USING LASSO REGULARIZATION --------------- #

Rprof(timefile) # -------------------------------------------------------------- #
sfmodel = cv.glmnet(mx.traincv, my.traincv, alpha = 1, parallel = TRUE)
Rprof(NULL) # ------------------------------------------------------------------ #

# Extract selected features
c <- coef(sfmodel, s = sfmodel$lambda.1se)
selected.fidxs <- which(c != 0)
selected.rnames <-row.names(c)[ selected.fidxs ]
selected.rnames<-selected.rnames[ !(selected.rnames %in% '(Intercept)') ];

# Use corresponding object to make predictions and estimate the errors
y.traincv.predicted <- predict(sfmodel, newx = mx.traincv, s = "lambda.1se")
y.test.predicted    <- predict(sfmodel, newx = mx.test,    s = "lambda.1se")

error.traincv <- rmse(my.traincv, y.traincv.predicted)
error.test    <- rmse(my.test,    y.test.predicted)

# Print
cat(sprintf("GLR: RMSE on Train+CV set = %e\n", error.traincv))
cat(sprintf("GLR: RMSE on Test set = %e\n",     error.test))

# -------------------------------- MODEL FITTING ------------------------------- #
# Fit model in cycle with different count of selected features.
# Start from 1 feature up to full set.

Results <- data.frame(fcnt = NULL, lTrainError = NULL, pTrainError = NULL, 
                      lTestError = NULL, pTestError = NULL)

for (size in 1:length(selected.rnames)) {
  
  # Make formulas for futher model fitting from selected features only
  ypart = colnames(my.traincv)[1]
  xpart = paste0(selected.rnames [1:size], collapse = "+") 
  
  # Make formula for futher model fitting from selected features only
  selected.fmla <- as.formula(paste(ypart, "~", xpart)) 
  
  # Print formula
  print(selected.fmla)  

  # ----------- SUPPORT VECTOR MACHINE REGRESSION (LINEAR) ----------------------- #
  
  # Lets try SVM regression
  Rprof(timefile, append = TRUE) # ----------------------------------------------- #
  svmmodel <- svm(selected.fmla, xy.traincv, cross = 10)
  Rprof(NULL) # ------------------------------------------------------------------ #
  
  # Use corresponding object to make predictions and estimate the errors
  y.traincv.predicted <- predict(svmmodel, newdata = xy.traincv)
  y.test.predicted    <- predict(svmmodel, newdata = xy.test)
  
  lerror.traincv <- rmse(y.traincv, y.traincv.predicted)
  lerror.test    <- rmse(y.test,    y.test.predicted)
  
  # Print
  cat(sprintf("SVM[ln]: RMSE on Train+CV set = %e\n", lerror.traincv))
  cat(sprintf("SVM[ln]: RMSE on Test set = %e\n",     lerror.test))
  
  # -------- SUPPORT VECTOR MACHINE REGRESSION (POLYNOMIAL) ---------------------- #
  
  # Lets try SVM regression
  Rprof(timefile, append = TRUE) # ----------------------------------------------- #
  svmmodel <- svm(selected.fmla, xy.traincv, cross = 10, kernel = "polynomial")
  Rprof(NULL) # ------------------------------------------------------------------ #
  
  # Use corresponding object to make predictions and estimate the errors
  y.traincv.predicted <- predict(svmmodel, newdata = xy.traincv)
  y.test.predicted    <- predict(svmmodel, newdata = xy.test)
  
  perror.traincv <- rmse(y.traincv, y.traincv.predicted)
  perror.test    <- rmse(y.test,    y.test.predicted)
  
  # Print
  cat(sprintf("SVM[poly]: RMSE on Train+CV set = %e\n", perror.traincv))
  cat(sprintf("SVM[poly]: RMSE on Test set = %e\n",     perror.test))
  
  Results <- rbind2(Results, data.frame(fcnt = size, 
                    lTrainError = lerror.traincv, pTrainError = perror.traincv, 
                    lTestError = perror.traincv, pTestError = perror.test))
}

# ------------------------------------------------------------------------------ #
summaryRprof(timefile, memory = "none")


