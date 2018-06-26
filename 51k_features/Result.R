library('glmnet')
library('doMC')

registerDoMC()

# ---------------------------- LOADING INPUTS ---------------------------------- #

datafile <- "FeatureSelectionEnv.RData"

if(!file.exists(datafile)) {
  
  x <- as.matrix(read.table("inputs.txt", sep = " ", header = FALSE))
  y <- as.matrix(read.table("outputs.txt", sep = " ", header = FALSE))
  
  # names
  colnames(x) <- paste0("x", 1:ncol(x))
  colnames(y) <- "y"
  
  m = nrow(x) # Examples count
  
  # -------------------------- PREPARING MAIN SETS ------------------------------- #
  
  bound <- floor(m * 0.8)  # boundary between train+cv (80%) and test (20%) sets
  
  x.traincv <- x[1:bound, ]        # Training and cross-validation inputs
  x.test    <- x[(bound+1):m,  ]   # Test inputs
  
  y.traincv <- y[1:bound, ]         # Training and cross-validation output
  y.test    <- y[(bound+1):m, ]     # Test output
  
  save.image(datafile)
} else {
  load(datafile)
}

# ------------------------- FEATURE SELECTION AND MODEL FITTING --------------- #

sfmodel = cv.glmnet(x.traincv, y.traincv, alpha = 1, parallel = TRUE)

# Use corresponding object to make predictions and estimate the errors 
y.traincv.predicted <- predict(sfmodel, newx = x.traincv, s = "lambda.1se")
y.test.predicted    <- predict(sfmodel, newx = x.test,    s = "lambda.1se")

error.traincv <- rmse(y.traincv, y.traincv.predicted)
error.test    <- rmse(y.test,    y.test.predicted)

# Print
cat(sprintf("GLR: MSE on Train+CV set = %e\n", error.traincv))
cat(sprintf("GLR: MSE on Test set = %e\n",     error.test))

# ------------------------------------------------------------------------------ #


