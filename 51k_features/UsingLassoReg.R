library('glmnet')
library('doMC')
library('gputools')

registerDoMC()

rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))

# ---------------------------- LOADING INPUTS ---------------------------------- #

datafile <- "LassoEnv.RData"

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

# ----------------- FEATURE SELECTION USING LASSO REGULARIZATION --------------- #

Rprof("UsingLassoTimes.out") # ------------------------------------------------- #
sfmodel = cv.glmnet(x.traincv, y.traincv, alpha = 1, parallel = TRUE)
Rprof(NULL) # ------------------------------------------------------------------ #

# lambda.min = 9.941108e-15
# lambda.1se = 1.582906e-14

# coef(model, s = "lambda.1se")

# Extract selected features
c <- coef(sfmodel, s = sfmodel$lambda.1se)
selected.fidxs <- which(c != 0) 
selected.rnames <-row.names(c)[ selected.fidxs ]

# Make formulas for futher model fitting from selected features only
selected.fmla <- as.formula(paste(colnames(y)[1], "~", 
                paste0(selected.rnames[2:length(selected.rnames)], collapse = "+")))

print(selected.fmla)
# ...

# Use corresponding object to make predictions and estimate the errors 
y.traincv.predicted <- predict(sfmodel, newx = x.traincv, s = "lambda.1se")
y.test.predicted    <- predict(sfmodel, newx = x.test,    s = "lambda.1se")

error.traincv <- rmse(y.traincv, y.traincv.predicted)
error.test    <- rmse(y.test,    y.test.predicted)

# Print
cat(sprintf("GLR: MSE on Train+CV set = %e\n", error.traincv))
cat(sprintf("GLR: MSE on Test set = %e\n",     error.test))

# -------------------------------- MODEL FITTING ------------------------------- #

xydf.traincv <- data.frame(x.traincv)
xydf.test    <- data.frame(x.test)

xydf.traincv [ 'y' ] <- y.traincv
xydf.test    [ 'y' ] <- y.test

# Lets try multiple regression
Rprof("UsingLassoTimes.out", append = TRUE) # ---------------------------------- #
mrmodel <- lm(selected.fmla, xydf.traincv)
Rprof(NULL) # ------------------------------------------------------------------ #

# Use corresponding object to make predictions and estimate the errors 
y.traincv.predicted <- predict(mrmodel, newdata = xydf.traincv)
y.test.predicted    <- predict(mrmodel, newdata = xydf.test)

error.traincv <- rmse(y.traincv, y.traincv.predicted)
error.test    <- rmse(y.test,    y.test.predicted)

# Print
cat(sprintf("MLR: MSE on Train+CV set = %e\n", error.traincv))
cat(sprintf("MLR: MSE on Test set = %e\n",     error.test))

# Lets try gpu generalized linear regression
Rprof("UsingLassoTimes.out", append = TRUE) # ---------------------------------- #
glmodel <- gpuGlm(selected.fmla, xydf.traincv, family = gaussian())
Rprof(NULL) # ------------------------------------------------------------------ #

# Use corresponding object to make predictions and estimate the errors 
y.traincv.predicted <- predict(glmodel, newdata = xydf.traincv)
y.test.predicted    <- predict(glmodel, newdata = xydf.test)

error.traincv <- rmse(y.traincv, y.traincv.predicted)
error.test    <- rmse(y.test,    y.test.predicted)

# Print
cat(sprintf("GPU GLR: MSE on Train+CV set = %e\n", error.traincv))
cat(sprintf("GPU GLM: MSE on Test set = %e\n",     error.test))

# ------------------------------------------------------------------------------ #
summaryRprof("UsingLassoTimes.out", memory = "none")


