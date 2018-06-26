#library("ranger")       # segment fault
library("randomForest")
#library("VSURF")        # segment fault
library('glmnet')
library('doMC')

registerDoMC()

# Error calculation
rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))

# ---------------------------- LOADING INPUTS ---------------------------------- #

datafile <- "OutliersEnv.RData"
timefile <- "OutliersTime.out"

if(!file.exists(datafile)) {
  
x <- read.csv("inputs.txt", sep = " ", header = FALSE)
y <- read.csv("outputs.txt", sep = " ", header = FALSE)

# Feature count and exapmles count
p <- ncol(x) 
m <- nrow(x)

# ----------------------------- PREPARE DATA ----------------------------------- #
# 1. Split data on outliers and normal set. 
# 2. Split each set on 3 sets: train, cv and test.
# 3. Mix corresponding sets of outliers and normal data.
# This is made for insure that outliers are presented in all main sets in same 
# proportions as normal data.

xy <- cbind2(x, y)

# Feature names
names(xy) <- c(paste0("x", 1:p), "y")

# Find 5% outliers and mark them as 1, others as 0
xy.sorted <- xy[ order(-y), ]

bound <- floor(m * 0.05)  # boundary between outliers and others

outliers  <- xy.sorted[1:bound, 1:p]
normals   <- xy.sorted[(bound+1):m, 1:p]

# Mark outliers as 1, other normal values as 0
outliers['Y']  <- rep(1, bound)
normals ['Y']  <- rep(0, m - bound)

# Examples count in sets
mn <- nrow(normals)
mo <- nrow(outliers)

# Make main sets: 60% 20% 20% 

# - Normal values
ntcvbound <- floor(mn * 0.6)  # boundary between training and cv set
ncvtbound <- floor(mn * 0.8)  # boundary between cv and test set

# - Outliers
otcvbound <- floor(mo * 0.6)  # boundary between training and cv set
ocvtbound <- floor(mo * 0.8)  # boundary between cv and test set

# Split
normals.train <- normals[1:ntcvbound, ]                   # Training set
normals.cv    <- normals[(ntcvbound+1):ncvtbound, ]       # Cross-validation set
normals.test  <- normals[(ncvtbound+1):mn, ]              # Test set

outliers.train <- outliers[1:otcvbound, ]                 # Training set
outliers.cv    <- outliers[(otcvbound+1):ocvtbound, ]     # Cross-validation set
outliers.test  <- outliers[(ocvtbound+1):mo, ]            # Test set

# Join
xy.train <- rbind(normals.train, outliers.train)
xy.cv    <- rbind(normals.cv, outliers.cv)
xy.test  <- rbind(normals.test, outliers.test)

# Mix
xy.train <- xy.train[sample(nrow(xy.train)), ]
xy.cv    <- xy.cv[sample(nrow(xy.cv)), ]
xy.test  <- xy.test[sample(nrow(xy.test)), ]

# Convert
mx.train   <- as.matrix(xy.train[1:p])
mx.cv      <- as.matrix(xy.cv[1:p])
mx.traincv <- rbind(mx.train, mx.cv)
mx.test    <- as.matrix(xy.test[1:p])

my.train   <- as.matrix(xy.train[p+1])
my.cv      <- as.matrix(xy.cv[p+1])
my.traincv <- rbind(my.train, my.cv)
my.test    <- as.matrix(xy.test[p+1])

save.image(datafile)
} else {
  load(datafile)
}

# ---------------------------- MODEL FITTING ----------------------------------- #

# ------------- LOGISTIC REGRESSION WITH FEATURE SELECTION --------------------- #

Rprof(timefile) # -------------------------------------------------------------- #
lmodel <- cv.glmnet(mx.traincv, my.traincv, family = "binomial", 
                    alpha = 1, parallel = TRUE)
Rprof(NULL) # ------------------------------------------------------------------ #

# Extract selected features
c <- coef(lmodel, s = lmodel$lambda.1se)
selected.fidxs <- which(c != 0) 
selected.rnames <-row.names(c)[ selected.fidxs ]

# Make formulas for futher model fitting from selected features only
selected.fmla <- as.formula(paste(colnames(my.traincv)[1], "~", 
                paste0(selected.rnames[2:length(selected.rnames)], collapse = "+")))

print(selected.fmla)
# ...

# Use corresponding object to make predictions and estimate the errors 
y.traincv.predicted <- predict(lmodel, newx = mx.traincv, s = "lambda.1se")
y.test.predicted    <- predict(lmodel, newx = mx.test,    s = "lambda.1se")

error.traincv <- rmse(my.traincv, y.traincv.predicted)
error.test    <- rmse(my.test,    y.test.predicted)

# Print
cat(sprintf("GLR: RMSE on Train+CV set = %e\n", error.traincv))
cat(sprintf("GLR: RMSE on Test set = %e\n",     error.test))

# ----------------------------- RANDOM FORESTS --------------------------------- #

#Rprof(timefile, append = TRUE) # ----------------------------------------------- #
#rfmodel <- randomForest(selected.fmla, data = xy.train, 
#                       importance = TRUE, proximity = TRUE)
#Rprof(NULL) # ------------------------------------------------------------------ #

#print(rfmodel)
#round(importance(rfmodel), 2)

# Use corresponding object to make predictions and estimate the errors 
#y.train.predicted   <- predict(rfmodel, newx = xy.train)
#y.test.predicted    <- predict(rfmodel, newx = xy.test)

#error.traincv <- rmse(my.train,   y.train.predicted)
#error.test    <- rmse(my.test,    y.test.predicted)

# Print
#cat(sprintf("RF: RMSE on Train set = %e\n",    error.traincv))
#cat(sprintf("RF: RMSE on Test set = %e\n",     error.test))


#Rprof(timefile, append = TRUE) # ----------------------------------------------- #
#fmodel <- VSURF(selected.fmla, data = xy.train, parallel = TRUE, 
#                clusterType = "FORK")
#Rprof(NULL) # ------------------------------------------------------------------ #

# View the forest results
#print(fmodel)

# ------------------------ SUPPORT VECTOR MACHINE ------------------------------ #

# -------------------------- MULTIPLE REGRESSION ------------------------------- #



# ------------------------------------------------------------------------------ #
summaryRprof(timefile, memory = "none")
 






