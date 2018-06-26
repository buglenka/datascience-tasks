library('pls')

# Load inputs
inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

# Main set (50983 features, 1954 examples)
df <- as.data.frame(c(inputs[1:50982], outputs[1]))

# Feature names
names(df) <- c(paste0("x", 1:50982), "y")

# ---- splitting the data to subsets: 80% 20% ---- #

bound <- floor(nrow(df) * 0.8)  # boundary between training+cv set and test set

df <- df[sample(nrow(df)), ]

df.traincv <- df[1:bound, ]              # Training and cross-validation set
df.test    <- df[(bound+1):nrow(df), ]   # Test set

# Build model
model <- pcr(y~., data = df.traincv, scale = TRUE, validation = "CV")

summary(model)

png("valplot.png")
validationplot(model)
dev.off()

png("valplotMSEP.png")
validationplot(model, val.type="MSEP")
dev.off()

png("valplotR2.png")
validationplot(model, val.type = "R2")
dev.off()

png("predplot.png")
predplot(model)
dev.off()

png("coefplot.png")
coefplot(model)
dev.off()

# Predict
pred <- predict(model, df.test, ncomp = 1090)

# Error
mean((pred - df.test$y)^2)

