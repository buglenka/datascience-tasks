# load the library
library(mlbench)
library(caret)

# load data
inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

# prepare data
datadf <- as.data.frame(c(inputs[1:50982],outputs[1]))
names(datadf) <- paste0("x", 1:50983)

# ensure results are repeatable
set.seed(7)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
model <- train(x50983~., data=datadf, method="lvq", preProcess="scale", trControl=control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)

# *** caught segfault ***
#address 0x7f2c674e8e8c, cause 'memory not mapped'
#
#Traceback:
# 1: terms.formula(formula, data = data)
# 2: terms(formula, data = data)
# 3: model.frame.default(form = x50983 ~ ., data = datadf, na.action = na.fail)
# 4: stats::model.frame(form = x50983 ~ ., data = datadf, na.action = na.fail)
# 5: eval(expr, envir, enclos)
# 6: eval(expr, p)
# 7: eval.parent(m)
# 8: train.formula(x50983 ~ ., data = datadf, method = "lvq", preProcess = "scale",     trControl = control)
# 9: train(x50983 ~ ., data = datadf, method = "lvq", preProcess = "scale",     trControl = control)
#aborting ...
#Segmentation fault (core dumped)
