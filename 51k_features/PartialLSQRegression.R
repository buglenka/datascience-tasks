library("pls")

# prepare data
inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

df <- as.data.frame(c(inputs[1:50982],outputs[1]))

names(df) <- paste0("x", 1:50983)

# fit the model
fit <- plsr(x50983~., data=df, validation="CV")

# summarize the fit
summary(fit)

# make predictions
predictions <- predict(fit, df, ncomp=6)

# summarize accuracy
rmse <- mean((df$x50983 - predictions)^2)
print(rmse)

# *** caught segfault ***
#address 0x7ef80a55be8c, cause 'memory not mapped'
#
#Traceback:
# 1: terms.formula(formula, data = data)
# 2: terms(formula, data = data)
# 3: model.frame.default(formula = x50983 ~ ., data = df)
# 4: model.frame(formula = x50983 ~ ., data = df)
# 5: eval(expr, envir, enclos)
# 6: eval(mf, parent.frame())
# 7: pls::mvr(x50983 ~ ., data = df, validation = "CV", method = "kernelpls")
# 8: eval(expr, envir, enclos)
# 9: eval(cl, parent.frame())
#10: plsr(x50983 ~ ., data = df, validation = "CV")
#aborting ...
#Segmentation fault (core dumped)
