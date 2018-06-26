library('FSelector')

inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

datadf <- c(inputs[1:50982],outputs[1])

names(datadf) <- paste0('x', 1:50983)

subset <- cfs(x50983~., datadf)
f <- as.simple.formula(subset, "x50983")
print(f)

# *** caught segfault ***
#address 0x7f6bc8505e8c, cause 'memory not mapped'

#Traceback:
# 1: terms.formula(formula, data = data)
# 2: terms(formula, data = data)
# 3: model.frame.default(formula, data, na.action = NULL)
# 4: model.frame(formula, data, na.action = NULL)
# 5: get.data.frame.from.formula(formula, data)
# 6: cfs(x50983 ~ ., datadf)
#aborting ...
#Segmentation fault (core dumped)
