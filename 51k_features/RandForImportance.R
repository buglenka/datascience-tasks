library('FSelector')

inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

datadf <- as.data.frame(c(inputs[1:50982],outputs[1]))

names(datadf) <- paste0('x', 1:50983)

weights <- random.forest.importance(x50983~., datadf, importance.type = 1)
print(weights)
subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "x50983")
print(f)
