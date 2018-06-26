inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

inoutputs <- c(inputs[1:50982], outputs[1])
names(inoutputs) <- paste("x", 1:50983)

dinoutputs <- data.frame(inoutputs)

lmres <- lm(x50983~., dinoutputs)

summary(lmres)
