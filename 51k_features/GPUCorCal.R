inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

inoutputs <- c(unlist(inputs[1:50982]),unlist(outputs[1]))

library(gputools)

colnames(inoutputs) <- paste("x", 1:50983)
rownames(inoutputs) <- paste("r", 1:1954)

minoutputs <- do.call(rbind, inoutputs)

cr <- gpuCor(minoutputs)

print(cr)

print("Summary")

print(summary(cr))
