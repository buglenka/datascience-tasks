# load the library
library(mlbench)
library(caret)

# load data
inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

# ensure the results are repeatable
set.seed(7)

# prepare data
datadf <- as.data.frame(c(inputs[1:50982],outputs[1]))
names(datadf) <- paste0("x", 1:50983)

# calculate correlation matrix
correlationMatrix <- cor(datadf)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# *** caught segfault ***
#address 0x7f86f0f30878, cause 'memory not mapped'
#
#Traceback:
# 1: cor(datadf)
#aborting ...
#Segmentation fault (core dumped)
