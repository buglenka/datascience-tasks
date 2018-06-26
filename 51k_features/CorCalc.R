inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

y <- c(unlist(outputs[1]))
for(i in 1: 50982) {
  x <- c(unlist(inputs[i]))
  
  corRes <- cor.test(x,y)

  if(corRes$p.value < 0.05) {
    print(i)
  }
}

