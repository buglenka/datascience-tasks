inputs <- read.csv("inputs.txt", sep = " ", header = FALSE)
outputs <- read.csv("outputs.txt", sep = " ", header = FALSE)

y <- unlist(outputs[1])

for (i in 1:length(inputs)) {
  print(i)

  x <- unlist(inputs[i])
  
  chisqxy <- chisq.test(x,y)

  print(chisqxy)
  
  if(chisqxy$p.value < 0.05) {
    print("LESS")
  }
}

# All the same results
# ...
#[1] 1
#
#        Pearson's Chi-squared test
#
#data:  x and y
#X-squared = 3749700, df = 3747800, p-value = 0.2416
