rm(list = ls())

#Q1
for (n in seq(7)) {
  print(n^3)
}

#Q2
set.seed(14)

rolls_results = rep(0,1000)
for (iteration in seq(1000)) {
  dice1 = sample(seq(6),1)
  dice2 = sample(seq(6),1)
  rolls = 1 # Assuming we count the initial roll
  sum = dice1 + dice2
  if (sum < 8) {
    while ((sum != 2) & (sum != 6)) {
      rolls = rolls + 1
      dice1 = sample(seq(6),1)
      dice2 = sample(seq(6),1)
      sum = dice1 + dice2
    }
  }
  rolls_results[iteration] = rolls
}
print(mean(rolls_results)) #Returns 3.433



