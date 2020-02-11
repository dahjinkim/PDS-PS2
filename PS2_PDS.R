#PDS-PS2
#Jin Kim

#1
for(i in 1:7){
  print(i^3)
}

#2
set.seed(14)
count <- NULL
for(i in 1:1000){
  roll <- sample(1:6, 2, replace = T)
  sum.roll <- sum(roll)
  if (sum.roll >= 8 & sum.roll <=12) {
    print(i)
    break
  }
  count <- c(count, i)
  print(paste("The average trial is", mean(count)))
}
