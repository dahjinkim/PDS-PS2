#PDS-PS2
#Jin Kim

#1
for(i in 1:7){
  print(i^3)
}

#2
set.seed(14)
for(i in 1:1000){
  a <- sample(1:6, 1)
  b <- sample(1:6, 1)
  ab <- a + b
  if (ab %in% c(8, 9, 10, 11, 12)) {
    break
  } else if (a|b == 2|6) {
    break
  }
}
