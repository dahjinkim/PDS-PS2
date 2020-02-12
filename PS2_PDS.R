#PDS-PS2
#Jin Kim

#1
for(i in 1:7){
  print(i^3)
}


#2
count <- 0
for(i in 1:1000){
  roll <- sample(1:6, 2, replace = T)
  sum.roll <- sum(roll)
  if (i == 1 & sum.roll >= 8) {
    print(roll)
    break
  } else if (i != 1 & (roll[1] == 2 | roll[2] == 2 | roll[1] == 6 | roll[2] == 6)) {
    print(i)
    print(roll)
    count <- c(count, i)
    break
  }
}


#3
#reading in the data
library(readr)
GSS <- read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/
GSS-data.csv")

#creating a function that takes in one of "Trump", "Clinton", and "Other"
#and return the number of participants who voted for each of the inputs.
vote.choice <- function(x){
  if(x == "Trump"){
    return(sum(GSS$pres16=="Trump"))
  } else if (x=="Clinton"){
    return(sum(GSS$pres16=="Clinton"))
  } else if (x== "Other"){
    return(sum(GSS$pres16 != "Clinton" & GSS$pres16 != "Trump"))
  } else {
    return(NULL)
  }
}
#test to see if it works
vote.choice("Trump")
vote.choice("Clinton")
vote.choice("Other")

#now edit the function so that something else is entered,
#the function returns the message "Please enter ..."
vote.choice <- function(x){
  if(x == "Trump"){
    return(sum(GSS$pres16=="Trump"))
  } else if (x=="Clinton"){
    return(sum(GSS$pres16=="Clinton"))
  } else if (x== "Other"){
    return(sum(GSS$pres16 != "Clinton" & GSS$pres16 != "Trump"))
  } else {
    return(print("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response."))
  }
}
#test to see if it works
Clington <- c(1, 2, 3)
vote.choice(1)
vote.choice("Clington")
vote.choice(Clington)


#4.
#installing packages
install.packages("fivethirtyeight")
library(fivethirtyeight)

