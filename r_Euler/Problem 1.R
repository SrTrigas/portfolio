# Get the sum of all numbers are multiples of 3 OR 5 below 1000
n = 999

multiple= function(i,m){
  mn = FALSE
  if (i %% m == 0) {
    mn = TRUE
  }
mn
}

placeholder = vector()

for (i in 1:n) {
  m3=multiple(i,3)
  m5=multiple(i,5)
  
  if ((m3 | m5) == TRUE){
    placeholder=c(placeholder,i)
  }
}

sum(placeholder)

