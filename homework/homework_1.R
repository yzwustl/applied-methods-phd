data <- read.csv("D:/Dropbox/Finance PhD/applied-methods-phd/homework/data/lalonde_nsw.csv")

# treatment effect
mean(data$re78[data$treat == 1]) - mean(data$re78[data$treat == 0])

# treatment effect for those who actually got the treatment
mean(data$re78[data$treat == 1]) - mean(data$re78[data$treat == 0])

#permutation tests
outcome   = data$re78
treatment = data$treat
null      = mean(data$re78[data$treat == 1]) - mean(data$re78[data$treat == 0])


permutation.test <- function(outcome,treatment,n){
  distribution=c()
  for(i in 1:n){
    ones <- rep(1, 185)
    zeros <- rep(0, 260)
    combined <- c(ones, zeros)
    l_treatment <- sample(combined)
    l_data      <- data.frame(outcome, l_treatment)
    l_treatment_effect <- mean(l_data$outcome[l_data$l_treatment == 1]) - mean(l_data$outcome[l_data$l_treatment == 0])
    distribution[i]    <- l_treatment_effect
  }
 result=sum(abs(distribution) >= abs(null))/(n)
 return(list(result, distribution))
}

test1 <- permutation.test(outcome, treatment, 1000)[[2]]


hist(test1, breaks=50, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=null, lwd=3, col="red")

permutation.test(outcome, treatment, 1000)[[1]]


t.test(data$re78~data$treat)


 
