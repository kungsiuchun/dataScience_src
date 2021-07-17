

set.seed(1, sample.kind = "Rounding") #
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# What is the probability that a test is positive?
mean(test)


mean(test[disease==1]==1) # 0.85

p(D|test-) = P(test-|D) * P(D) / P(Test-|D) *P(D) + P(Test-|H) *P(H)
           = 0.15 * 0.02 / 0.15*0.02 + 0.9*0.98

# What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0]==1)

# What is the probability that you have the disease if the test is positive?
mean(disease[test==1]==1)

# If a patient's test is positive, by how many times does that increase their risk of having the disease?
mean(disease[test==1]==1)/mean(disease==1)

