# rule_of_six permutation test


library(boot)
library(data.table)

source('r/functions/summary_dt.R')
source('r/functions/bs_means.R')

ros <- readRDS('data/rule_of_six_wide.rds')

Sims <- 50000
nrow(ros)

# check the distribution of each value for contacts
summary(ros$before)
summary(ros$after)
summary(ros$diff)

## Check the amount of positive or negative changes
table(sign(ros$diff))

## Bootstrapped mean and ci
bs_mean <- boot(ros$diff, bs_means, R = 1000)
bs_ci <- boot.ci(bs_mean, index=1, type = "perc")
bs_ci90 <- boot.ci(bs_mean, index=1, type = "perc", conf = 0.9)

### Permutation test on the sign. Proportion of decreases
## Keep function in script so easier to see what it is doing.
sample_sign_neg <- function(x){
  x1 <- x * sample(c(-1,1), length(x), replace = TRUE)
  mean(x1<0)
}
perm_sign <- replicate(Sims, sample_sign_neg(ros$diff))

## Permutation test
sample_sign <- function(x){
  x * sample(c(-1,1), length(x), replace = TRUE)
}

perm_test <- replicate(Sims, sample_sign(ros$diff))

perm_means <- apply(perm_test,2, mean)

mean(perm_means)
hist(perm_means, main = "Rule of Six")
abline(v = mean(ros$diff))
## How many times are permuations means more extreme thatn the observed mean di
table(abs(perm_means) >= abs(mean(ros$diff)))/Sims

# User written function in r/functions. 
ros_summ <- summary_dt(ros, perm_means, perm_sign, Sims, bs_ci, bs_ci90, contacts = "Contacts excluding work and school", label = "Rule of Six")

saveRDS(ros_summ, 'outputs/ros_summary.RDS')

