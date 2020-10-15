### Conduct permutation tests for local restrictions

library(boot)
library(data.table)

source('r/functions/summary_dt.R')
source('r/functions/bs_means.R')

ld <- readRDS('data/ld_wide.rds')

Sims <- 50000

nrow(ld)

# Adult and children
table(ld$survey_type)

# check the distribution of each value for contacts
summary(ld$before)
summary(ld$after)
summary(ld$diff)

## Check the amount of positive or negative changes
table(sign(ld$diff))

## Bootstrapped mean and ci
bs_mean <- boot(ld$diff, bs_means, R = 1000)
bs_ci <- boot.ci(bs_mean, index=1, type = "perc")
bs_ci90 <- boot.ci(bs_mean, index=1, type = "perc", conf = 0.9)

### Permutation test on the sign. Proportion of decreases
## Keep function in script so easier to see what it is doing.
sample_sign_neg <- function(x){
  x1 <- x * sample(c(-1,1), length(x), replace = TRUE)
  mean(x1<0)
}
perm_sign <- replicate(Sims, sample_sign_neg(ld$diff))


### Permutation test on the means. 

sample_sign <- function(x){
  x * sample(c(-1,1), length(x), replace = TRUE)
}

perm_test <- replicate(Sims, sample_sign(ld$diff))

perm_means <- apply(perm_test,2, mean)

# Create graph of simulations
mean(perm_means)
hist(perm_means, main = "Local")
abline(v = mean(ld$diff))
## How many times are permuations means more extreme than the observed mean di
table(abs(perm_means) >= abs(mean(ld$diff)))/Sims
sum(abs(perm_means) >= abs(mean(ld$diff)))/Sims

# User written function in r/functions.
ld_summ <- summary_dt(ld, perm_means, perm_sign, Sims, bs_ci, bs_ci90, contacts = "Contacts excluding work and school", label = "Local")

saveRDS(ld_summ , 'outputs/ld_summary.RDS')

