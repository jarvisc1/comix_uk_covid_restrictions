## an_wfh
## conduct permutation test

library(boot)
library(data.table)


source('r/functions/summary_dt.R')
source('r/functions/bs_means.R')


wfh <- readRDS('data/wfh_wide.rds')

Sims <- 50000

nrow(wfh)

# check the distribution of each value for contacts
summary(wfh$before)
summary(wfh$after)
summary(wfh$diff)

## Check the amount of positive or negative changes
table(sign(wfh$diff))

## Bootstrapped mean and ci
bs_mean <- boot(wfh$diff, bs_means, R = 1000)
bs_ci <- boot.ci(bs_mean, index=1, type = "perc")
bs_ci90 <- boot.ci(bs_mean, index=1, type = "perc", conf = 0.9)

### Permutation test on the sign. Proportion of decreases
## Keep function in script so easier to see what it is doing.
sample_sign_neg <- function(x){
  x1 <- x * sample(c(-1,1), length(x), replace = TRUE)
  mean(x1<0)
}
perm_sign <- replicate(Sims, sample_sign_neg(wfh$diff))

### Permutation test on the means. 
sample_sign <- function(x){
  x * sample(c(-1,1), length(x), replace = TRUE)
}

perm_test <- replicate(Sims, sample_sign(wfh$diff))

perm_means <- apply(perm_test,2, mean)

mean(perm_means)
hist(perm_means, main = "WFH")
abline(v = mean(wfh$diff))
## How many times are permuations means more extreme than the observed mean di
table(abs(perm_means) >= abs(mean(wfh$diff)))/Sims

## Equivalence
table(perm_means > 1.1)/Sims
table(perm_means < -1.1)/Sims

# User written function in r/functions.
wfh_summ <- summary_dt(wfh, perm_means, perm_sign,  Sims, bs_ci, bs_ci90, contacts = "Contacts at work for employed", label = "Work from Home")

saveRDS(wfh_summ, 'outputs/wfh_summary.RDS')







