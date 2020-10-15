## an_tenpm
## conduct permutation test

library(boot)
library(data.table)

source('r/functions/summary_dt.R')
source('r/functions/bs_means.R')

tenpm <- readRDS('data/10pm_wide.rds')

Sims <- 50000

# Adult and children
table(tenpm$survey_type)


nrow(tenpm)

# check the distribution of each value for contacts
summary(tenpm$before)
summary(tenpm$after)
summary(tenpm$diff)

## Check the amount of positive or negative changes
table(sign(tenpm$diff))

## Bootstrapped mean and ci
bs_mean <- boot(tenpm$diff, bs_means, R = 1000)
bs_ci <- boot.ci(bs_mean, index=1, type = "perc")
bs_ci90 <- boot.ci(bs_mean, index=1, type = "perc", conf = 0.9)

### Permutation test on the sign. Proportion of decreases
## Keep function in script so easier to see what it is doing.
sample_sign_neg <- function(x){
  x1 <- x * sample(c(-1,1), length(x), replace = TRUE)
  mean(x1<0)
}
perm_sign <- replicate(Sims, sample_sign_neg(tenpm$diff))

### Permutation test on the means. 

sample_sign <- function(x){
  x * sample(c(-1,1), length(x), replace = TRUE)
}

perm_test <- replicate(Sims, sample_sign(tenpm$diff))

perm_means <- apply(perm_test,2, mean)

mean(perm_means)
hist(perm_means, main = "10pm")
abline(v = mean(tenpm$diff))
## How many times are permuations means more extreme than the observed mean di
table(abs(perm_means) >= abs(mean(tenpm$diff)))/Sims

# User written function in r/functions.
tenpm_summ <- summary_dt(tenpm, perm_means, perm_sign, Sims, bs_ci, bs_ci90, contacts = "Other Contacts", label = "10pm closure")

saveRDS(tenpm_summ, 'outputs/10pm_summary.RDS')








