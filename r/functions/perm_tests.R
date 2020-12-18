library(boot)
  ### Permutation test on the sign. Proportion of decreases
  ## Keep function in script so easier to see what it is doing.
  sample_sign_neg <- function(x){
    x1 <- x * sample(c(-1,1), length(x), replace = TRUE)
    mean(x1<0)
  }
  
  ### Permutation test on the means. 
  
  sample_sign <- function(x){
    x * sample(c(-1,1), length(x), replace = TRUE)
  }



perm_tests <- function(dt, contacts_, label_ , sims = 50000) {
  ## Bootstrapped mean and ci
  bs_mean <- boot(dt$diff, bs_means, R = 1000)
  bs_ci <- boot.ci(bs_mean, index=1, type = "perc")
  bs_ci90 <- boot.ci(bs_mean, index=1, type = "perc", conf = 0.9)
  perm_test <- replicate(sims, sample_sign(dt$diff))
  
  perm_sign <- replicate(sims, sample_sign_neg(dt$diff))
  perm_means <- apply(perm_test,2, mean)
  
  
  # User written function in r/functions.
  summary_dt(dt, perm_means, perm_sign, sims, bs_ci, bs_ci90, contacts = contacts_, label = label_)
}




