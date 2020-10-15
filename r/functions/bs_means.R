## bs_means

bs_means <- function(vals, indices) {
  samples <- vals[indices]
  mean(samples)
}