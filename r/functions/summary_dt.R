## Summarise the results from the permutation tests
# dt = data
# perm = permutation mean diff
# perm_sign = permutation sign
# Number of simulations performed
# bs_ci = 95% CI
# bs_ci90 = 90% CI
# contacts = Text for how the contacts are defined
# label = The type of restriction
# bs

summary_dt <- function(dt, perm, perm_sign, sims, bs_ci, bs_ci90, contacts, label) {
  data.table(
    Restriction = label,
    Contacts = contacts,
    N = nrow(dt),
    Adults = sum(dt[["survey_type"]] == "adult"),
    Children = sum(dt[["survey_type"]] == "child"),
    Decreased = sum(dt[["diff"]] < 0),
    Same = sum(dt[["diff"]] == 0),
    Increased = sum(dt[["diff"]]> 0),
    p.value_sign = round(sum(abs(perm_sign) >= abs(mean(dt[["diff"]]<0)))/sims,4),
    Mean_prior = round(mean(dt[["before"]]), 2),
    Mean_after = round(mean(dt[["after"]]), 2),
    Mean_diff = round(mean(dt[["diff"]]),2),
    BS_lci = round(bs_ci[["percent"]][4],2),
    BS_uci = round(bs_ci[["percent"]][5],2),
    p.value_val = round(sum(abs(perm) >= abs(mean(dt[["diff"]])))/sims,4),
    Med_prior = median(dt[["before"]]),
    q25_prior = quantile(dt[["before"]], probs = 0.25),
    q75_prior = quantile(dt[["before"]], probs = 0.75),
    Med_after = median(dt[["after"]]),
    q25_after = quantile(dt[["after"]], probs = 0.25),
    q75_after = quantile(dt[["after"]], probs = 0.75)
  )
  
}

