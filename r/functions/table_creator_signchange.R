## Take a data.table, a restriction and then give either the counts, percentages, combined, 
## or counts in long form for the table. 



tab_display_signchange_count <- function(dt, var, lab, restriction, labnum) {
  tab <- table(dt[[var]], dt[["signchange_lab"]])
  tab_count <- as.data.table(tab)
  names(tab_count) = c("values", "change", "N")
  tab_count$label = lab
  tab_count$restriction <- restriction
  tab_count$valnum <- rep(1:(nrow(tab_count)/3),3)
  tab_count$labnum <- labnum
  tab_count
}

tab_display_signchange_perc <- function(dt, var, lab, restriction, labnum) {
  tab <- table(dt[[var]], dt[["signchange_lab"]])
  tab_count <- as.data.table(tab)
  tab_perc <- as.data.table(prop.table(tab, margin = 1))
  names(tab_perc) = c("values", "change", "N")
  tab_perc$label = lab
  tab_perc$restriction <- restriction
  tab_perc$valnum <- rep(1:(nrow(tab_perc)/3),3)
  tab_perc$labnum <- labnum
  tab_perc
}

table_creator_signchange <- function(dt, restriction, display = c("count", "perc", "both", "count_long")) {
  signchange_counts <- rbind(
    tab_display_signchange_count(dt, "part_age_group_labs", "Age groups", restriction, labnum = 1),
    tab_display_signchange_count(dt, "part_gender", "Gender", restriction, labnum = 2),
    tab_display_signchange_count(dt, "part_employed", "Employed", restriction, labnum = 3),
    tab_display_signchange_count(dt, "part_social_group", "Socio-economic status", restriction, labnum = 4)
  )
  
  signchange_perc <- rbind(
    tab_display_signchange_perc(dt, "part_age_group_labs", "Age groups", restriction, labnum = 1),
    tab_display_signchange_perc(dt, "part_gender", "Gender", restriction, labnum = 2),
    tab_display_signchange_perc(dt, "part_employed", "Employed", restriction, labnum = 3),
    tab_display_signchange_perc(dt, "part_social_group", "Socio-economic status", restriction, labnum = 4)
  )
  
  signchange_perc$perc <- round(signchange_perc$N*100,2)
  signchange_perc$N <- NULL
  cast_count <- dcast(signchange_counts, labnum + valnum + restriction + label + values ~ change, value.var = "N")
  cast_perc <- dcast(signchange_perc, labnum + valnum + restriction + label + values ~ change, value.var = "perc")
  
  
  cast_count$total <- cast_count$Decreased + cast_count$Same + cast_count$Increased
  
  if(display == "count"){
    cast_count
  }else if(display == "perc"){
    cast_perc
  }else if(display == "count_long"){
    signchange_counts
  }else if(display == "both"){
    data.table(
      labnum = cast_count$labnum,
      valnum = cast_count$valnum,
      Restriction = cast_count$restriction,
      Label = cast_count$label,
      Value = cast_count$values,
      Decreased = paste0(cast_count$Decreased, " (", round(cast_perc$Decreased,2),"%)"),
      Same = paste0(cast_count$Same, " (", round(cast_perc$Same, 2),"%)"),
      Increased = paste0(cast_count$Increased, " (", round(cast_perc$Increased,2),"%)"),
      Total = cast_count$total
    )
  }
}





