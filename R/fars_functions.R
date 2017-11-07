
# three functions:
# perc_cis
# test_trend_ca
# test_trend_log_reg

library(dplyr)
library(stats) 
library(dplyr) 
library(broom)

#####################################################

perc_cis <- function(x = x, n = n){
  library(scales)
  
  proportion <-  x/n
  est_of_stand <-  sqrt((proportion*(1 - proportion))/n)
  low_CI <- proportion-(1.96*est_of_stand)
  upp_CI <- proportion+(1.96*est_of_stand)

prop <- percent(proportion)
low_conf <- percent(low_CI)
upp_conf <- percent(upp_CI)


perc_cis_vec <- paste(prop, " ", "(", low_conf, ",", " ",upp_conf, ")", sep = "")
return(perc_cis_vec)
}

perc_cis(x = 9000, n = 23000)
# [1] "39.1% (38.5%, 39.8%)"

#####################################################

test_trend_ca <- function(drug, data = clean_fars) {
  if(drug == "Nonalcohol"){
    clean <- clean_fars %>%
      filter(drug_type != "Alcohol") %>%
      group_by(unique_id, year) %>% 
      summarize(positive_test = sum(positive_for_drug, na.rm = TRUE), 
                positive = any(positive_test > 0),
                total_tests = length(!is.na(positive_for_drug))) %>% 
      ungroup() %>% 
      group_by(year) %>%
      summarize(total_tests = sum(total_tests), positive = sum(positive)) 
  } else{
    clean <- clean_fars %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                total_tests = sum(!is.na(positive_for_drug)))
  }
  ca_drug_type <- prop.trend.test(x = clean$positive,
                                  n = clean$total_tests)
  Z <-  sqrt(ca_drug_type$statistic)
  p.value <- ca_drug_type$p.value
  
  test_trend_table <- tibble::tibble(Z, p.value)
  
  test_trend <- test_trend_table %>% 
    mutate(Z= signif(Z, digits = 2)) %>%
    mutate(Z = as.numeric(Z)) %>% 
    mutate(p.value = round(p.value, digits = 3)) %>%
    mutate(p.value = as.numeric(p.value))
  
  return(test_trend)
}

test_trend_ca(drug = "Alcohol")
test_trend_ca(drug = "Nonalcohol")
test_trend_ca(drug = "Stimulant")

#####################################################

test_trend_log_reg <- function(drug, data = clean_fars) {
    if(drug == "Nonalcohol"){
      to_test <- clean_fars %>%
        filter(!is.na(drug_type)) %>% 
        filter(drug_type != "Alcohol") %>% 
        group_by(unique_id, year) %>% 
        summarize(positive_for_drug = any(positive_for_drug))
      
} else{
      to_test <- clean_fars %>%
        filter(!is.na(drug_type)) %>%
        filter(drug_type == drug)
}
    log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
    log_reg_clean <- slice(tidy(log_reg), 2)
    Z <- log_reg_clean$statistic
    p.value <- log_reg_clean$p.value
    
    log_reg_table <- tibble::tibble(Z, p.value)
    
    log_reg_trend <- log_reg_table %>% 
      mutate(Z= signif(Z, digits = 2)) %>%
      mutate(Z = as.numeric(Z)) %>% 
      mutate(p.value = round(p.value, digits = 3)) %>%
      mutate(p.value = as.numeric(p.value))
    
   return(log_reg_trend)
}

test_trend_log_reg(drug = "Alcohol")
test_trend_log_reg(drug = "Nonalcohol")

