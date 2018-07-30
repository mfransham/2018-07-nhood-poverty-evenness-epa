###################################################
# calculate indices of dissimilarity and isolation
###################################################

##############################
# index of dissimilarity
##############################

dissimilarity <- function(groupA, groupB) {
  summand <- abs((groupA/sum(groupA) - groupB/sum(groupB)))
  index <- 0.5 * sum(summand)
  return(index)
}

################################
# D - Massey and Denton formula
# gives same result
################################

dissim.MD <- function(groupA, poptotal) {
  t_i <- poptotal
  p_i <- groupA / poptotal
  P <- sum(groupA) / sum(poptotal)
  T_all <- sum(poptotal)
  index <- 0.5 * sum(t_i * abs(p_i - P) ) / (T_all * P * (1-P) )
} 

#####################################
## adjusted index of dissimilarity
#####################################

# see code/adjustedD.R
# makes hardly any difference for all ages

dissimilarityAdj <- function(groupA, groupB) {
  summand <- abs((groupA/sum(groupA) - groupB/sum(groupB)))
  Db <- 0.5 * sum(summand)
  totalPopn <- (sum(groupA) + sum(groupB))
  n <- as.integer(totalPopn / length(groupA))
  p = sum(groupA) / totalPopn
  Dc = 0 
  for (x in c(0:n)) {
    Dc <-  Dc + dbinom(x, size = n, prob = p) * abs(x/n - p) 
  }
  Dc <-  Dc / (2*p*(1-p))
  J <- 1 - exp( (-3*Db/Dc) + 2.8) 
  Dadj <- J * Db
  return(Dadj)
}

######################
# index of isolation
######################

isolation <- function(groupA, poptotal) {
  summand <- (groupA/sum(groupA)) * (groupA/poptotal)
  index <- sum(summand)
  return(index)
}

#####################################
# Gini index - Massey and Denton
# this gives different results to using ineq::Gini
#####################################

Gini.MD <- function(groupA, poptotal) {
 # variables
 t <- poptotal
 p <- groupA/poptotal
 p <- ifelse(is.na(p), 0, p)
 T_all <- sum(poptotal)
 P <- sum(groupA)/sum(poptotal)
 # do the sums
 summand <- 0
 for (i in 1:length(groupA) ) {
   summand <- summand +
     sum(t[i] * t * abs(p[i] - p) )
 }
 denominator <- (2 * T_all^2 * P * (1-P) )
 return(summand / denominator)
}

#####################
# Atkinson index
#####################

# formula as per Massey and Denton (1988) 
atkinson <- function(groupA, poptotal, shapeParam) {
  # set variables
  b <- shapeParam
  t <- poptotal
  p <- ifelse(t > 0, groupA/t, 0)
  p <- ifelse(p>1, 1, p) # to fix one case in 2006 where p>1
  T_all <- sum(poptotal)
  P <- sum(groupA)/sum(poptotal)
  # do the maths
  summand <- sum( ( (1-p)^(1-b) ) * (p^b) * t / (P * T_all) )
  summand_exp <- abs(summand)^(1 / (1-b) )
  multiplier <- (P / (1-P) )
  return(1 - (multiplier*summand_exp) )
}

#########################
# coverage and efficiency
#########################

# the choice of ties.method matters in the calculation of coverage rate
# 'max' chosen here is a more inclusive method than 'min', and results in slightly higher coverage rates
# the pattern is broadly the same however
# use of 'min', the default for dplyr::percent_rank, results in different values
coverage <- function(number_poor, percent_poor, highest_defn) {
  totalpoor <- sum(number_poor)
  percent_poor_rank <- 1 - ( rank(percent_poor, ties.method = "max") / length(percent_poor) )
  totalpoor_highpov <- sum(number_poor * ifelse(percent_poor_rank <= highest_defn, 1, 0))
  return(totalpoor_highpov / totalpoor)
}

# efficiency 
efficiency <- function(number_poor, total_popn, percent_poor, highest_defn) {
  percent_poor_rank <- 1 - ( rank(percent_poor, ties.method = "min") / length(percent_poor) )
  poor_highpov <- sum(number_poor * ifelse(percent_poor_rank <= highest_defn, 1, 0) )
  popn_highpov <- sum(total_popn * ifelse(percent_poor_rank <= highest_defn, 1, 0) )
  return(poor_highpov / popn_highpov)
}
