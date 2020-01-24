# written by K. Garner, 2020
# this code simulates possible outcomes for experience weights, given decay factors
rm(list=ls())
# PACKAGES AND SOURCE FILES
# -------------------------------------------------------------------------------------------------------------------------
# install required packages
# install.packages(c("rstudioapi", "tidyverse")) # uncomment and run this line if you don't have the packages installed

# load packages
library(tidyverse)
# set wd to current 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# FUNCTIONS
# -------------------------------------------------------------------------------------------------------------------------
update_experience_weight <- function(previous_experience_weight, decay_factor){
  # use this function to compute one experience weight update, given a decay factor
  experience_weight = (previous_experience_weight * decay_factor) + 1
}

run_simulations <- function(decay_factor, ntrials){
  # use this function to update experience weights over the total number of ntrials and a given prior decay factor
  experience_weights_across_trials = rep(0, times=ntrials) # set initial experience weight to 0, given 
  # description of parameter around equation 2.1 in https://onlinelibrary.wiley.com/doi/epdf/10.1111/1468-0262.00054
  for (i in 2:length(experience_weights_across_trials)){
    experience_weights_across_trials[i] = update_experience_weight( experience_weights_across_trials[i-1], decay_factor)
  }
  experience_weights_across_trials = data.frame(t = 1:length(experience_weights_across_trials),
                                                weights=experience_weights_across_trials,
                                                prior = decay_factor)
}

# SET DECAY FACTORS TO TEST OVER
# ------------------------------------------------------------------------------------------------------------------------
# going to test 10 possible decay factors from 0 < x < 1, given the prior set by den Ouden et al (2013)
# plotting prior
prior_dat <- data.frame(x=seq(-1, 2, by=.01)) %>%
                        mutate(prior=dbeta(x, 1.2, 1.2)) 
with(prior_dat, plot(x, prior, type="l", main="prior distribution for decay parameter", xlab="experience weight", ylab="probability"))
experience_decay_factors = seq(0.1, .9, by=.1)

# RUN EACH DECAY FACTOR OVER 1000 TRIALS, FOR THE SPECIFIED EXP DECAY FACTORS
# ------------------------------------------------------------------------------------------------------------------------
ntrials = 1000
simulation_output = do.call(rbind, lapply(experience_decay_factors, run_simulations, ntrials=ntrials)) 
simulation_output$prior <- as.factor(simulation_output$prior)

# PLOT EACH SIMULATION
# ------------------------------------------------------------------------------------------------------------------------
simulation_output %>% ggplot(aes(x=t, y=weights, group=prior)) + geom_line(aes(color=prior)) + facet_wrap(~prior)

