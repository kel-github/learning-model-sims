# written by K. Garner, 2019
# this code is written to simulate behaviour, using the experience weighted model (Ouden et al, 2013, Neuron)
# simulations occur over N iterations, and the output is saved to a csv file, as is the 'accuracy data' 
# - i.e. win->stick and lose->shift

# this code is largely written for readability over efficiency
# -----------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
source("EWA_SRL_functions.R") # where the magic happens
library(tidyverse)
library(cowplot)
# ----------------------------------------------------------------------------------------------------------------------------
# define simulation settings
# ----------------------------------------------------------------------------------------------------------------------------
set.seed(42) # meaning of life for random seed - makes the simulation reproducible 
N_sims_per_group <- 1000
N_trials_per_sim <- 500 # max number of trials, assuming the 11 stages of the task are not completed prior
# define groups and the model parameters in the order of the defined groups
parameters_list <- list()
parameters_list[["control"]] <- data.frame(experience_decay_factor=.2346, payoff_decay_factor=.2219, inverse_temperature=2.5435)
parameters_list[["spared"]] <- data.frame(experience_decay_factor=.2322, payoff_decay_factor=.1045, inverse_temperature=2.1049)
parameters_list[["impaired"]] <- data.frame(experience_decay_factor=.4264, payoff_decay_factor=.1164, inverse_temperature=1.8796)

# define choices and probabilities across reversals
choices <- c("blue", "orange")
outcome_probs <- list()
# work out best way to set the index for these in the simulation function
outcome_probs[["pre"]] <- matrix(c(.8, .2, .2, .8), nrow=2)
outcome_probs[["pre-rewards"]] <- c(1,0)
outcome_probs[["post"]] <- matrix(c(.4, .4, .2, .2, .2, .6), nrow=3)
outcome_probs[["post-rewards"]] <- c(6,2,0)

RV_block <- c(rep("pre", times=7), rep("post", times=4))

# ----------------------------------------------------------------------------------------------------------------------------
# run simulation
# ---------------------------------------------------------------------------------------------------------------------------
dat <- lapply(c("control", "spared","impaired"), function(x) run.multi.sims(N_trials_per_sim, parameters_list[[x]], choices, outcome_probs, RV_block, N_sims_per_group, x))
dat <- do.call(rbind, dat)

# ----------------------------------------------------------------------------------------------------------------------------
# score data for % trials to criterion, win->stay, lose->shift
dat$ws <- NA # for win->stay
dat$ls <- NA # for lose->shift
for (i in 2:length(dat$ws)) dat$ws[i] = if_else(dat$outcomes[i-1] == 1 & dat$choices[i] == dat$choices[i-1], 1, 0) # score the win->stays
for (i in 2:length(dat$ws)) dat$ws[i] = if_else(dat$outcomes[i-1] == 0, NA,  as.logical(dat$ws[i])) # replace with NA when the previous value was not a win
for (i in 2:length(dat$ws)) dat$ws[i] = if_else(dat$s[i-1]!=dat$s[i], NA, as.logical(dat$ws[i]))

for(i in 2:length(dat$ls)) dat$ls[i] = if_else(dat$outcomes[i-1] != 1 & dat$choices[i] != dat$choices[i-1], 1, 0) # score lose->shift
for(i in 2:length(dat$ls)) dat$ls[i] = if_else(dat$outcomes[i-1] != 0, NA, as.logical(dat$ls[i]))
for (i in 2:length(dat$ls)) dat$ls[i] = if_else(dat$s[i-1]!=dat$s[i], NA, as.logical(dat$ls[i]))

plot.dat <- inner_join(dat %>% group_by(group, s, RV) %>%
                       transmute(trials_to_criterion=length(outcomes)) %>%
                       group_by(group, s) %>%
                       summarise(mu_t2c=mean(trials_to_criterion)),
                       dat %>% group_by(group, s) %>%
                       summarise(winstay=mean(ws, na.rm=TRUE),
                                 loseshft=mean(ls, na.rm=TRUE)),
                       by=c("group", "s"))
write.csv(plot.dat, "EWA_sim_behavResults.csv")

# --------------------------------------------------------------------------------------------------------------------------
# plot results of simulations

# plot average trials to criterion
plot.dat %>% mutate(group=factor(group, levels=c("control", "spared", "impaired"))) %>%
  ggplot(aes(x=mu_t2c, fill=group)) + geom_density(alpha=0.4) + xlab("average trials to criterion") + facet_wrap(~group, nrow=3) + theme_cowplot()
ggsave("EWAsim_trls2crit.pdf", dpi=300)
# plot average winstay
plot.dat %>% mutate(group=factor(group, levels=c("control", "spared", "impaired"))) %>%
  ggplot(aes(x=winstay, fill=group)) + geom_density(alpha=0.4) + xlab("proportion win->stay") + xlim(c(0,1)) + facet_wrap(~group, nrow=3) + theme_cowplot()
ggsave("EWAsim_prcntwinstay.pdf", dpi=300)
# plot average loseshift
plot.dat %>% mutate(group=factor(group, levels=c("control", "spared", "impaired"))) %>%
  ggplot(aes(x=loseshft, fill=group)) + geom_density(alpha=0.4) + xlab("proportion lose->shift") + xlim(c(0,1)) + facet_wrap(~group, nrow=3) + theme_cowplot()                                                                                               
ggsave("EWAsim_prcntloseshft.pdf", dpi=300)

# --------------------------------------------------------------------------------------------------------------------------
# save workspace for future reference
save(dat, plot.dat, file='EWA-sim.RData')