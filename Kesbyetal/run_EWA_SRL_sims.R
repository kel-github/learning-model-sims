# written by K. Garner, 2019
# this code is written to simulate behaviour, using the experience weighted model (Ouden et al, 2013, Neuron)
# simulations occur over N iterations, and the output is saved to a csv file, as is the 'accuracy data' 
# - i.e. win->stick and lose->shift

# -----------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
source("EWA_SRL_functions.R") # where the magic happens
library(tidyverse)
library(cowplot)
library(DescTools)

fname = "EWA-sim.RData" # name of saved workspace
if( file.exists(fname) ){ # then you have run the sim already so just load the workspace
  
  load(fname)
} else {
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
  
  
  plot.dat <- inner_join(dat %>% group_by(group, s, RV) %>% filter(RV>1 & RV<7) %>%
                           transmute(trials_to_criterion=length(outcomes)) %>%
                           group_by(group, s) %>%
                           summarise(mu_t2c=mean(trials_to_criterion)),
                         dat %>% group_by(group, s) %>% filter(RV>1 & RV<7) %>% # KG just added
                           summarise(winstay=mean(ws, na.rm=TRUE),
                                     loseshft=mean(ls, na.rm=TRUE)),
                         by=c("group", "s"))
  write.csv(plot.dat, "EWA_sim_behavResults_RV2to6.csv")
}

# --------------------------------------------------------------------------------------------------------------------------
# COMPARE OBSERVED DATA TO RESULTS
# --------------------------------------------------------------------------------------------------------------------------
observed.dat <- read.csv("observed-data.csv", header=T)
observed.dat <-  observed.dat %>% mutate(group=recode(group, "Con" = "control", "PP spared" = "spared", "PP impaired" = "impaired")) %>%
                                  mutate(group=fct_relevel(group, "control", "spared", "impaired"))
observed.dat$X <- NULL
names(observed.dat) <- c("group", "mu_t2c", "winstay", "loseshft")

# --------------------------------------------------------------------------------------------------------------------------
# plot results of simulations
# --------------------------------------------------------------------------------------------------------------------------

## set aestheticss
grp.labs <- c("Control intact", "Persistent psychosis intact", "Persistent psychosis impaired")
names(grp.labs) <- c("control", "spared", "impaired")
grp.cols <- c("#FFFFFF", "#badcff", "#2190ff")
# plot average trials to criterion
plot.dat %>% mutate(group=factor(group, levels=c("control", "impaired", "spared"))) %>%
             mutate(group = fct_relevel(group, "control", "spared", "impaired")) %>%
              ggplot(aes(x=mu_t2c,fill=group)) + geom_density(alpha=0.4) + xlab("average trials to criterion") + 
              geom_point(data=observed.dat, aes(x=mu_t2c, y=0.002, fill=group, group=group)) +
              facet_wrap(~group, nrow=3, labeller = labeller(group=grp.labs)) + 
              scale_fill_manual(values=grp.cols) +
              theme_cowplot() +
              theme(legend.position = "none")
ggsave("EWAsim_trls2crit.pdf", dpi=300)

# plot average winstay
plot.dat %>% mutate(group=factor(group, levels=c("control", "spared", "impaired"))) %>%
             mutate(group = fct_relevel(group, "control", "spared", "impaired")) %>%
              ggplot(aes(x=winstay, fill=group)) + geom_density(alpha=0.4) + xlab("proportion win->stay") + 
              geom_point(data=observed.dat, aes(x=winstay, y=0.002, fill=group, group=group)) +
              xlim(c(0,1)) + 
              facet_wrap(~group, nrow=3, labeller = labeller(group=grp.labs)) + 
              scale_fill_manual(values=grp.cols) + 
              theme_cowplot() +
              theme(legend.position = "none") 
ggsave("EWAsim_prcntwinstay.pdf", dpi=300)

# plot average loseshift
plot.dat %>% mutate(group=factor(group, levels=c("control", "spared", "impaired"))) %>%
             mutate(group = fct_relevel(group, "control", "spared", "impaired")) %>%
              ggplot(aes(x=loseshft, fill=group)) + geom_density(alpha=0.4) + xlab("proportion lose->shift") +
              geom_point(data=observed.dat, aes(x=loseshft, y=0.002, fill=group, group=group)) +
              xlim(c(0,1)) + 
              facet_wrap(~group, nrow=3, labeller = labeller(group=grp.labs)) + 
              scale_fill_manual(values=grp.cols) + 
              theme_cowplot() +
              theme(legend.position = "none")   
ggsave("EWAsim_prcntloseshft.pdf", dpi=300)

# --------------------------------------------------------------------------------------------------------------------------
# STATS TESTING
# --------------------------------------------------------------------------------------------------------------------------
plot.dat <- plot.dat %>% mutate(group=factor(group, levels=c("control", "spared", "impaired"))) %>%
                         mutate(group = fct_relevel(group, "control", "spared", "impaired"))             
DunnettTest(mu_t2c~group, data=plot.dat, control="control")
# Dunnett's test for comparing several treatments with a control :  
#     95% family-wise confidence level
# 
# $control
# diff    lwr.ci    upr.ci    pval    
# spared-control    5.251753  4.265309  6.238197 6.7e-16 ***
# impaired-control 18.225757 17.239313 19.212201 7.8e-16 ***
# 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

DunnettTest(winstay~group, data=plot.dat, control="control")
# Dunnett's test for comparing several treatments with a control :  
#     95% family-wise confidence level
# 
# $control
# diff       lwr.ci       upr.ci    pval    
# spared-control   -0.002027438 -0.007062686  0.003007809  0.5750    
# impaired-control -0.034352141 -0.039387389 -0.029316894 1.1e-15 ***
# 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

DunnettTest(loseshft~group, data=plot.dat, control="control")
# Dunnett's test for comparing several treatments with a control :  
#     95% family-wise confidence level
# 
# $control
# diff     lwr.ci     upr.ci    pval    
# spared-control   0.05771943 0.05163959 0.06379927 4.4e-16 ***
# impaired-control 0.08137987 0.07530003 0.08745971 < 2e-16 ***
# 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# --------------------------------------------------------------------------------------------------------------------------
# save workspace for future reference
save(dat, plot.dat, file='EWA-sim.RData')