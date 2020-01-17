# written by K. Garner, 2019
# this code is written to simulate behaviour, using theRP model (Ouden et al, 2013, Neuron)
# by using this code, and pre-defining the parameters that would typically be obtained through model fitting,
# we can generate data knowing "ground truth" and therefore check the model fitting software recovers the correct parameters

# this code is written for readability over effieciency
rm(list=ls())
###############################################################################################################################
# define model functions
rp_update <- function(punishment_learn_rate, reward_learn_rate, choice_value_last_trial, outcome_last_trial){
  # update the choice value for the choice made on the last trial, given the outcome of the last trial by adding to the value for the given choice on the 
  # previous trial, the punishment learning rate weighted by the difference between the outcome and the choice value from the last trial (if it was a punishment trial), or the reward learning rate 
  # weighted by the difference between the previous value and outcome

  # first assess whether the current trial is a punishment (0) or a reward trial (1), and set punishment and reward learn rates appropriately
  if (!any(as.logical(outcome_last_trial))) {
        punish = punishment_learn_rate
        reward = 0
  } else {
        punish = 0
        reward = reward_learn_rate
  }
  choice_value_this_trial = choice_value_last_trial + ( punish * (  outcome_last_trial - choice_value_last_trial ) ) + ( reward * (  outcome_last_trial - choice_value_last_trial ) )
  choice_value_this_trial
}


action_selection <- function( choice_value, other_choice_value, inverse_temperature ){
  # compute the probability of choice, given the value for that choice, the value for the other choice, and the 
  # inverse temperature parameter - 
  
  probability_choice = exp( inverse_temperature*choice_value  ) / sum( exp( inverse_temperature*choice_value ), exp( inverse_temperature*other_choice_value ) )
  probability_choice
}

get_trial_outcome <- function( choice, outcome_probs ){
  # sample the reward distribution lying beneath the choice,
  if (choice == "blue"){
    idx = 1
  } else if (choice == "orange"){
    idx = 2
  }
  outcome = sample(c(1, 0), 1, prob = outcome_probs[, idx])
  outcome
}

###############################################################################################################################
# now define parameters for simulation
###############################################################################################################################
# define the model parameters
# I have chosen to randomly sample them, given the prior distributions, taken from Ouder et al, 2013, Neuron
punishment_learn_rate = rbeta(1, 1.2, 1.2)
reward_learn_rate = rbeta(1, 1.2, 1.2)
inverse_temperature = 1 #rnorm(1, 0, 10)

# define weightings of probability of good or bad outcome for each choice
# assuming choice between two stimuli, a 'blue' and an 'orange'
choices = c("blue", "orange")
outcome_probs_blue = c(.8, .2) # blue is good
outcome_probs_orange = c(.5, .5) # orange is even
outcome_probs = matrix(c(outcome_probs_blue, outcome_probs_orange), nrow = 2, ncol = 2)

# define the number of trials and switch points
ntrials = 100
switch = c(50) # this is an n length vector for n switches - eg. to switch on trials 50 % 100: switch = c(50, 100)

# start dataframe to collect data
data = data.frame( t = c(1, rep(NA, ntrials-1 )),
                   blue_choice_value = c(.5, rep(NA, ntrials-1)), 
                   orange_choice_value = c(.5, rep(NA, ntrials-1)),
                    p_blue = rep(NA, ntrials),
                    p_orange = rep(NA, ntrials),
                    this_trial_action = rep(NA, ntrials),
                    this_trial_outcome = rep(NA, ntrials))

###############################################################################################################################
# run simulation
###############################################################################################################################
for (i in 1:ntrials){
  
  # are the probabilities switching?
  if (i %in% switch) outcome_probs = outcome_probs[, c(2,1)]
  # is it the first trial?, if not, update based on last trial
  if (i > 1){
    # do update here
    data$t[i] = i # assign new trial number
    last_choice = choices[match( data$this_trial_action[i-1], choices)] # get last choice
    last_unchosen = choices[which( data$this_trial_action[i-1] != choices  )] # same, but for unchosen
    # now get the value updates for chosen and unchosen
    data[i, charmatch(last_choice, colnames(data))] = rp_update( punishment_learn_rate, reward_learn_rate, data[i-1, charmatch(last_choice, colnames(data))], data$this_trial_outcome[i-1] )
    data[i, charmatch(last_unchosen, colnames(data))] =  data[i-1, charmatch(last_unchosen, colnames(data))]
  }
  # generate choice based on current trial's values
  data$p_blue[i] = with(data, action_selection(blue_choice_value[i], orange_choice_value[i], inverse_temperature) )
  data$p_orange[i] = 1-data$p_blue[i]
  data$this_trial_action[i] = sample( choices, 1, prob=c(data$p_blue[i], data$p_orange[i]) )
  data$this_trial_outcome[i] = with(data, (get_trial_outcome( this_trial_action[i], outcome_probs ) ) )   
    
}


###############################################################################################################################
# make into a dataframe and save to wideform for further use
###############################################################################################################################
write.csv(data, "RP_example.csv")
