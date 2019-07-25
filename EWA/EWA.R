# written by K. Garner, 2019
# this code is written to simulate behaviour, using the experience weighted model (Ouden et al, 2013, Neuron)
# by using this code, and pre-defining the parameters that would typically be obtained through model fitting,
# we can generate data knowing "ground truth" and therefore check the model fitting software recovers the correct parameters

# this code is written for readability over effieciency
rm(list=ls())
###############################################################################################################################
# define model functions
ewa_update <- function(experience_decay_factor, payoff_decay_factor, last_experience_weight, last_choice_value, previous_outcome){
  # given an experience decay factor, and  a payoff_decay _factor, compute the
  # experience weight and the value for the choice, given the value, weight and outcome
  # for the previous choice
  experience_weight = last_experience_weight * experience_decay_factor + 1
  choice_value = (last_choice_value * payoff_decay_factor * last_experience_weight + previous_outcome) / experience_weight
  choice_value
}

action_selection <- function( choice_value, other_choice_value, inverse_temperature ){
  # compute the probability of choice, given the value for that choice, the value for the other choice, and the 
  # inverse temperature parameter - 
  
  probability_choice = exp( inverse_temperature*choice_value  ) / sum( exp( inverse_temperature*choice_value ), exp( inverse_temperature*other_choice_value ) )
  probability_choice
}

get_trial_outcome <- function( choice, choice_probs ){
  # sample the reward distribution lying beneath the choice,
  if (choice == "blue"){
    idx = 1
  } else if (choice == "orange"){
    idx = 2
  }
  outcome = sample(c(1, 0), 1, prob = choice_probs[idx, ])
}


###############################################################################################################################
# now define parameters for simulation
###############################################################################################################################

# define the model parameters
# I have chosen to randomly sample them, given the prior distributions, taken from Ouder et al, 2013, Neuron
experience_decay_factor = rbeta(1, 1.2, 1.2)
payoff_decay_factor = rbeta(1, 1.2, 1.2)
inverse_temperature = rnorm(1, 0, 10)

# define weightings of probability of good or bad outcome for each choice
# assuming choice between two stimuli, a 'blue' and an 'orange'
choices = c("blue", "orange")
outcome_probs_blue = c(.8, .2) # blue is good
outcome_probs_orange = c(.2, .8) # orange is bad
outcome_probs = matrix(c(outcome_probs_blue, outcome_probs_orange), nrow = 2, ncol = 2)

# set up empty vectors to collect generated data
choice = c() # empty vector for choices
choice_values = c()
experience_weights = c()

outcomes = c() # empty vector for outcomes
ntrials = 50 # will do 50 before switching

###############################################################################################################################
# run simulation
###############################################################################################################################
for (i in 1:ntrials){
  
  if (i == 1){
    # assuming first choice is 50:50, 0 is blue, 1 is orange
    choice[i] = sample(choices, 1, prob = c(.5, .5) )
    choice_values[i] = .5 # because it is 50/50 at this point
    experience_weights[i] = .5
  }
  else {
    
    # get choice values and experience weights for updates
    tmp_choice_values = choice_values[c(max(which(choice == "blue")), max(which(choice == "orange")))] # this gets the last value for each choice (don't worry about the warning msg)
    tmp_choice_values[is.na(tmp_choice_values)] = .5 # just in case one has not assigned a value yet
    tmp_experience_weights = experience_weights[ c(max(which(choice == "blue")), max(which(choice == "orange")))  ]
    tmp_experience_weights[is.na(tmp_experience_weights)] = .5
    
    # convert each of these into the new weight value (i.e. apply update rule to last chosen, but pass through next value)
    choice_value_for_update = tmp_choice_values[ which(choices == choice[i-1])   ]
    experience_weight_for_update = tmp_experience_weights[ which( choices == choice[i-1]) ]
    new_choice_value = ewa_update( experience_decay_factor, payoff_decay_factor, experience_weight_for_update, choice_value_for_update, outcomes[i-1] )
    # assign to the choice values to be compared for action generation
    tmp_choice_values[ which(choices == choice[i-1])   ] = new_choice_value
    # feed in to get action probabilities
    this_trials_action_probs = c( action_selection( tmp_choice_values[1], tmp_choice_values[2], inverse_temperature ), action_selection(  tmp_choice_values[2], tmp_choice_values[1], inverse_temperature   ) )
    choice[i] = sample(choices, 1, prob = this_trials_action_probs)
  }
  
  # get trial outcome
  outcomes[i] = get_trial_outcome(choice[i], outcome_probs)
  
}

###############################################################################################################################
# make into a dataframe and save to wideform for further use
###############################################################################################################################
data <- data.frame(choices = choice, outcomes = outcomes)
write.csv(data, "example.csv")
