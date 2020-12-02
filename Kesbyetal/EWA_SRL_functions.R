
# EWA Model Functions
#------------------------------------------------------------------------------------------------------------------------------
# define model functions
ewa_update <- function(experience_decay_factor, payoff_decay_factor, last_experience_weight, last_choice_value, previous_outcome){
  # given an experience decay factor, and  a payoff_decay _factor, compute the
  # experience weight and the value for the choice, given the value, weight and outcome
  # for the previous choice
  experience_weight = last_experience_weight * experience_decay_factor + 1
  choice_value = (last_choice_value * payoff_decay_factor * last_experience_weight + previous_outcome) / experience_weight
  out = list( choice_value = choice_value, experience_weight = experience_weight )
  out
}

action_selection <- function( choice_value, other_choice_value, inverse_temperature ){
  # compute the probability of choice, given the value for that choice, the value for the other choice, and the 
  # inverse temperature parameter - 
  
  probability_choice = exp( inverse_temperature*choice_value  ) / sum( exp( inverse_temperature*choice_value ), exp( inverse_temperature*other_choice_value ) )
  probability_choice
}

get_trial_outcome <- function( choice, rewards, choice_probs ){
  # sample the reward distribution lying beneath the choice
  # choice = the choice of the agent on that trial
  # rewards = the rewards available (N matches choice_probs)
  # choice_probs = the reward probabilities, given the choice
  outcome = sample(rewards, 1, prob = choice_probs)
}

# Simulation Functions
#------------------------------------------------------------------------------------------------------------------------------
run.experiment.sim <- function(ntrials, parameters, choice_poss, outcome_probs, RV_block, simNumber){
  # this function runs one simulation experiment
  
  # ntrials = max number of trials in a sim
  # parameters = Rho, phi and beta for the simulation
  # choice_poss = what are the choices the agent can make? e.g. c("blue","orange")
  # outcome_probs = the list of possible outcome probabilities for each stage of the experiment, see run_EWA_SRL_sims.R for format
  # RV_block = the structure of the reversals
  # simNumber = which simnumber is this? 
  
  # set up parameters and colection matrices
  choice <- rep(NA, ntrials) # set empty vector to collect choice data
  next_trials_action_probs <- matrix(NA, ntrials, nrow=2) # set up empty matrix to collect the action probabilities 
  next_trials_action_probs[,1] <- c(.5,.5) # set them to equal on the 1st trial
  rownames(next_trials_action_probs) <- choice_poss # name the rows by the choices, for clarity
  choice_values <- c(0.5, 0.5)
  names(choice_values) <- choice_poss
  experience_weights <- c(1,1)
  names(experience_weights) <- choice_poss
  outcomes <- rep(NA, ntrials)
  
  RV <- rep(NA, ntrials)
  RV[1] <- 0 # reversal number
  maxRV <- 11
  correct_count <- rep(NA, ntrials)  # will move the RV number each time it gets up to 6

  max_correct <- 6 # the max number of correct before switch
  
  for (i in 1:ntrials){ 
    
    # check the RV number, if max exit the loop #######################################
    if (RV[i] == maxRV) break
    if (i == ntrials) break # same for trial number
  
    # set indexes for trial, using RV value ##############################################
    these_outcome_probs <- outcome_probs[[RV_block[RV[i]+1]]]
    these_outcome_rewards <- outcome_probs[[paste(RV_block[RV[i]+1], "-rewards", sep="")]]
    if (as.numeric(RV[i]+1) %% 2){
      colnames(these_outcome_probs) <- choice_poss
    }  else {
      colnames(these_outcome_probs) <- choice_poss[c(2,1)]
    }

     # get the choice made on trial i ######################################################
     choice[i] = sample(choice_poss, 1, prob = next_trials_action_probs[,i])
     # was the choice correct?, if so, increase score
     if (max(these_outcome_probs[,choice[i]]) == tail(these_outcome_probs[,choice[i]],1)){ # if this is true, then the agent picked the 'incorrect' response{
          correct_count[i] <- 0
      } else {
        if (i == 1){ correct_count[i] = 1
        } else { correct_count[i] <- correct_count[i-1] + 1
        }
      }
     if (correct_count[i] == max_correct) {
       RV[i+1] <- RV[i] + 1
       correct_count[i] <- 0 # this means that the max correct count returned in the dataframe will be 5, but it aids counting 
     } else {
       RV[i+1] <- RV[i]
     }
    
    # get the outcome for this trial
    outcomes[i] <- get_trial_outcome( choice[i], these_outcome_rewards, these_outcome_probs[,choice[i]])
     
    # update the choice values and the experience weights based on the previous outcome ##################################
    updates <- ewa_update(parameters[,"experience_decay_factor"], parameters[,"payoff_decay_factor"], experience_weights[choice[i]], choice_values[choice[i]], outcomes[i])
    choice_values[choice[i]]  <- updates$choice_value 
    experience_weights[choice[i]] <- updates$experience_weight # now the choice and experience weights have been updated, given the outcome on this trial
  
    # now get action probabilities for next trial (iterating through a matrix and keeping old values is a hangover from previous code)
    next_trials_action_probs[choice[i],i+1] <- action_selection( choice_values[choice[i]],  choice_values[names(choice_values)!=choice[i]], parameters[,"inverse_temperature"] )
    next_trials_action_probs[names(choice_values)[names(choice_values)!=choice[i]],i+1] <-  action_selection( choice_values[names(choice_values)!=choice[i]], choice_values[choice[i]],  parameters[,"inverse_temperature"]  ) 
    }
  
    # collate the results of this trial as a dataframe
    out <- data.frame( choices = choice, outcomes = outcomes, correct_count = correct_count, RV = RV, s = simNumber  )
    # remove na rows
    out %>% drop_na()
} 

run.multi.sims <- function(ntrials, parameters, choice_poss, outcome_probs, RV_block, nsims, group){
  # run nsims x simulations for group
  dat <- lapply(c(1:nsims), function(x) run.experiment.sim(ntrials=ntrials, parameters=parameters,
                                                           choice_poss=choice_poss, outcome_probs=outcome_probs, RV_block=RV_block, simNumber=x))
  dat <- do.call(rbind, dat)
  dat$group <- group
  dat
}

  
