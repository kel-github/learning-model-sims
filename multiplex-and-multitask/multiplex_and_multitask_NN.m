%%% written by K. Garner, July 2020
%%% this code is a simple demonstration of the principles underlying the
%%% neural network model of Feng et al, 2014. Multitasking vs multiplexing:
%%% Toward a normative account of limitations in the simultaneous execution
%%% of control-demanding behaviours. Cogn Affect Behav Neurosci. 
%%% doi: 10.3758/s13415-013-0236-9

clear all


%% define inputs, outputs and the weight matrix that connects them (see Fig
% 3 of the paper
n.units = 20;
n.control = 10;
n.crosstalk = 4; % pathway overlap, or F, in the paper - i.e. how many other output pairs
                 % will the input pair map to?
n.p_congruent = 0.75; % proportion of irrelevant crosstalk pathways that are incongruent
n.incongruent = n.p_congruent*n.crosstalk;
n.trials = 100;

% note - authors also implemented changes to the network size, which I am
% not doing here

model.inputs = repmat([-.5, .5], 1, n.units/2 );
model.outputs = zeros( 1, n.units );
model.weights = zeros( n.units, n.units );

pathways = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
irrel_idx = zeros( n.units, n.crosstalk*2 ); % index of irrelevant pathways for each unit
cong_idx = ones( n.units, n.units ); % matrix denoting whether the path is congruent (+1) or incongruent (-1)

for i = 1:length(pathways)
    
    model.weights(pathways(i):pathways(i)+1, pathways(i)) = [.5, -.5];
    model.weights(pathways(i):pathways(i)+1, pathways(i)+1) = [-.5, .5];
    
    if any(n.crosstalk)
        
       irrel_pathways = pathways(pathways~=pathways(i)); % get the other potential pathways
       irrel_pathways = irrel_pathways(randperm(length(irrel_pathways), n.crosstalk));
       irrel_idx(pathways(i), :) = sort([irrel_pathways, irrel_pathways+1]);
       irrel_idx(pathways(i)+1, :) = sort([irrel_pathways, irrel_pathways+1]);
       
       if any(n.incongruent)
         
            inc_idx = irrel_pathways(randperm(length(irrel_pathways), n.incongruent)); % which pathways to make incongruent
            inc_idx = sort([inc_idx, inc_idx + 1]); % to get each connection
            cong_idx(inc_idx, pathways(i)) = cong_idx(inc_idx, pathways(i)).*-1;
            cong_idx(inc_idx, pathways(i)+1) = cong_idx(inc_idx, pathways(i)+1).*-1;      
       end
       
       for iIrrel = 1:n.crosstalk
           
          model.weights(irrel_pathways(iIrrel):irrel_pathways(iIrrel)+1, pathways(i)) = [.5, -.5];
          model.weights(irrel_pathways(iIrrel):irrel_pathways(iIrrel)+1, pathways(i)+1) = [-.5, .5];
       end
    end
end
model.weights = model.weights .* cong_idx;

% and now variables to collect info over trials
trials.weights = repmat(model.weights, 1, 1, n.trials);
trials.inputs = repmat(model.inputs, 1, 1, n.trials);
trials.outputs = repmat(model.outputs, 1, 1, n.trials);
trials.control = zeros(1, n.control, n.trials);
for iCont = 1:n.trials
    trials.control(1,:,iCont) = rand(1, n.control); % randomly select some control numbers for each trial (instead of optimising
end
trials.error = zeros(1, n.trials);

% ----------------------------------------------------------------------------------------------

%% trial loop
for iTrial = 1:n.trials
    
    this_trials_weights = trials.weights(:, :, iTrial); 
        
    % compute value of output units for that trial
    % first normalise weights
    this_trials_weights = this_trials_weights*(1/(n.crosstalk + 1));
    this_trials_control_scalar = repelem(trials.control(:,:,iTrial), 2);
    % apply control to input values, calculate controlled output values
    trials.outputs(:, :, iTrial) = this_trials_control_scalar.*trials.inputs( :, :, iTrial)*this_trials_weights;

    
    % calculate mse
    this_trials_error = (1/length(this_trials_control_scalar)) * sum((this_trials_control_scalar.*trials.outputs(:, :, iTrial)-trials.inputs(:, :, iTrial)).^2);   
    trials.error(iTrial) = this_trials_error;    
    % here an objective function would select new control values in order to
    % minimise error. the final convergence on a control vector would be
    % checked for control values > 0.5. It would be deemed that these tasks
    % would have been executed.
 
end




