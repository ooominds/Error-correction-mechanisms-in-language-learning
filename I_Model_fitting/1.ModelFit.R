### This script produces two csv files: one that contains the fit accuracy for each combination 
### participant x param value and one that contains the best fit accuracy for each participant
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023). 


# Defining useful paths (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "I_Model_fitting")
DATA_TRAIN = paste0(TOP, "Data/Data_train.csv")
DATA_TEST = paste0(TOP, "Data/Data_test.csv")

# Set the working directory
setwd(WD)

# Loading libraries
library(ndl)
library(stringr) # To be able to manipulate strings
library(tictoc)

# Call the Widrow-Hoff learning function (equivalent to Rescorla-Wagner for discrete inputs)
source("all_learning.R")
### Loading other useful functions
source("TestResults.R")

options(width = 150)


###################
# Useful functions
###################

### Function that simulates one model (given alpha) on the data from one participant.
### The function returns the weight matrix, along with the activations, predicted softmax probabilities and 
### responses for the test events. It also outputs for each test event whether the model matches the subject's 
### response and the overall match rate
simulate_1RW_1subj = function(data_train, data_test, subj_num, alpha = 0.01){

  ### Construct expanded training set for the whole experiment
  Train.exp = data_train[data_train$SubNo == subj_num, c("Cues", "Outcomes")]
  # Create a sparse event file
  Train.exp.sparse = make_sparse(Train.exp[,c('Cues', 'Outcomes')])
  
  # Cue and outcome names
  cNames = sort(unique(unlist(strsplit(as.character(Train.exp[, 'Cues']), split='_'))))
  oNames = sort(unique(unlist(strsplit(as.character(Train.exp[, 'Outcomes']), split='_'))))
  N_outcomes = length(oNames)

  ### Learning
  # Running incremental R-W model and extract the final weight matrix
  WH = run_whoff_epoch(Train.exp.sparse, noTarget = N_outcomes, cNames = cNames, 
                       oNames = oNames, lRate = alpha)
  W_subj = WH$W

  ### Test results
  # Extract Test events
  Test = data_test[data_test$SubNo == subj_num, c("Cues", "Outcome_selected")]
  # Rename column
  colnames(Test)[colnames(Test)=="Outcome_selected"] = "Outcomes"
  Test$Cues = as.character(Test$Cues)
  Test$Outcomes = as.character(Test$Outcomes)
  # Get the vector of unique outcomes
  outcomes = c("mp", "np")
  #outcomes = sort(unique(as.character(Train.exp$Outcomes)))
  Result.test = TestResults(Test, outcomes, W_subj)

  ### Extract test results
  RW_predictions = Result.test$DetailResult
  # add activations and softmax probabilities 
  activs = as.data.frame(Result.test$activation)
  probabs = as.data.frame(Result.test$p_softmax)
  colnames(activs) = c("mp_activation", "np_activation")
  colnames(probabs) = c("mp_pred_prob", "np_pred_prob")
  data_activs = cbind(activs, probabs)
  data_activs$Cues = row.names(data_activs)
  data_activs = data_activs[, c(5, 1:4)]
  rownames(data_activs) = NULL # Remove row names
  RW_predictions = merge(RW_predictions, data_activs, by="Cues")

  return(list(weights = W_subj, 
              match_rate = Result.test$match_rate,
              RW_predictions = RW_predictions))
}

### Function that calculates the fit accuracy (i.e. match rate) for each combination of participant and parameter value
model_fit <- function(data_train, data_test, alpha_vect){

	griddf = expand.grid(alpha = alpha_vect)
  griddf = griddf[order(griddf$alpha), , drop = FALSE]

	# Extract number of participants
  N_subj = length(unique(data_train$SubNo))

  # Extract number of model simulations
  N_model = nrow(griddf)

  # dataframe where we will store all comparison results between participants and the models
  fit_all = data.frame(SubNo = rep(0, N_subj*N_model),
                        Model = rep(0, N_subj*N_model),
                        alpha = rep(99, N_subj*N_model),
                        match_rate = rep(99, N_subj*N_model))

  # Iteration num (to use for displaying run progress)
  iter_ij = 0 # Initialisation

  ### Compare each participant with all models
  for (j in 1:N_subj){

    ### Compare model output with participants' data
    for (i in 1:nrow(griddf)){

      # ith param set
      alpha_i = griddf$alpha[i]

      # participant's results
      sim_results = simulate_1RW_1subj(data_train = data_train, 
                                        data_test = data_test, 
                                        subj_num = j, 
                                        alpha = alpha_i)

      # match rate between the ith model and jth participant
      match_rate_ij = sim_results$match_rate

      # Add enteries to fit_all dataframe
      index_ij = N_model * (j-1) + i # row to fill in fit_all. This assume adjusted SubNo
      #index_ij = N_subj * (i-1) + j # row to fill in fit_all. This assume adjusted SubNo
      fit_all$Model[index_ij] = i
      fit_all$SubNo[index_ij] = j  
      fit_all$alpha[index_ij] = alpha_i
      fit_all$match_rate[index_ij] = match_rate_ij

      # Update iteration number
      iter_ij = iter_ij + 1

      # Message to display after every 100 iterations
      if ((iter_ij) %% 100 == 0) {
        cat((iter_ij), "iterations so far\n")
      }

		}

  }

  return(fit_all)

}

### Find the best parameter values 
find_best_model <- function(fit_all){

  # Extract number of participants and models
  N_subj = length(unique(fit_all$SubNo))
  N_model = length(unique(fit_all$Model))

  # dataframe where we will store the best parameters for each participant
  best_fit_all = data.frame(SubNo = rep(99, N_subj), 
                            alpha_best_median = rep(99, N_subj),
                            match_rate_best = rep(99, N_subj),
                            num_match_max = rep(0, N_subj))

  ### Compare each participant with all models
  for (j in 1:N_subj){

    # Add enteries to best_fit_all dataframe
    match_max_j = max(fit_all$match_rate[fit_all$SubNo==j]) # best match rate
    best_fit_all$SubNo[j] = j
    best_fit_all$alpha_best_median[j] = median(fit_all$alpha[fit_all$SubNo==j & fit_all$match_rate==match_max_j])
    best_fit_all$match_rate_best[j] = match_max_j
    best_fit_all$num_match_max[j] = length(which(fit_all$SubNo==j & fit_all$match_rate == match_max_j )) 

  }

  return(best_fit_all)

}

###################################
# Data preparation
###################################

### Load the behavioural data
data_train_behav = read.csv(file = DATA_TRAIN)  
data_test_behav = read.csv(file = DATA_TEST)  

# Remove events composed of 3 cues
data_test_behav = data_test_behav[-which(data_test_behav$Cues %in% c('FA1_FA2_FA3',
                                                                     'FP1_FP2_FP3',
                                                                     'MA1_MA2_MA3',
                                                                     'PC_MP1_MP2')), ]
data_test_behav = droplevels(data_test_behav)

####################################################################################################
# Simulate all models for all participants to get the fit accuracy for each possible parameter value
####################################################################################################

alpha_vect0 = seq(0, 0.5, 0.01); alpha_vect0 = alpha_vect0[alpha_vect0 != 0]

tic()
fit_all = model_fit(data_train = data_train_behav, 
                    data_test = data_test_behav,
                    alpha_vect = alpha_vect0)
toc()
write.csv(fit_all, file = "./Results/fit_all.csv", row.names = FALSE)

##############################################################
# Find the best parameter value for each subject + Evaluation
##############################################################

tic()
best_fit_all = find_best_model(fit_all)
toc()
write.csv(best_fit_all, file = "./Results/best_fit_all.csv", row.names = FALSE)



