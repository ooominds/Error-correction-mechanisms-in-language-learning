### this script calculates the Leave-One-Out (LOO) accuracy for all participants and for all cue pair events.
### Then from that it calculate the LOO Cross-Validation (LOOCV) accurracy for each participant, by aggregating over all left-out events
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
              fit_acc = Result.test$match_rate,
              RW_predictions = RW_predictions))
}

### Function that calculates for a participant, the fit accuracy (i.e. match rate) for each parameter value with 
model_fit_1subj <- function(data_train, data_test, alpha_vect, subj_num){

  # Extract number of model simulations
  N_model = length(alpha_vect)

  # Dataframe where we will store all comparison results between participants and the models
  fit_subj = data.frame(SubNo = rep(0, N_model),
                       Model = rep(0, N_model),
                       alpha = rep(99, N_model),
                       fit_acc = rep(99, N_model))

  ### Compare model output with participants' data
  for (i in 1:N_model){

    # ith param set
    alpha_i = alpha_vect[i]

    # participant's results
    sim_results = simulate_1RW_1subj(data_train = data_train, 
                                     data_test = data_test, 
                                     subj_num = subj_num, 
                                     alpha = alpha_i)

    # match rate between the ith model and jth participant
    fit_acc_ij = sim_results$fit_acc

    # Add enteries to fit_subj dataframe
    fit_subj$Model[i] = i
    fit_subj$SubNo[i] = subj_num # back to the true SubNo 
    fit_subj$alpha[i] = alpha_i
    fit_subj$fit_acc[i] = fit_acc_ij

  }

  return(fit_subj)

}

### Find the best parameter values 
find_best_model_1subj <- function(fit_subj, subj_num){

  # Add enteries to best_fit_subj dataframe  
  fit_acc_max_j = max(fit_subj$fit_acc[fit_subj$SubNo==subj_num]) # best match rate
  alpha_best_median_j = median(fit_subj$alpha[fit_subj$fit_acc==fit_acc_max_j])
  fit_acc_best_j = max(fit_subj$fit_acc) # best match rate
  num_fit_acc_max_j = length(which(fit_subj$fit_acc == fit_acc_max_j)) 

  best_fit_subj = list(SubNo = subj_num, 
                       #alpha_best_mean = alpha_best_mean_j, 
                       alpha_best_median = alpha_best_median_j,
                       fit_acc_best = fit_acc_best_j, 
                       num_match_max = num_fit_acc_max_j)

  return(best_fit_subj)

}

model_fit_LOO <- function(data_train, data_test, alpha_vect){

  # Initialise start time to be able to compute run time
  start0 = tic()

	# Extract number of participants
  N_subj = length(unique(data_train$SubNo))

  # Extract number of model simulations
  N_model = length(alpha_vect)

  # Number of rows in the test dataset is equal to the number of rows in the final results dataset
  N_test_rows = nrow(data_test)

  # Convert column containing the events into a character column
  data_test$Cues = as.character(data_test$Cues)

  # Dataframe where we will store all comparison results between participants and the models
  fit_best_LOO = data.frame(SubNo = rep(0, N_test_rows),
                            DataTestID = rep(0, N_test_rows),
                            EventDropped = rep("E", N_test_rows),
                            alpha_best_median = rep(99, N_test_rows),
                            fit_acc_best = rep(99, N_test_rows))

  fit_best_LOO$EventDropped = as.character(fit_best_LOO$EventDropped)

  # Iteration num (to use also for displaying run progress)
  iter_ij = 0 # Initialisation
  start1 = tic()

  ### Compare each participant with all models
  for (j in 1:N_subj){

    ### Find number of events in the test data for the jth subject
    events_test_j = data_test$Cues[data_test$SubNo == j]
    N_events = length(events_test_j)

    ### Compare model output with participants' data
    for (i in 1:N_events){

      event_drop_i = events_test_j[i] # event to drop in the present iteration i
      data_test_LOO_i = data_test[-which(data_test$Cues %in% event_drop_i),]

      ### fit outputs for jth subject and ith LOO test dataset 
      fit_subj_j = model_fit_1subj(data_train, data_test_LOO_i, alpha_vect, j)

      # best fit outputs for the jth subject
      best_fit_subj_j = find_best_model_1subj(fit_subj_j, j)

      # Update iteration number
      iter_ij = iter_ij + 1

      # Add enteries to fit_best_LOO dataframe
      index_ij = iter_ij # row to fill in fit_best_LOO. This assumes adjusted SubNo
      fit_best_LOO$DataTestID[index_ij] = i
      fit_best_LOO$EventDropped[index_ij] = event_drop_i
      fit_best_LOO$SubNo[index_ij] = j 
      fit_best_LOO$alpha_best_median[index_ij] = best_fit_subj_j$alpha_best_median
      fit_best_LOO$fit_acc_best[index_ij] = best_fit_subj_j$fit_acc_best

      # Message to display after every 100 iterations
      if (iter_ij %% 10 == 0) {
        end1 = tic()
        cat(iter_ij, "iterations completed out of", N_test_rows, "in", end1-start1, "seconds\n")
      }

		}

  }

  end0 = tic()
  cat(N_test_rows, "iterations completed in", end0-start0, "seconds\n")

  return(fit_best_LOO)

}

### Find the best parameter values 
Compute_fit_LOOCV <- function(fit_best_LOO){

  # Extract number of participants and models
  N_subj = length(unique(fit_best_LOO$SubNo))

  # Dataframe where we will store the best parameters for each participant
  fit_LOOCV = data.frame(SubNo = rep(99, N_subj), 
                         fit_acc_LOOCV = rep(99, N_subj))

  ### Compare each participant with all models
  for (j in 1:N_subj){

    # Add enteries to fit_LOOCV dataframe
    fit_LOOCV$SubNo[j] = j
    fit_LOOCV$fit_acc_LOOCV[j] = mean(fit_best_LOO$fit_acc_best[fit_best_LOO$SubNo==j])

  }

  return(fit_LOOCV)

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

# This can be slow to run (took about 8 min to run on a laptop with Intel i7-1065G7 CPU)

alpha_vect0 = seq(0, 0.5, 0.01); alpha_vect0 = alpha_vect0[alpha_vect0 != 0]

tic()
fit_best_LOO = model_fit_LOO(data_train = data_train_behav,
                             data_test = data_test_behav,
                             alpha_vect = alpha_vect0)
toc()
write.csv(fit_best_LOO, file = "./Results/fit_best_LOO.csv", row.names = FALSE)

##############################################################
# Find the best parameter value for each subject + Evaluation
##############################################################

tic()
fit_LOOCV = Compute_fit_LOOCV(fit_best_LOO)
toc()
write.csv(fit_LOOCV, file = "./Results/fit_LOOCV.csv", row.names = FALSE)



