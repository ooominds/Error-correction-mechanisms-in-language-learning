### this script simulates R-W models using the best fit learning rates found for each participant
### then produces weights matrices, activations and outcome predictions for each participant. 
### The last two are added to the dataset containing the test results from the experiment
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

### Function that simulates one model (given alpha) using the data from one participant.
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

### Function that simulates R-W using the best model parameters for each participant 
Simulate_fit <- function(data_train, data_test, best_fit_df){

	# Extract number of participants
  N_subj = length(unique(data_train$SubNo))

  # Dataframe that will contain all fit results (predicted outcomes, activations, etc)
  fit_results_all = data.frame(matrix(nrow=0, ncol = 8))
  colnames(fit_results_all) = c('SubNo', "Cues", 'Outcome_predicted', "Is_match_with_model", 
                                "mp_activation", "np_activation", "mp_pred_prob", "np_pred_prob")
  
  # Dataframe that will contain all the weights
  weights_all = data.frame(matrix(nrow=0, ncol = 4))
  colnames(weights_all) = c('SubNo', 'Cue', 'mp', 'np')

  ### Compare each participant with all models
  for (j in 1:N_subj){

      # ith param set
      alpha_j = best_fit_df$alpha_best_median[j]

      # participant's results
      sim_results = simulate_1RW_1subj(data_train = data_train, 
                                        data_test = data_test, 
                                        subj_num = j, 
                                        alpha = alpha_j)     
      
      # fit results dataframe
      fit_results_j = sim_results$RW_predictions
      fit_results_j = fit_results_j[, -which(colnames(fit_results_j)=='Outcome_selected')] # remove outcome_selected columns
      fit_results_j$SubNo = j
      fit_results_j = fit_results_j[, c(8,1:7)]
      fit_results_all = rbind(fit_results_all, fit_results_j)
      
      # Prepare the weights dataframe
      weights_j = data.frame(sim_results$weights)
      weights_j$SubNo = j
      weights_j$Cue = row.names(weights_j)
      weights_j = weights_j[, c(3,4,1,2)]
      weights_all = rbind(weights_all, weights_j)
      
  }

return(list(fit_results_all = fit_results_all, weights_all = weights_all))

}

###################################
# Data preparation
###################################

### Load the behavioural data
data_train_behav = read.csv(file = DATA_TRAIN)  
data_test_behav = read.csv(file = DATA_TEST) 
data_bestfit = read.csv(file = "./Results/best_fit_all.csv")

##########################################
# Simulate all models for all participants
##########################################

tic()
Sim_results = Simulate_fit(data_train = data_train_behav, 
                           data_test = data_test_behav,
                           best_fit_df = data_bestfit)
toc()
sim_results_all = Sim_results$fit_results_all
weights_all = Sim_results$weights_all

### Add the columns from the fit dataset to data_test
data_test_ext = merge(x = data_test_behav, y = sim_results_all, by = c("SubNo", "Cues"), all.x = TRUE)
# Sort participants and trials in an ascending order
data_test_ext = data_test_ext[order(data_test_ext$SubNo, data_test_ext$Trial), ] 

write.csv(data_test_ext, file = "./Results/Data_test_withfit.csv", row.names = FALSE)
write.csv(weights_all, file = "./Results/fit_weights_all.csv", row.names = FALSE)



