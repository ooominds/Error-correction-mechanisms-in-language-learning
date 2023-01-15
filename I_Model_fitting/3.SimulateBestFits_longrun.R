### As with SimulateBestFits.r, this script simulates R-W models using the best fit learning 
### rates found for each participant but now assumes 1000 repetitions per events instead of 15 
### (used to make Figure S7 in Appendix S7). The script then produces weights matrices, activations and 
### outcome predictions for each participant. The last two are added to the dataset containing 
### the test results from the experiment.
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
# Fixed parameters
###################

N0 = 1000 # num of repetitions of each event

###################
# Useful functions
###################

### Function that simulates one model (given alpha and Is_background_cue) one the data from one participant.
### The function returns the weight matrix, along with the activations, predicted softmax probabilities and 
### responses for the test events. It also outputs for each test event whether the model matches the subject's 
### response and the overall match rate
simulate_1RW_1subj = function(train_events, data_test, subj_num, alpha = 0.01){

  ### Construct expanded training set for the whole experiment
  ### Expand learning events of block 1
  Train1 = train_events[c(1:4),]
  Train1.exp = Train1[rep(row.names(Train1), Train1$Frequency),]
  # shuffle the vector to make it random
  Train1.exp = Train1.exp[sample(nrow(Train1.exp)),]

  ### Expand learning events of block 2
  Train2 = train_events[c(5:8),]
  Train2.exp = Train2[rep(row.names(Train2), Train2$Frequency),]
  # shuffle the vector to make it random
  Train2.exp = Train2.exp[sample(nrow(Train2.exp)),]

  ### Construct expanded training set for the whole experiment
  Train.exp = rbind(Train1.exp, Train2.exp)
  # Remove the frequency column
  Train.exp = subset(Train.exp, select=-c(Frequency))
  
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
Simulate_fit <- function(data_train, train_events, data_test, best_fit_df){

	# Extract number of participants
  N_subj = length(unique(data_train$SubNo))

  # Dataframe that will contain all fit results (predicted outcomes, activations, etc)
  fit_results_all = data.frame(matrix(nrow=0, ncol = 8))
  colnames(fit_results_all) = c('SubNo', "Cues", 'Outcome_predicted', "Is_match_with_model", 
                                "mp_activation", "np_activation", "mp_pred_prob",        
                                "np_pred_prob")
  
  # Dataframe that will contain all the weights
  weights_all = data.frame(matrix(nrow=0, ncol = 4))
  colnames(weights_all) = c('SubNo', 'Cue', 'mp', 'np')

  ### Compare each participant with all models
  for (j in 1:N_subj){

      # ith param set
      alpha_j = best_fit_df$alpha_best_median[j]

      # participant's results
      sim_results = simulate_1RW_1subj(train_events = train_events, 
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

### Dataset containing training events with frequencies
# Training data
Train_events = data.frame(Cues = c(
                                  # Block 1
                                  "MP1_FA1",
                                  "MP2_FA2",
                                  "FA1_FA2",
                                  "FP1_FP2",
                                  # Block 2
                                  "PC_MP1_MP2",
                                  "MA1_MA2_MA3",
                                  "FA1_FA2_FA3",
                                  "FP1_FP2_FP3"),
                          Outcomes = c(
                                  # Block 1
                                  "mp",
                                  "mp",
                                  "np",
                                  "np",
                                  # Block 2
                                  "mp",
                                  "mp",
                                  "np",
                                  "np"),
                          Frequency = N0 * c(
                                  # Block 1
                                  1,
                                  1,
                                  1,
                                  1,
                                  # Block 2
                                  1,
                                  1,
                                  1,
                                  1),
                          row.names = c(1:8)
                          )

print(Train_events)
#          Cues Outcomes Frequency
# 1     MP1_FA1       mp     10000
# 2     MP2_FA2       mp     10000
# 3     FA1_FA2       np     10000
# 4     FP1_FP2       np     10000
# 5  PC_MP1_MP2       mp     10000
# 6 MA1_MA2_MA3       mp     10000
# 7 FA1_FA2_FA3       np     10000
# 8 FP1_FP2_FP3       np     10000

##########################################
# Simulate all models for all participants
##########################################

tic()
Sim_results = Simulate_fit(data_train = data_train_behav, 
                           train_events = Train_events,
                           data_test = data_test_behav,
                           best_fit_df = data_bestfit)
toc()
sim_results_all = Sim_results$fit_results_all
weights_all = Sim_results$weights_all
write.csv(weights_all, file = "./Results/fit_weights_all_longrun.csv", row.names = FALSE)



