### This script hosts the TestResults() function, which produces test accuracy, activation matrix, 
### probability matrix, contingency table, and a detailed result data frame based on 
### a list of events, outcomes and a weight matrix

### Script written by Dr Adnane Ez-zizi (last modified on 21/04/2022)
### We use and adapt the act2probs() function from the ndl package by Arppe, Hendrix, Milin, Baayen, Sering and Shaoul
### under GPL-3 licence (https://CRAN.R-project.org/package=ndl)

### Modified version of the act2probs function of ndl, which uses softmax transformation
acts2probs_softmax = function(acts){
  acts_exp = exp(acts)
  rowsums = apply(acts_exp, 1, sum)
  m = matrix(rep(rowsums, rep(ncol(acts_exp), nrow(acts_exp))), nrow(acts_exp), 
             ncol(acts_exp), byrow = TRUE)
  p = acts_exp/m
  colnames(p) = colnames(acts_exp)
  return(p)
}

find_max_if_possible = function(v){
  if (abs(max(v) - min(v)) < 10^(-6)){
    res = 'DRAW'
  } else{
    res = names(which.max(v))
  }
  return(res)
}

acts2preds = function(acts){
  predictions = apply(acts, 1, find_max_if_possible)
  return(predictions)
}

### Function that produces test accuracy, activation matrix, 
### probability matrix, contingency table, and a detailed 
### result data frame 
TestResults = function(data, outcomes, weights) {
  
  # Activations
  n_context.test = nrow(data)
  n_out = length(outcomes)
  activations.test = matrix(NA, nrow = n_context.test, ncol = n_out)
  rownames(activations.test) = data$Cues
  colnames(activations.test) = colnames(weights)
  
  # Add null entries in the weight matrix for the cues that appear in the test set
  # but not in the training set (copied from estimateActivations function)
  cues = rownames(weights)
  obsCues = strsplit(as.character(data$Cues), "_")
  uniqueObsCues = unique(unlist(obsCues))
  newCues = uniqueObsCues[!is.element(uniqueObsCues, cues)]
  if (length(newCues) > 0) {
    wnew = matrix(0, length(newCues), ncol(weights))
    rownames(wnew) = newCues
    colnames(wnew) = colnames(weights)
    weights.test = rbind(weights, wnew)
    cues = c(cues, newCues)
  } else {
    weights.test = weights
  }
  
  for (n in 1:n_context.test){
    # Create the vector of all available cues in the nth context
    cues.n = unique(unlist(strsplit(data$Cues[n], "_")))
    for (k in 1:n_out){
      # Activation of the n-th context for each outcome
      activations.test[n, outcomes[k]] = sum(weights.test[cues.n, outcomes[k]])
    }
  }
  
  ## Predictions from activations
  Outcome.test = as.factor(data$Outcome) # Test resp variable
  
  # Probability matrix from activation
  p.test = acts2probs(activations.test)$p 
  # Probability matrix from activation based on softmax
  p_softmax.test = acts2probs_softmax(activations.test)
  # Predicted Tense for each context
  pred.test = acts2preds(activations.test)
  
  # Statistics for the resulting contingency table
  ctable.test0 = table(Outcome.test, as.factor(pred.test)) 
  ctable.test0
  # The predicted outcomes might not include all outcomes in the test data, 
  #so we add the missing outcomes manually by adding the corresponding columns to 
  #the contingency table
  ctable.test = matrix(0, length(levels(Outcome.test)), length(levels(Outcome.test)))
  rownames(ctable.test) = levels(Outcome.test)
  colnames(ctable.test) = levels(Outcome.test)
  # Adding the missing columns
  for (j in 1:length(levels(Outcome.test))) {
    if (colnames(ctable.test)[j] %in% colnames(ctable.test0)){
      ctable.test[, colnames(ctable.test)[j]] = ctable.test0[, colnames(ctable.test)[j]]
    }
  }
  ptable.test= round(ctable.test/rowSums(ctable.test), 2) # Extracting the proportion table
  accuracy.test = crosstableStatistics(ctable.test)$accuracy # prediction accuracy
  
  # Summary of the results
  Results.test = cbind.data.frame(Cues = data$Cues,
                                  Outcome_selected = data$Outcome,
                                  Outcome_predicted = pred.test,
                                  Is_match_with_model = ifelse(data$Outcome==pred.test, 1, 0))
  rownames(Results.test) <- NULL # Remove row names
  
  # Return the activation matrix, the probability matrix, 
  # the contingency table, and the detailed accuracy data frame 
  result = (list(weights = weights.test, 
                 activation = activations.test,
                 p = p.test,
                 p_softmax = p_softmax.test,
                 ctable = ctable.test,
                 ptable = ptable.test,
                 match_rate = accuracy.test, 
                 DetailResult = Results.test))
  class(result) = "Result.test"
  return(result)
  
}