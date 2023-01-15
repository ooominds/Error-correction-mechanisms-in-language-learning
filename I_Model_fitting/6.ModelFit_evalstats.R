### this script calculates the evaluation statistics related to the model fitting results, which are presented in 
### the section "Participant-model match rates"
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023).


# Set the working directory (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "I_Model_fitting")
setwd(WD)

#####################e
# Preparing the data
#####################

### Load the dataset 
best_fit = read.csv(file = './Results/best_fit_all.csv')
best_fit_loocv = read.csv(file = './Results/fit_LOOCV.csv')

### Remove participants 10, 14 and 34 (bias towards one of the responses)
best_fit = best_fit[-which(best_fit$SubNo %in% c(10, 14, 34)), ]
best_fit_loocv = best_fit_loocv[-which(best_fit_loocv$SubNo %in% c(10, 14, 34)), ]

###############
# Statistics
###############

### model fit accuracy
range(best_fit$match_rate_best) # 0.24 1.00
mean(best_fit$match_rate_best) # 0.6771
sd(best_fit$match_rate_best) # 0.1699
length(which(best_fit$match_rate_best>=0.8)) # 17 out of 63

### model fit LOOCV accuracy
range(best_fit_loocv$fit_acc_LOOCV) # 0.24 1.00
mean(best_fit_loocv$fit_acc_LOOCV) # 0.6771
sd(best_fit_loocv$fit_acc_LOOCV) # 0.1698
length(which(best_fit_loocv$fit_acc_LOOCV>=0.8)) # 17 out of 63





