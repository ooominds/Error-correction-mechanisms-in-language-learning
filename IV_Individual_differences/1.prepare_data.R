### This script prepares the individual difference data necessary to run the linear regression analysis
### that is reported in the section "Relationship between model-fit quality and individual difference measures"
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023). 

####################
# Preliminary steps
####################

# Defining useful paths (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "IV_Individual_differences")
FIT_ACC_DATA = paste0(TOP, "I_Model_fitting/Results/best_fit_all.csv")
SRT_DATA = paste0(TOP, "Data/Data_SRT.csv")
WM_DATA = paste0(TOP, "Data/Data_WM.csv")
DEMOGRAPHICS_DATA = paste0(TOP, "Data/Demographics.csv")
MATCHES_DATA = paste0(TOP, "II_Comparison_strategies/Results/data_strategy_matches.csv")

### Set WD
setwd(WD)

# Load necessary libraries
library(dplyr)
library(lme4) 
library(lmerTest) 
library(car) 

### Load the fit accuracy data
fitacc_data = read.csv(FIT_ACC_DATA)
fitacc_data = fitacc_data[, c("SubNo", "alpha_best_median")]

#################################################################
# Extract the slope and intercept adjustments from the SRT task 
#################################################################

# Load the SRT data
SRT = read.csv(file = SRT_DATA)

### Extract random intercept and random slope for each participant by applying 
### a linear mixed effects model 

# Check the distribution of RT 
qqp(SRT$RT, "norm")
hist(SRT$RT)

# Log transformation for normality
hist(log(SRT$RT))
SRT$LogRT = log(SRT$RT)

# Scale the Trial variable
SRT$TrialScaled = scale(SRT$Trial)

lmer_srt = lmer(LogRT ~ TrialScaled +
                        (1|SubNo) +
                        (0+TrialScaled|SubNo),
               data=SRT)

# Build the main model
lmer_srt = lmer(LogRT ~ TrialScaled +
                        (1|SubNo) +
                        (0+TrialScaled|SubNo),
               data=SRT,
               control=lmerControl(optimizer='bobyqa'))

# Finally, we extract the coefficients and merge the data.
coefs_adjustements = coef(lmer_srt)$SubNo
colnames(coefs_adjustements) = c('Participant_intercept_lmm', 'Participant_slope_lmm')
coefs_adjustements$SubNo = as.numeric(rownames(coefs_adjustements))

###################################################
# Prepare the final set containing all ID measures
###################################################

# Load the WM data
WM_data = read.csv(file = WM_DATA)
WM_data = WM_data[, c("SubNo", "WM_score")]

# Load the demographics data
demographics_data = read.csv(file = DEMOGRAPHICS_DATA)
demographics_data = demographics_data[, c("SubNo", "Gender", "Age", "Languages", "Education")]

# Load the data of the match rates of all strategies including the RW model
matches_data = read.csv(file = MATCHES_DATA)
matches_data = matches_data[, c("SubNo", "RW", "prescriptive", "normative")]
colnames(matches_data) = c("SubNo", "match_rate_RW", "match_rate_presc", "match_rate_norm")

# Prepare the final dataset
fitacc_with_IDs = left_join(fitacc_data, coefs_adjustements, by = 'SubNo')
fitacc_with_IDs = left_join(fitacc_with_IDs, WM_data, by = 'SubNo')
fitacc_with_IDs = left_join(fitacc_with_IDs, demographics_data, by = 'SubNo')
fitacc_with_IDs = left_join(fitacc_with_IDs, matches_data, by = 'SubNo')

# Sort participants and trials in an ascending order
fitacc_with_IDs = fitacc_with_IDs[order(fitacc_with_IDs$SubNo), ] 

# Save the datasets
write.csv(fitacc_with_IDs, file = './Data/fitacc_with_IDs.csv', row.names = FALSE)



