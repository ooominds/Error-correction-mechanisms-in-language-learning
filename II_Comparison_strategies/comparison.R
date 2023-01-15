### This script compares R-W against four rule-based strategies and produces Figure 3
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023).

####################
# Preliminary steps
####################

# Defining useful paths (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "II_Comparison_strategies")
DATA_TEST = paste0(TOP, "I_Model_fitting/Results/Data_test_withfit.csv")
DATA_EVENTS = paste0(TOP, "Data/events_abstract.csv")

### Set WD
setwd(WD)

### Load necessary libraries
library(ggplot2)
library(stringr)
library(dplyr)

options(width = 100)

#####################################
# Load the data (all participant)
#####################################

### Load the test dataset
data_test = read.csv(file = DATA_TEST)
nrow(data_test) # 1905

### Remove events composed of 3 cues
data_test = data_test[-which(data_test$Cues %in% c('FA1_FA2_FA3',
                                                   'FP1_FP2_FP3',
                                                   'MA1_MA2_MA3',
                                                   'PC_MP1_MP2')), ]
nrow(data_test) # 1641

###########################################
# Data preparation
###########################################

### Loading the data 
data_comp = read.csv(file = DATA_EVENTS)
nrow(data_comp) # 29

# Remove events composed of 3 cues
data_comp = data_comp[which(data_comp$IsPair == 'yes'), ]

### Initial information about the data_comp
dim(data_comp)
# [1] 25   15

names(data_comp)
#  [1] "Trial"                           "Cues_abstract"
#  [3] "Outcome_predicted_prescriptive"  "Outcome_predicted_normative"
#  [5] "Outcome_predicted_feminineonly"  "Outcome_predicted_masculineonly"
#  [7] "IsPair"                          "IsRecall"
#  [9] "bFA"                             "bFP"
# [11] "PC"                              "uFA"
# [13] "uFP"                             "uMA"
# [15] "uMP"

##################################################
# Add each strategy's response to the test dataset
##################################################

data_test = left_join(data_test, data_comp[, c('Cues_abstract', 
                                               'Outcome_predicted_prescriptive',
                                               'Outcome_predicted_normative',
                                               'Outcome_predicted_feminineonly',
                                               'Outcome_predicted_masculineonly')], by = 'Cues_abstract')

data_test$Outcome_predicted_RW = data_test$Outcome_predicted


#####################################################
# Proportion of participants best-fit by each strategy
#####################################################

### Add a column that encodes whether a response matches a particular strategy 
### (1 if the behavioural response matches the response predicted by the strategy)
data_test$Is_match_RW = ifelse(as.character(data_test$Outcome_predicted_RW) == 
                                            as.character(data_test$Outcome_selected), 1, 0)
data_test$Is_match_feminineonly = ifelse(as.character(data_test$Outcome_predicted_feminineonly) == 
                                         as.character(data_test$Outcome_selected), 1, 0)
data_test$Is_match_masculineonly = ifelse(as.character(data_test$Outcome_predicted_masculineonly) == 
                                          as.character(data_test$Outcome_selected), 1, 0)
data_test$Is_match_prescriptive = ifelse(as.character(data_test$Outcome_predicted_prescriptive) == 
                                          as.character(data_test$Outcome_selected), 1, 0)
data_test$Is_match_normative = ifelse(as.character(data_test$Outcome_predicted_normative) == 
                                          as.character(data_test$Outcome_selected), 1, 0)

### Proportion of model matches per participant
data_matches = aggregate(. ~ SubNo, data = data_test[, c('SubNo', 
                                                         'Is_match_RW',
                                                         'Is_match_feminineonly',
                                                         'Is_match_masculineonly',
                                                         'Is_match_prescriptive',
                                                         'Is_match_normative')], 'mean')
colnames(data_matches) = c('SubNo', "RW" , "feminine-only", "masculine-only", "prescriptive", "normative")


### Add a column that encodes whether a particular strategy has the highest match proportion 
### (1 if the match proportion is the highest among all strategies)
data_matches$Is_RW_best = 0
data_matches$Is_feminineonly_best = 0
data_matches$Is_masculineonly_best = 0
data_matches$Is_prescriptive_best = 0
data_matches$Is_normative_best = 0

for (i in 1:nrow(data_matches)){
     max_i = max(data_matches[i, c(2:6)])
     for (j in 2:6){
          if (data_matches[i, j] == max_i){
               data_matches[i, j+5] = 1
          }
     }    
}

# Save match rates data
write.csv(data_matches, file = "./Results/data_strategy_matches.csv", row.names = FALSE)

### Remove participants 10 and 14 and 34 (bias towards one of the responses)
data_matches = data_matches[-which(data_matches$SubNo %in% c(10, 14, 34)), ]

### Summary stats
summary(data_matches[, c("RW", "prescriptive", "normative", "feminine-only", "masculine-only")])
#        RW          prescriptive      normative      feminine-only    masculine-only
#  Min.   :0.2400   Min.   :0.2000   Min.   :0.2800   Min.   :0.2000   Min.   :0.2400
#  1st Qu.:0.5800   1st Qu.:0.5600   1st Qu.:0.5400   1st Qu.:0.4000   1st Qu.:0.4400  
#  Median :0.6800   Median :0.6400   Median :0.6957   Median :0.4800   Median :0.5200
#  Mean   :0.6767   Mean   :0.6231   Mean   :0.6684   Mean   :0.4886   Mean   :0.5114
#  3rd Qu.:0.8000   3rd Qu.:0.7200   3rd Qu.:0.8000   3rd Qu.:0.5600   3rd Qu.:0.6000  
#  Max.   :1.0000   Max.   :0.8800   Max.   :0.9167   Max.   :0.7600   Max.   :0.8000

### Proportion of participants best fit by each strategy
bestfit_res = data.frame(Prop_best_fit = apply(data_matches[, c(7:11)], 2, mean), Num_best_fit = apply(data_matches[, c(7:11)], 2, sum))
bestfit_res$Model = sapply(rownames(bestfit_res), function(s) unlist(strsplit(s, "_"))[2]) 
rownames(bestfit_res) = NULL
bestfit_res = bestfit_res[, c(3,1,2)]
bestfit_res = bestfit_res[order(-bestfit_res$Prop_best_fit), ]
#           Model Prop_best_fit Num_best_fit
# 1            RW     0.4920635           31
# 5     normative     0.4126984           26
# 4  prescriptive     0.1904762           12
# 3 masculineonly     0.1587302           10
# 2  feminineonly     0.0952381            6


# Represent the proportions as a barplot
p <- ggplot(bestfit_res, aes(x = reorder(Model, -Prop_best_fit), y = Prop_best_fit))
p <- p + geom_bar(position = position_dodge(0.2), stat = "identity", fill = "gray75") + 
  labs(x = "Model", y = "Proportion of participants best fit", 
       title = "") + ylim(0, 0.7) +
  scale_x_discrete(labels=c("RW" = "R-W", 
                            "normative" = "normative\n(masculine-biased)", 
                            "prescriptive" = "prescriptive\n(feminine-biased)",
                            "masculineonly" = "masculine-only",
                            "feminineeonly" = "feminine-only")) +
  theme(legend.position = "top", 
        legend.title = element_text(face="bold"), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) 
# Export the figure
png('./Results/Barplot_bestfit.png', he=6, wi=9, units='in', res=300)
plot(p)
dev.off()

###################################################
# Match rates between each strategy and R-W 
###################################################

### Remove participants 10 and 14 and 34 (bias towards one of the responses)
data_test = data_test[-which(data_test$SubNo %in% c(10, 14, 34)), ]

### Match rates between RW and other strategies:
data_test$is_match_RW_normative = ifelse(as.character(data_test$Outcome_predicted_RW) == 
                                                 as.character(data_test$Outcome_predicted_normative), 1, 0)
data_test$is_match_RW_prescriptive = ifelse(as.character(data_test$Outcome_predicted_RW) == 
                                                 as.character(data_test$Outcome_predicted_prescriptive), 1, 0)
data_test$is_match_RW_feminineonly = ifelse(as.character(data_test$Outcome_predicted_RW) == 
                                                 as.character(data_test$Outcome_predicted_feminineonly), 1, 0)
data_test$is_match_RW_masculineonly = ifelse(as.character(data_test$Outcome_predicted_RW) == 
                                                 as.character(data_test$Outcome_predicted_masculineonly), 1, 0)
matches_RW_withother = aggregate(. ~ SubNo, data = data_test[, c('SubNo', 
                                                                 'is_match_RW_normative',
                                                                 'is_match_RW_prescriptive',
                                                                 'is_match_RW_feminineonly',
                                                                 'is_match_RW_masculineonly')], 'mean')
colnames(matches_RW_withother) = c('SubNo', "normative" , "prescriptive", "feminine-only", "masculine-only")
sapply(matches_RW_withother[, -c(1)], "mean")
# normative   prescriptive  feminine-only masculine-only
# 0.9055199      0.8537670      0.5727295      0.4266356

