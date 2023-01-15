### This script produces the figure depicting the relationship between learning rates and RW's fit 
### accuracy (Figure S6)
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023). 

####################
# Preliminary steps
####################

# Set the working directory (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "I_Model_fitting")
setwd(WD)

### Load necessary libraries
library(ggplot2)

###########################################
# Data preparation
###########################################

### Loading the data 
fit_all = read.csv(file = "./Results/fit_all.csv")

# Remove participants 10 and 14 and 34 (bias towards one of the responses)
fit_all = fit_all[-which(fit_all$SubNo %in% c(10, 14, 34)), ]
fit_all = droplevels(fit_all)

# Recoding the subject column
fit_all$SubNo = rep(1:63, each = 50)

#####################################################################
# Plot relationship between learning rates and model fit accuracies
#####################################################################

# For facet label names 
fit_all$SubNo_fact = paste0("Subj. ", fit_all$SubNo)
fit_all$SubNo_fact = factor(fit_all$SubNo_fact, levels = unique(fit_all$SubNo_fact))

png('./Results/LearningRates_plot.png', he=16, wi=12.5, units='in', res=300)
p = ggplot(fit_all, aes(x = alpha, y = match_rate)) + 
      geom_point() + #geom_line() + 
      facet_wrap(~ SubNo_fact, nrow = 9, ncol = 7) + 
      labs(x="Learning rate", y = "Model fit accuracy") + 
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 14, face = "bold", hjust=0.5),
            strip.text = element_text(size = 12, face = "bold"))
plot(p)
dev.off()      

  





