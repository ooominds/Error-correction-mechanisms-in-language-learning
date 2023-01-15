### This script analyses the relationship between the model activation-based measures and 
### participants' choices and response times (Table 3 and Figure 4). It also compares the observed 
### and predicted proportions of form choices (Figure 5)
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023). 

####################
# Preliminary steps
####################

# Defining useful paths (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "III_Matches_with_RW")
DATA_TEST = paste0(TOP, "I_Model_fitting/Results/Data_test_withfit.csv")

### Set WD
setwd(WD)

### Load necessary libraries
library(lmerTest)
library(ggplot2) 
library(sjPlot) 
library(popbio) 
library(car) 
library(gridExtra)
library(lattice)
library(reshape2)
library(stringr)

options(width = 100)
options(show.signif.stars=FALSE)
options(digits=8)

### Set the theme for the sjPlots
set_theme(
  legend.pos = "top",
  axis.textsize.x = 1.1,
  axis.textsize.y = 1.1
)

###########################################
# Data preparation
###########################################

### Loading the data 
data = read.csv(file = DATA_TEST)
nrow(data) # 1905

# Remove participants 10 and 14 and 34 (bias towards one of the responses)
data = data[-which(data$SubNo %in% c(10, 14, 34)), ]
nrow(data) # 1818

# Remove events composed of 3 cues
data = data[-which(data$Cues %in% c('FA1_FA2_FA3',
                                    'FP1_FP2_FP3',
                                    'MA1_MA2_MA3',
                                    'PC_MP1_MP2')), ]
data = droplevels(data)

# Change the names of uMP variable for the plot legends
data$uMP = as.character(data$uMP)
data$uMP[data$uMP=='0'] = 'absent'  
data$uMP[data$uMP=='1'] = 'present' 

# Add a variable that encodes the difference between the two activations
data$activ_np_minus_mp = data$np_activation - data$mp_activation
data$activ_abs = abs(data$np_activation - data$mp_activation)

# Recoding variables
data$SubNo = as.factor(as.character(data$SubNo))
data$Trial = as.factor(as.character(data$Trial))
data$uMP = as.factor(as.character(data$uMP))
data$np = as.factor(as.character(data$np))


##################################################################
# Model fit: Mixed-effects logistic regression for the np choice
##################################################################

glmm_activ_np = glmer(np ~ activ_np_minus_mp
                          + (activ_np_minus_mp | SubNo)
                          + (1 | Cues), 
                      data = data, 
                      family="binomial")
summary(glmm_activ_np, corr = FALSE)
# Formula: np ~ activ_np_minus_mp + (activ_np_minus_mp | SubNo) + (1 | Cues)
#    Data: data

#      AIC      BIC   logLik deviance df.resid
#   1831.0   1863.1   -909.5   1819.0     1560

# Scaled residuals:
#      Min       1Q   Median       3Q      Max
# -3.05067 -0.69720 -0.17622  0.68077  4.07066 
# 
# Random effects:
#   Groups Name              Variance Std.Dev. Corr  
#   Cues   (Intercept)       0.32308  0.56840        
#   SubNo  (Intercept)       0.25739  0.50733        
#          activ_np_minus_mp 3.08326  1.75592  -0.128
# Number of obs: 1566, groups:  Cues, 66; SubNo, 63
# 
# Fixed effects:
#                   Estimate Std. Error z value  Pr(>|z|)
# (Intercept)       -0.28210    0.11744 -2.4021    0.0163
# activ_np_minus_mp  1.91419    0.29259  6.5423 6.058e-11

#######################################################
# Model fit: Mixed-effects logistic regression for RT
#######################################################

######## Data preparation #########

### Data transformation

# Check the distribution of RT
summary(data$RT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00  392.00  715.50  934.26 1273.75 4640.00 

hist(data$RT)
# => Skewed to the right: try log and cox-box transformation

# log transformation of RT
hist(log(data$RT))
# => Not bad but let's see if we can do better

# Cox-box transformation of RT
lambda = powerTransform(data$RT/1000)$lambda
# Estimated transformation parameter
# data$RT 
# 0.23807307 
hist((data$RT/1000)^lambda)
# => This looks good

data$transformedRT = scale((data$RT/1000)^lambda)
summary(data$transformedRT)
# Min.   :-3.819832  
# 1st Qu.:-0.705008  
# Median :-0.034377  
# Mean   : 0.000000  
# 3rd Qu.: 0.705162  
# Max.   : 2.782535


######## Modelling #########

### Final model based on activ_np_minus_mp (activation support)
lmm_activ_RT = lmer(transformedRT ~ activ_np_minus_mp
                                    + I(activ_np_minus_mp^2)
                                    + (activ_np_minus_mp + I(activ_np_minus_mp^2) | SubNo)
                                    + (1 | Cues),
                    data = data)
summary(lmm_activ_RT)
# REML criterion at convergence: 4172.4
#
# Scaled residuals:
#       Min        1Q    Median        3Q       Max
# -3.098829 -0.630029 -0.015868  0.650289  2.766030

# Random effects:
#  Groups   Name                   Variance Std.Dev. Corr
#  Cues     (Intercept)            0.022409 0.14970
#  SubNo    (Intercept)            0.198804 0.44587
#           activ_np_minus_mp      0.054049 0.23248  -0.341
#           I(activ_np_minus_mp^2) 0.074914 0.27370   0.112  0.353
#  Residual                        0.727966 0.85321
# Number of obs: 1566, groups:  Cues, 66; SubNo, 63
# 
# Fixed effects:
#                         Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)             0.079260   0.067949 72.680984  1.1665  0.24724
# activ_np_minus_mp      -0.023278   0.057379 56.498607 -0.4057  0.68651
# I(activ_np_minus_mp^2) -0.195914   0.077803 53.106434 -2.5181  0.01485
#
# Correlation of Fixed Effects:
#             (Intr) act___
# actv_np_mn_ -0.192
# I(ctv___^2) -0.275  0.127

### Diagnostics for the residuals and random effects
plot(lmm_activ_RT, resid(.) ~ activ_np_minus_mp^2, abline = 0)
qqnorm(resid(lmm_activ_RT))
qqnorm(ranef(lmm_activ_RT)$Cues[,1])
qqnorm(ranef(lmm_activ_RT)$SubNo[,1])

##########################################################
# Final model summary table and figure to use in the paper
##########################################################

# Summary as a nice html table (Tables 3 and F in the paper)
tab_model(glmm_activ_np, lmm_activ_RT)

# Plot marginal effect of activ_np_minus_mp on np
pp1 = plot_model(glmm_activ_np, type = "pred", terms = "activ_np_minus_mp", 
                title = "",
                axis.lim = list(c(-2, 1.5), c(0, 1)),
                legend.title = "masculine personal cue",
                axis.title = c('Model activation support for the non-masculine form', 'Proportion of non-masculine plural choices'))
# Plot marginal effect of activ_np_minus_mp on RT
pp2 = plot_model(lmm_activ_RT, type = "pred", terms = "activ_np_minus_mp", 
                title = "",
                #axis.lim = list(c(-2, 1.5), c(0, 1)),
                legend.title = "masculine personal cue",
                axis.title = c('Model activation support for the non-masculine form', 'Response time (transformed)'))

# Export the figure
png('./Results/MarginalEffects_npAndRT.png', he=6, wi=9, units='in', res=300)
grid.arrange(pp1, pp2, nrow = 1)
dev.off()

###############################################################
# Predicting the probabilities of np for each event (Figure 5)
###############################################################

### Model based on R-W activation support measure
glmm_activ_np = glmer(np ~ activ_np_minus_mp
                          + (activ_np_minus_mp|SubNo)
                          + (1|Cues), 
                      data = data, 
                      family="binomial")

### Add predicted np probabilities to the data
# predicted probability of np
data$np_pred_prob_glmm = predict(glmm_activ_np, newdata = data, type = "response")	

# Prepare data for ploting 
data_forplot = data[, c("SubNo", "Trial", "Cues_abstract", "np", "np_pred_prob_glmm")]
data_forplot$np = as.numeric(as.character(data_forplot$np))

# Rename columns
colnames(data_forplot)[colnames(data_forplot) == "np"] = "Participants"
colnames(data_forplot)[colnames(data_forplot) == "np_pred_prob_glmm"] = "Model"

### Convert to long format based on "Participants" and "Model" columns
# Conversion
data_forplot_long = melt(data_forplot, id.vars=c("SubNo", "Trial", "Cues_abstract"))
# Rename columns
colnames(data_forplot_long)[colnames(data_forplot_long)=="variable"] = "Data"
colnames(data_forplot_long)[colnames(data_forplot_long)=="value"] = "np_choice"
data_forplot_long = droplevels(data_forplot_long)

# replace "_" with "+"
data_forplot_long$Cues_abstract = str_replace(data_forplot_long$Cues_abstract, "PC", "ibFA")
data_forplot_long$Cues_abstract = str_replace(data_forplot_long$Cues_abstract, "_", " + ")
# reorder based on participants' np proportions
data_forplot_long$Cues_abstract2 = reorder(data_forplot_long$Cues_abstract[data_forplot_long$Data == "Participants"], 
                                           data_forplot_long$np_choice[data_forplot_long$Data == "Participants"])

### Point plot
pp = ggplot(data_forplot_long, aes(x = Cues_abstract2, y = np_choice, color=Data, shape=Data))
pp = pp + 
  geom_point(stat = "summary", fun = "mean", size=3,
             position = position_dodge(width=0.25)) +
  labs(x = "Event", y = "Proportion of participants choosing non-masculine plural",
       title = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01)) +
  scale_color_manual(values=c("red3", "blue3"))+
  scale_shape_manual(values=c(16, 15))+
  theme(legend.position = "top",
        legend.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11.5, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"))
# Export the figure
png('./Results/modelfit_by_event.png', he=7, wi=9, units='in', res=300)
plot(pp)
dev.off()
