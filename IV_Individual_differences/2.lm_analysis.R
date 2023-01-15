### This script runs the linear regression analysis from the "Relationship between model-fit quality and 
### individual difference measures " section
###
### Script written by Dr Adnane Ez-zizi (last modified on 11/01/2023). 

####################
# Preliminary steps
####################

# Set the working directory (change the path "TOP" as appropriate)
TOP = "./Scripts_language_learning/"
WD = paste0(TOP, "IV_Individual_differences")
setwd(WD)

# Load necessary libraries
library(ggplot2)
library(sjPlot)
library(gridExtra)
library(xtable)

options(width = 100)

#####################
# Preparing the data
#####################

### Load the ID dataset
data = read.csv(file = './Data/fitacc_with_IDs.csv')
nrow(data) # 66

### Remove participants 10 and 14 and 34 (bias towards one of the responses)
data = data[-which(data$SubNo %in% c(10, 14, 34)), ]

# Remove Gender response of the participant who responded "prefer not to say"
data$Gender = as.factor(data$Gender)
levels(data$Gender)[levels(data$Gender)=='prefer_not'] = NA
data = droplevels(data)
data_complete = data[complete.cases(data),]
rownames(data_complete) = c(1:nrow(data_complete))
nrow(data_complete) # 62

### Check WM scores
summary(scale(data_complete$WM_score))
# Min.   :-4.344491  
# 1st Qu.:-0.440458  
# Median : 0.094612  
# Mean   : 0.000000  
# 3rd Qu.: 0.788222  
# Max.   : 1.680006

which(data_complete$WM_score==min(data_complete$WM_score))
# 2
# => Subject 2 is 4.3 stds aways from the mean - a clear outlier. 
#    The next WM zscore is -1.81 stds away from the mean. 
#    It seems that this subject did not understand the task and
#    in the first trials was retaining digits from the mathematical operations.
#    Data from this subject will be removed  

### Remove this subject (Num 2)
data_complete = data_complete[-which(data_complete$WM_score==min(data_complete$WM_score)), ]
data_complete = droplevels(data_complete)

### Create WM zscores
data_complete$WM_zscore = scale(data_complete$WM_score)

#############################
# Prepare data for modelling
#############################

# Logit transformation for the match rate between R-W and participants' responses
data_complete$match_rate_RW_logit = log((data_complete$match_rate_RW + min(data_complete$match_rate_RW)) / ((1-data_complete$match_rate_RW) + min(data_complete$match_rate_RW) )) # we add min(1-y) since the y values are high
data_complete$match_rate_presc_logit = log((data_complete$match_rate_presc + min(data_complete$match_rate_presc)) / ((1-data_complete$match_rate_presc) + min(data_complete$match_rate_presc) )) # we add min(1-y) since the y values are high
data_complete$match_rate_norm_logit = log((data_complete$match_rate_norm + min(data_complete$match_rate_norm)) / ((1-data_complete$match_rate_norm) + min(data_complete$match_rate_norm) )) # we add min(1-y) since the y values are high

# Checking the transformations
hist(data_complete$match_rate_RW_logit)
hist(data_complete$match_rate_presc_logit)
hist(data_complete$match_rate_norm_logit)
plot(data_complete$match_rate_RW, data_complete$match_rate_RW_logit)

######################################################
# Modelling the estimated model-participant match rate
######################################################

# Final model
lm_matchrate_RW = lm(match_rate_RW_logit ~ WM_zscore + Gender, data = data_complete)
summary(lm_matchrate_RW)
# Residuals:
#       Min        1Q    Median        3Q       Max 
# -1.136206 -0.233685  0.069752  0.269480  1.292498 
# 
# Coefficients:
#              Estimate Std. Error t value  Pr(>|t|)
# (Intercept)  0.594130   0.076084  7.8089 1.281e-10
# WM_zscore    0.172655   0.062537  2.7608  0.007706
# Gendermale  -0.282027   0.129162 -2.1835  0.033058
# 
# Residual standard error: 0.46983 on 58 degrees of freedom
# Multiple R-squared:  0.14769,	Adjusted R-squared:  0.1183 
# F-statistic: 5.0251 on 2 and 58 DF,  p-value: 0.0097133

### Summary table to use in the paper
tab_model(lm_matchrate_RW)

### Model diagnostics
plot(fitted(lm_matchrate_RW), resid(lm_matchrate_RW)) 
grid()

plot(data_complete$WM_zscore, resid(lm_matchrate_RW)) 
grid()

qqnorm(resid(lm_matchrate_RW))
qqline(resid(lm_matchrate_RW))

### Model without extreme residuals
summary(lm_matchrate_RW_noev <- lm(match_rate_RW_logit ~ WM_zscore + Gender,
                                  data=data_complete,
                                  subset=abs(scale(resid(lm_matchrate_RW)))<2.5))
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.1339 -0.2444  0.1019  0.3005  0.7600 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.59458    0.07131   8.338 1.89e-11 ***
#  WM_zscore    0.17514    0.05862   2.988  0.00414 ** 
#  Gendermale  -0.34484    0.12285  -2.807  0.00683 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4403 on 57 degrees of freedom
# Multiple R-squared:  0.1919,	Adjusted R-squared:  0.1636 
# F-statistic: 6.768 on 2 and 57 DF,  p-value: 0.002305


###################
# Plots (Figure 7)
###################

### Proportion of matches by WM-score
# linear trend + confidence interval
pp1 <- ggplot(data_complete, aes(x=WM_zscore, y=match_rate_RW)) +
  geom_smooth(method=lm, colour = "black") +
  labs(x = "WM z-score", y = "Proportion of matches with R-W", title = "") +
  theme(legend.position = "top", 
        legend.title = element_text(face="bold"), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) 

### Proportion of matches by gender
pp2 <- ggplot(data_complete, aes(x=Gender, y=match_rate_RW)) + 
      geom_boxplot(fill="grey") + 
      labs(x = "Gender", y = "Proportion of matches with R-W") + 
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 12, face = "bold")) 


# Export the figure
png('./Results/MarginalEffects_modelmatches.png', he=6, wi=9, units='in', res=300)
grid.arrange(pp1, pp2, nrow = 1)
dev.off()

############################
# A few stats for the paper
############################

# Mean and SD of fit accuracy for female and male participants
mean(data_complete$match_rate_RW[data_complete$Gender=="female"]) # 0.6942048
mean(data_complete$match_rate_RW[data_complete$Gender=="male"]) # 0.6273518
sd(data_complete$match_rate_RW[data_complete$Gender=="female"]) # 0.1718527
sd(data_complete$match_rate_RW[data_complete$Gender=="male"]) # 0.156276

# Mean and SD of WM zscore for female and male participants
mean(data_complete$WM_zscore[data_complete$Gender=="female"]) # -0.1813659
mean(data_complete$WM_zscore[data_complete$Gender=="male"]) # 0.3215123
sd(data_complete$WM_zscore[data_complete$Gender=="female"]) # 1.044985
sd(data_complete$WM_zscore[data_complete$Gender=="male"]) # 0.843666


##############################
# Correlations (for Table S9)
##############################

options(width = 170)

### inter-correlations between all continuous variables
cor_cont = cor(data_complete[, c("Age", "WM_zscore", "Participant_slope_lmm", 
                                  "match_rate_RW", "match_rate_presc", "match_rate_norm")])
#                               Age   WM_zscore Participant_slope_lmm match_rate_RW match_rate_presc match_rate_norm
# Age                    1.00000000 -0.03768412           -0.04959814   -0.07206904      -0.06459996      -0.1702266
# WM_zscore             -0.03768412  1.00000000           -0.15299627    0.26987004       0.26080478       0.2264673
# Participant_slope_lmm -0.04959814 -0.15299627            1.00000000   -0.19029301      -0.15601918      -0.1429167
# match_rate_RW         -0.07206904  0.26987004           -0.19029301    1.00000000       0.88572939       0.9408486
# match_rate_presc      -0.06459996  0.26080478           -0.15601918    0.88572939       1.00000000       0.7753851
# match_rate_norm       -0.17022656  0.22646734           -0.14291669    0.94084864       0.77538513       1.0000000

### Get the p-values

### Pearson correlation between WM score and SRT slope
cor.test(~ WM_zscore + Participant_slope_lmm, data = data_complete)
# data:  WM_zscore and Participant_slope_lmm
# t = -1.18919, df = 59, p-value = 0.23913
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.38979898  0.10278451
# sample estimates:
#         cor 
# -0.15299627 

### Pearson correlation between WM span and Age
cor.test(~ WM_zscore + Age, data = data_complete)
# data:  WM_zscore and Age
# t = -0.289663, df = 59, p-value = 0.77309
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.28678339  0.21618817
# sample estimates:
#          cor 
# -0.037684121

### Pearson correlation between SRT slope and Age
cor.test(~ Participant_slope_lmm + Age, data = data_complete)
# data:  Participant_slope_lmm and Age
# t = -0.38144, df = 59, p-value = 0.70425
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.29770064  0.20478027
# sample estimates:
#          cor 
# -0.049598139 

### Pearson correlation between RW match rate and WM span
cor.test(~ match_rate_RW + WM_zscore, data = data_complete)
# data:  match_rate_RW and WM_zscore
# t = 2.15279, df = 59, p-value = 0.035436
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.019365346 0.488493322
# sample estimates:
#        cor 
# 0.26987004 

### Pearson correlation between RW match rate and SRT slope
cor.test(~ match_rate_RW + Participant_slope_lmm, data = data_complete)
# data:  match_rate_RW and Participant_slope_lmm
# t = -1.48887, df = 59, p-value = 0.14185
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.421896579  0.064624519
# sample estimates:
#         cor 
# -0.19029301 

### Pearson correlation between RW match rate and Age
cor.test(~ match_rate_RW + Age, data = data_complete)
# data:  match_rate_RW and Age
# t = -0.555016, df = 59, p-value = 0.58098
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.31811645  0.18307422
# sample estimates:
#          cor 
# -0.072069036 

### Pearson correlation between prescriptive match rate and WM span
cor.test(~ match_rate_presc + WM_zscore, data = data_complete)
# data:  match_rate_presc and WM_zscore
# t = 2.0751, df = 59, p-value = 0.042347
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.0096155477 0.4810331563
# sample estimates:
#        cor 
# 0.26080478 

### Pearson correlation between prescriptive match rate and SRT slope
cor.test(~ match_rate_presc + Participant_slope_lmm, data = data_complete)
# data:  match_rate_presc and Participant_slope_lmm
# t = -1.21326, df = 59, p-value = 0.22986
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.392422100  0.099719422
# sample estimates:
#         cor 
# -0.15601918 

### Pearson correlation between prescriptive match rate and Age
cor.test(~ match_rate_presc + Age, data = data_complete)
# data:  match_rate_presc and Age
# t = -0.49724, df = 59, p-value = 0.62087
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.31135569  0.19031677
# sample estimates:
#          cor 
# -0.064599963 

### Pearson correlation between normative match rate and WM span
cor.test(~ match_rate_norm + WM_zscore, data = data_complete)
# data:  match_rate_norm and WM_zscore
# t = 1.78593, df = 59, p-value = 0.07925
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.026886731  0.452483325
# sample estimates:
#        cor 
# 0.22646734 

### Pearson correlation between normative match rate and SRT slope
cor.test(~ match_rate_norm + Participant_slope_lmm, data = data_complete)
# data:  match_rate_norm and Participant_slope_lmm
# t = -1.10915, df = 59, p-value = 0.27187
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.38102458  0.11296977
# sample estimates:
#         cor 
# -0.14291669 

### Pearson correlation between normative match rate and Age
cor.test(~ match_rate_norm + Age, data = data_complete)
# data:  match_rate_norm and Age
# t = -1.3269, df = 59, p-value = 0.18965
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.404699231  0.085248486
# sample estimates:
#         cor 
# -0.17022656 

### Pearson correlation between RW and prescriptive match rates
cor.test(~ match_rate_RW + match_rate_presc, data = data_complete)
# data:  match_rate_RW and match_rate_presc
# t = 14.6562, df = 59, p-value < 2.22e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.81588853 0.93009669
# sample estimates:
#        cor 
# 0.88572939 

### Pearson correlation between RW and normative match rates
cor.test(~ match_rate_RW + match_rate_norm, data = data_complete)
# data:  match_rate_RW and match_rate_norm
# t = 21.3289, df = 59, p-value < 2.22e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.90296246 0.96422110
# sample estimates:
#        cor 
# 0.94084864 

### Pearson correlation between prescriptive and normative match rates
cor.test(~ match_rate_presc + match_rate_norm, data = data_complete)
# data:  match_rate_presc and match_rate_norm
# t = 9.43144, df = 59, p-value = 2.1985e-13
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.65059911 0.85940092
# sample estimates:
#        cor 
# 0.77538513 

###########

### inter-correlations between gender and all continous variables
data_complete$Gender_dichot = ifelse(data_complete$Gender=="male", 0, 1) # recoding Gender into 0-1 variable

### Point-biserial correlation between WM span and Gender
test_WM_gender = cor.test(~ WM_zscore + Gender_dichot, data = data_complete)
# data:  WM_zscore and Gender_dichot
# t = -1.92825, df = 59, p-value = 0.058641
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.4666878479  0.0088839905
# sample estimates:
#         cor 
# -0.24348146 
cor_WM_gender = test_WM_gender$estimate

### Point-biserial correlation between SRT slope and Gender
test_slope_gender = cor.test(~ Participant_slope_lmm + Gender_dichot, data = data_complete)
# data:  Participant_slope_lmm and Gender_dichot
# t = -0.248508, df = 59, p-value = 0.8046
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.28186166  0.22128656
# sample estimates:
#          cor 
# -0.032336096
cor_slope_gender = test_slope_gender$estimate

### Point-biserial correlation between Age and Gender
test_age_gender = cor.test(~ Age + Gender_dichot, data = data_complete)
# data:  Age and Gender_dichot
# t = 1.32426, df = 59, p-value = 0.19053
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.085585398  0.404415390
# sample estimates:
#        cor 
# 0.16989699 
cor_age_gender = test_age_gender$estimate

### Point-biserial correlation between RW match rate and Gender
test_RW_gender = cor.test(~ match_rate_RW + Gender_dichot, data = data_complete)
# data:  match_rate_RW and Gender_dichot
# t = 1.50608, df = 59, p-value = 0.13738
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.062434392  0.423702489
# sample estimates:
#        cor 
# 0.19241149 
cor_RW_gender = test_RW_gender$estimate

### Point-biserial correlation between prescriptive match rate and Gender
test_presc_gender = cor.test(~ match_rate_presc + Gender_dichot, data = data_complete)
# data:  match_rate_presc and Gender_dichot
# t = 0.980072, df = 59, p-value = 0.33105
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.12937584  0.36670146
# sample estimates:
#        cor 
# 0.12656844 
cor_presc_gender = test_presc_gender$estimate

### Point-biserial correlation between normative match rate and Gender
test_norm_gender = cor.test(~ match_rate_norm + Gender_dichot, data = data_complete)
# data:  match_rate_norm and Gender_dichot
# t = 1.20088, df = 59, p-value = 0.2346
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.10129637  0.39107354
# sample estimates:
#        cor 
# 0.15446453 
cor_norm_gender = test_norm_gender$estimate

### Construct the final correlation matrix
# Add the column encoding correlations with Gender
cor_gender_col = c(cor_age_gender, cor_WM_gender, cor_slope_gender, cor_RW_gender, cor_presc_gender, cor_norm_gender)
names(cor_gender_col) = rownames(cor_cont)
cor_all = cbind(cor_gender_col, cor_cont)
colnames(cor_all)[1] = 'Gender'

# Add the row encoding correlations with Gender
cor_gender_row = c(1, cor_age_gender, cor_WM_gender, cor_slope_gender, cor_RW_gender, cor_presc_gender, cor_norm_gender)
names(cor_gender_row) = colnames(cor_all)
cor_all = rbind(t(cor_gender_row), cor_all)
rownames(cor_all)[1] = 'Gender'

# Hide upper triangle
cor_upper = cor_all
cor_upper[upper.tri(cor_all)] = ""
cor_upper = as.data.frame(cor_upper)
write.csv(cor_upper, file = "./Results/Correlation_matrix.csv")

######################################################
# Extra checks and analyses (reported in Appendix S9)
######################################################

### Check WM effect separately for males and females 
lm_matchrate_RW_f = lm(match_rate_RW_logit ~ WM_zscore, data = data_complete[data_complete$Gender=="female", ])
summary(lm_matchrate_RW_f)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -1.1286 -0.2588  0.1050  0.3668  0.7496 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.59562    0.07806   7.630 4.17e-09 ***
# WM_zscore    0.18086    0.07453   2.427   0.0202 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.4801 on 37 degrees of freedom
# Multiple R-squared:  0.1373,    Adjusted R-squared:  0.114
# F-statistic: 5.888 on 1 and 37 DF,  p-value: 0.02023

lm_matchrate_RW_m = lm(match_rate_RW_logit ~ WM_zscore, data = data_complete[data_complete$Gender=="male", ])
summary(lm_matchrate_RW_m)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.83429 -0.20512  0.03247  0.22999  1.29014

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.3194     0.1057   3.023  0.00672 **
# WM_zscore     0.1499     0.1194   1.255  0.22392
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.4617 on 20 degrees of freedom
# Multiple R-squared:  0.07301,   Adjusted R-squared:  0.02666 
# F-statistic: 1.575 on 1 and 20 DF,  p-value: 0.2239

# Summaries for the paper
tab_model(lm_matchrate_RW_f)
tab_model(lm_matchrate_RW_m)

