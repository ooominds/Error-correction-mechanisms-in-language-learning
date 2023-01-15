### This script produces the figures depicting the distribution of weights from the fitted models (Figures 2 and S7)
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
library(reshape2)

###########################################
# Data preparation
###########################################

### Loading the data 
fit_weights = read.csv(file = "./Results/fit_weights_all.csv")
fit_weights_longrun = read.csv(file = "./Results/fit_weights_all_longrun.csv")

# Rename PC as IB
fit_weights$Cue = as.factor(fit_weights$Cue)
fit_weights_longrun$Cue = as.factor(fit_weights_longrun$Cue)
levels(fit_weights$Cue)[levels(fit_weights$Cue) == "PC"] = "FA4"
levels(fit_weights_longrun$Cue)[levels(fit_weights_longrun$Cue) == "PC"] = "FA4"

# Remove participants 10 and 14 and 34 (bias towards one of the responses)
fit_weights = fit_weights[-which(fit_weights$SubNo %in% c(10, 14, 34)), ]
fit_weights_longrun = fit_weights_longrun[-which(fit_weights_longrun$SubNo %in% c(10, 14, 34)), ]
fit_weights = droplevels(fit_weights)
fit_weights_longrun = droplevels(fit_weights_longrun)

names(fit_weights)
# "SubNo" "Cue"   "mp"    "np" 

# Recoding the subject column
fit_weights$SubNo = rep(1:63, each = 12)

### Convert to long format based on the outcome columns
convert_tolong <- function(data_weights) {

      # Conversion
      data_weights_long = melt(data_weights, id.vars=c("SubNo", "Cue"))
      # Rename columns
      colnames(data_weights_long)[colnames(data_weights_long)=="variable"] = "Outcome"
      colnames(data_weights_long)[colnames(data_weights_long)=="value"] = "Weight"
      # reorder by SubNo
      data_weights_long = data_weights_long[order(data_weights_long$SubNo, data_weights_long$Cue),]
      # Rename rows
      rownames(data_weights_long) = 1:nrow(data_weights_long)
      # Reorder levels of Cue for the histogram
      data_weights_long$Cue = factor(data_weights_long$Cue, levels=c("MA1", 
                                                                    "MA2", 
                                                                    "MA3",
                                                                    "FA1", 
                                                                    "FA2", 
                                                                    "FA3", 
                                                                    "FA4",
                                                                    "MP1", 
                                                                    "MP2",
                                                                    "FP1", 
                                                                    "FP2", 
                                                                    "FP3"))
      return(data_weights_long)
} 
fit_weights_long = convert_tolong(fit_weights)
fit_weights_longrun_long = convert_tolong(fit_weights_longrun)


#########################################
# Plot historgrams of weight associations
#########################################

display_weights <- function(data_weights_long, image_path) {
      png(image_path, he=8, wi=9, units='in', res=300)
      p1 <- ggplot(data_weights_long, aes(Weight, color=Outcome, fill=Outcome)) + 
            geom_histogram(position="identity", alpha=0.5)+ 
            facet_wrap(~ Cue, nrow = 4, ncol = 3) + 
            scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
            scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
            labs(x="Association weight", y = "Count") + 
            theme(legend.position = "top", 
                  legend.title = element_text(face="bold"), 
                  legend.text=element_text(size=10), 
                  axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12, face = "bold"),
                  plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                  strip.text = element_text(size = 12, face = "bold"))
      plot(p1)
      dev.off()      
}

display_weights(fit_weights_long, './Results/Weight_histograms.png')
display_weights(fit_weights_longrun_long, './Results/Weight_histograms_longrun.png')



