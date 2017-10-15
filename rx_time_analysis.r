#Reaction time publication 
#interrater reliability and Intraclass Correlation Coefficient (ICC) analysis

#load data
setwd('C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/projects/HPL Study/data')
data <- read.csv('full_rx_time_data.csv')

#load libraries
library(irr)
library(ggplot2)

##############
#descriptives#
##############
summary(data)


#################
###   ICC     ###
#################

###Comparison within Sway (test/retest reliability)
#create matrix with just sway results

sway_1 <- data$T1_reaction_time_ms
sway_2 <- data$T2_reaction_time_ms
sway_3 <- data$T3_reaction_time_ms
sway_4 <- data$T4_reaction_time_ms
icc_sway_all <- cbind(sway_1, sway_2, sway_3, sway_4) #ICC = 0.686

s1_s2 <-cbind(sway_1, sway_2) #ICC = 0.765
s2_s3 <- cbind(sway_2, sway_3) #ICC = 0.597
s3_s4 <-cbind(sway_3,sway_4) #ICC = 0.843


#scatterplots
plot(sway_1, sway_2)
plot(sway_2, sway_3)
plot(sway_3, sway_4)

#ICC(3,k), assess consistency
results_test_retest <- icc(ratings = icc_sway_all, type ="consistency", unit = "single", model = "twoway")#ICC = 0.713
icc(ratings = s1_s2, type ="agreement", unit = "single", model = "twoway")#.765
icc(ratings = s2_s3, type ="agreement", unit = "single", model = "twoway")#.597
icc(ratings = s3_s4, type ="agreement", unit = "single", model = "twoway") #.843

### Comparison between Sway and CTIP
#ICC(3,k) because raters aren't random, and k for average of values...
#irr notes: ratings = dataframe, type = "consistency", unit = "average", model = "twoway"
# input: two columns dataframe for comparison: sway ave, CTIP ave.
ctip <- data$mean_CTIP_ms
sway <- data$mean_all_trials_ms
icc_df <- cbind(ctip, sway)
results_compare <- icc(ratings = icc_df, type = "consistency", unit = "average", model = "twoway") #0.732 ICC


###############################
### Plots for Publications  ###
###############################
library(ggpubr)

#scatterplot of mean sway vs mean ctip with r value

ggscatter(data, x = "mean_all_trials_ms", y = "mean_CTIP_ms",
          color = "black",  size = 3, #points color, shape, size
          add = "reg.line",
          add.params = list(color = "black"),
          conf.int = FALSE,
          cor.coef = TRUE,
          cor.coeff.args = list(method = "pearson", label.x = 230, label.sep = "\n"),
          xlab = "Average Sway Result (milliseconds)",
          ylab = "Average CTIP Result (milliseconds)"
          )


#barplot for sway trials comparison
#create new df with long format for plotting
barplot <- read.csv('rx_barplot_sway_results.csv', stringsAsFactors = TRUE)
barplot$subject_id <- as.factor(barplot$subject_id)
barplot$sway_trial <- as.factor(barplot$sway_trial)

#barplot
ggbarplot(barplot, x = "sway_trial", y = "results_ms",
          #label = TRUE, 
          add = "mean_sd",
          xlab = "Sway Trial",
          ylab = "Average Response (milliseconds)"
)

