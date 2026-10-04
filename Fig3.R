#Figures
library(dplyr)



### FIGURES ####

### FIGURE 1: Bx Scores Across Groups ####

no_ASD_data <- read.csv("no_ASD_data.csv")
ASD_data <- read.csv("ASD_data.csv")

par(mfrow = c(1, 2))

# Plot histogram for NON-ASD SCI
no_ASD_data_sd <- sd(no_ASD_data$sci_score)
no_ASD_data_mean <- mean(no_ASD_data$sci_score)

hist(no_ASD_data$sci_score, 
     col = "lightblue",
     main = "SCI Scores in \n Participants without ASD",
     xlab = "SCI Score",
     ylab = "Number of Participants without ASD",
     xlim = c(4,25))
abline(v = no_ASD_data_mean, col = "blue", lwd = 2, lty = 1) 
abline(v = no_ASD_data_mean - no_ASD_data_sd, col = "blue", lwd = 2, lty = 2) # Blue, dashed line
abline(v = no_ASD_data_mean + no_ASD_data_sd, col = "blue", lwd = 2, lty = 2) # Blue, dashed line

# Plot histogram for ASD SCI
ASD_data_sd <- sd(ASD_data$sci_score)
ASD_data_mean <- mean(ASD_data$sci_score)

hist(ASD_data$sci_score,
     col = "lightpink",
     main = "SCI Scores in \nParticipants with ASD",
     xlab = "SCI Score",
     ylab = "Number of Participants with ASD",
     xlim = c(4,25))
abline(v = ASD_data_mean, col = "red", lwd = 2, lty = 1) 
abline(v = ASD_data_mean - ASD_data_sd, col = "red", lwd = 2, lty = 2) # Blue, dashed line
abline(v = ASD_data_mean + ASD_data_sd, col = "red", lwd = 2, lty = 2) # Blue, dashed line

# Plot histogram for NON-ASD RRB
no_ASD_data_sd <- sd(no_ASD_data$rrb_score)
no_ASD_data_mean <- mean(no_ASD_data$rrb_score)

hist(no_ASD_data$rrb_score,
     col = "lightblue",
     main = "RRB Scores in \n Participants without ASD",
     xlab = "RRB Score",
     ylab = "Number of Participants without ASD",
     xlim = c(4,25))
abline(v = no_ASD_data_mean, col = "blue", lwd = 2, lty = 1) 
abline(v = no_ASD_data_mean - no_ASD_data_sd, col = "blue", lwd = 2, lty = 2) # Blue, dashed line
abline(v = no_ASD_data_mean + no_ASD_data_sd, col = "blue", lwd = 2, lty = 2) # Blue, dashed line

# Plot histogram for ASD RRB
ASD_data_sd <- sd(ASD_data$rrb_score)
ASD_data_mean <- mean(ASD_data$rrb_score)

hist(ASD_data$rrb_score,
     col = "lightpink",
     main = "RRB Scores in \nParticipants with ASD",
     xlab = "RRB Score",
     ylab = "Number of Participants with ASD",
     xlim = c(4,25))
abline(v = ASD_data_mean, col = "red", lwd = 2, lty = 1) 
abline(v = ASD_data_mean - ASD_data_sd, col = "red", lwd = 2, lty = 2) # Blue, dashed line
abline(v = ASD_data_mean + ASD_data_sd, col = "red", lwd = 2, lty = 2) # Blue, dashed line

par(mfrow = c(1, 1))

# Means and SDs
mean(no_ASD_data$sci_score)
sd(no_ASD_data$sci_score)

mean(ASD_data$sci_score)
sd(ASD_data$sci_score)

mean(no_ASD_data$rrb_score)
sd(no_ASD_data$rrb_score)

mean(ASD_data$rrb_score)
sd(ASD_data$rrb_score)

# T tests
t.test(no_ASD_data$sci_score, ASD_data$sci_score)
t.test(no_ASD_data$rrb_score, ASD_data$rrb_score)









