


no_ASD_data <- read.csv("no_ASD_data.csv")
ASD_data <- read.csv("ASD_data.csv")


# Set up two plots side by side
par(mfrow = c(1, 2))

# ---- Non-ASD group ----
no_ASD_data_sd <- sd(no_ASD_data$acs_raked_propensity_score, na.rm = TRUE)
no_ASD_data_mean <- mean(no_ASD_data$acs_raked_propensity_score, na.rm = TRUE)

hist(no_ASD_data$acs_raked_propensity_score,
     col = "lightblue",
     main = "Propensity Scores in\nParticipants without ASD",
     xlab = "Propensity Score",
     ylab = "Number of Participants without ASD",
     xlim = c(min(no_ASD_data$acs_raked_propensity_score, na.rm = TRUE),
              max(no_ASD_data$acs_raked_propensity_score, na.rm = TRUE)),
     breaks = 30)

abline(v = no_ASD_data_mean, col = "blue", lwd = 2, lty = 1)  # mean
abline(v = no_ASD_data_mean - no_ASD_data_sd, col = "blue", lwd = 2, lty = 2)  # -1 SD
abline(v = no_ASD_data_mean + no_ASD_data_sd, col = "blue", lwd = 2, lty = 2)  # +1 SD


# ---- ASD group ----
ASD_data_sd <- sd(ASD_data$acs_raked_propensity_score, na.rm = TRUE)
ASD_data_mean <- mean(ASD_data$acs_raked_propensity_score, na.rm = TRUE)

hist(ASD_data$acs_raked_propensity_score,
     col = "lightpink",
     main = "Propensity Scores in\nParticipants with ASD",
     xlab = "Propensity Score",
     ylab = "Number of Participants with ASD",
     xlim = c(min(ASD_data$acs_raked_propensity_score, na.rm = TRUE),
              max(ASD_data$acs_raked_propensity_score, na.rm = TRUE)),
     breaks = 30)

abline(v = ASD_data_mean, col = "red", lwd = 2, lty = 1)   # mean
abline(v = ASD_data_mean - ASD_data_sd, col = "red", lwd = 2, lty = 2)  # -1 SD
abline(v = ASD_data_mean + ASD_data_sd, col = "red", lwd = 2, lty = 2)  # +1 SD

# Reset plotting layout
par(mfrow = c(1, 1))
