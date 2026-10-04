
# remember to install library... 
library(dplyr)

#Change Directory to location of datasets

##### Load data from ABCD dataset #######
setwd("/ABCDStudyNDA")
#  read in screener form with information about asd
abcd_screen01 <- read.delim("abcd_screen01.txt") 

# read in stratification weights with propensity scores
acspsw03 <- read.delim("acspsw03.txt")
#filter data so only baseline
acspsw03_BL <- acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",] 

# read in motion variable
abcd_dmdtifp202 <- read.delim("abcd_dmdtifp202.txt")
abcd_dmdtifp202_BL <- abcd_dmdtifp202[abcd_dmdtifp202$eventname == "baseline_year_1_arm_1",] 

# read in nih toolbox data
abcd_tbss01 <- read.delim("abcd_tbss01.txt") 
# filter nih toolbox data so only baseline
abcd_tbss01_BL <- abcd_tbss01[abcd_tbss01$eventname == "baseline_year_1_arm_1",]

# read in behavioral measures related to asd
abcd_pssrs01 <- read.delim("abcd_pssrs01.txt")
abcd_pssrs01$sci_score <- as.numeric(abcd_pssrs01$ssrs_6_p) + as.numeric(abcd_pssrs01$ssrs_15r_p) + as.numeric(abcd_pssrs01$ssrs_18_p) + as.numeric(abcd_pssrs01$ssrs_29_p) + as.numeric(abcd_pssrs01$ssrs_35_p) + as.numeric(abcd_pssrs01$ssrs_37_p)
#abcd_pssrs01$sci_score <- abcd_pssrs01$sci_score
abcd_pssrs01$rrb_score <- as.numeric(abcd_pssrs01$ssrs_16_p) +  as.numeric(abcd_pssrs01$ssrs_24_p) + as.numeric(abcd_pssrs01$ssrs_39_p) + as.numeric(abcd_pssrs01$ssrs_42_p) + as.numeric(abcd_pssrs01$ssrs_58_p)
#abcd_pssrs01$rrb_score <- na.omit(rrb_score)

# read in cerebellar tracts
setwd("Melyssa_ASD") #CHANGE THIS TO YOUR OWN DIRECTORY
abcd_cbell <- read.csv("abcd_data_rename_cbell_vars_reharmoized.csv") 
# tracts are averaged according to updated definitions:
# https://docs.google.com/spreadsheets/d/1iUhMbhyo-YTuw4ScTSW8Q5eE7zSvk6uPRaPW1pdVtrM/edit#gid=0
# using wm_average_tract_measures.py
# https://www.dropbox.com/scl/fi/alqca13wn8wneniay517d/wm_average_tract_measures.py?rlkey=89sggxzlq0exs34871g4994sj&dl=0


# KSADS tests
setwd("/ABCDStudyNDA")
#  read in screener form with information about asd
ksads <- read.delim("abcd_ksad01.txt") 
ksads_BL <- ksads[ksads$eventname == "baseline_year_1_arm_1",] 



##### Merge data by subjectkey #######
# merge different types of data to create a single dataframe based on subjectkey
#list of data frames to be merged
abcd_data_list <- list(
  abcd_cbell,
  abcd_screen01, 
  acspsw03_BL,
  abcd_tbss01_BL,
  abcd_pssrs01,
  abcd_dmdtifp202_BL)

# create a new data frame that will have all the other dataframes combined
abcd_data <- NULL
abcd_data <- Reduce(function(x, y) merge(x, y, all.y=FALSE, by = "subjectkey"), abcd_data_list)
length(abcd_data)
length(abcd_data$subjectkey)

# drop duplicated cols
abcd_data <- subset(abcd_data, select = -c(collection_id.x, dataset_id.x, 
                                           src_subject_id.x, 
                                           interview_date.x, interview_age.x, 
                                           sex.x, eventname.x, collection_title.x, 
                                           study_cohort_name.x, collection_id.y, 
                                           dataset_id.y, src_subject_id.y, interview_age.y, 
                                           interview_date.y, sex.y, eventname.y, 
                                           collection_title.y, study_cohort_name.y))


length(abcd_data)
length(abcd_data$subjectkey)
length(abcd_data$PaT_Num_Fibers)


##### Filter Out Some Participants #######
# remove participants where it is not clear if they have ASD or not
# remove participants with common diagnoses (scrn_commondx) from control group scrn_asd == 0
table(abcd_data$scrn_asd)
table(abcd_data$scrn_commondx)
abcd_data_filtered <- filter(abcd_data, (abcd_data$scrn_asd == "0" | abcd_data$scrn_asd == "1"))
# they have ASD or they dont. irrespective of common diagnoses
table(abcd_data_filtered$scrn_asd)
table(abcd_data_filtered$scrn_commondx)


# Filter out missing data and filter out pathways without streamlines
MISSING <- is.na(abcd_data_filtered$nihtbx_totalcomp_uncorrected) | 
  is.na(abcd_data_filtered$sci_score) | is.na(abcd_data_filtered$rrb_score) | 
  abcd_data_filtered$PaT_Num_Fibers == 0 | abcd_data_filtered$IP_Num_Fibers == 0 | abcd_data_filtered$ICP_Num_Fibers == 0 | 
  abcd_data_filtered$MCP_Num_Fibers == 0 | abcd_data_filtered$SCP_Num_Fibers == 0
sum(MISSING) # number of participants we are excluding
abcd_data_filtered_no_NA <- subset(abcd_data_filtered, subset = !MISSING) # this is the data we want to use for all subsequent analyses

# table final totals of included participants
table(abcd_data_filtered_no_NA$scrn_asd)
table(abcd_data_filtered_no_NA$scrn_commondx)
table(abcd_data_filtered_no_NA$scrn_asd, abcd_data_filtered_no_NA$scrn_commondx)

# cast a few variables as numerical values
abcd_data_filtered_no_NA$interview_age.x <- as.numeric(abcd_data_filtered_no_NA$interview_age.x)
abcd_data_filtered_no_NA$nihtbx_totalcomp_uncorrected <- as.numeric(abcd_data_filtered_no_NA$nihtbx_totalcomp_uncorrected)
abcd_data_filtered_no_NA$acs_raked_propensity_score <- as.numeric(abcd_data_filtered_no_NA$acs_raked_propensity_score)
abcd_data_filtered_no_NA$dmdtifp1_1183 <- as.numeric(abcd_data_filtered_no_NA$dmdtifp1_1183)

table(abcd_data_filtered_no_NA$scrn_asd)
table(abcd_data_filtered_no_NA$scrn_commondx)
abcd_data_filtered_commondx <- filter(abcd_data_filtered_no_NA, ((abcd_data_filtered_no_NA$scrn_asd == "0" & abcd_data_filtered_no_NA$scrn_commondx == "0") | abcd_data_filtered_no_NA$scrn_asd == "1"))
# they have ASD or they dont. irrespective of common diagnoses
table(abcd_data_filtered_commondx$scrn_asd)
table(abcd_data_filtered_commondx$scrn_commondx)

abcd_data_filtered_commondx_both <- filter(abcd_data_filtered_no_NA, ((abcd_data_filtered_no_NA$scrn_asd == "0" & abcd_data_filtered_no_NA$scrn_commondx == "0") | (abcd_data_filtered_no_NA$scrn_asd == "1" &  abcd_data_filtered_no_NA$scrn_commondx == "0")))
# they have ASD or they dont. irrespective of common diagnoses
table(abcd_data_filtered_commondx_both$scrn_asd)
table(abcd_data_filtered_commondx_both$scrn_commondx)



# test these two datasets
# abcd_data_filtered_commondx #original group smaller
# abcd_data_filtered_no_NA #new group bigger


##### MANCOVAs #######
# we would like to see if there are differences in the microstructural measures
# of the cerebellar pathways across subjects with or without ASD

# Number of Streamlines Mancova
# are the number of streamlines significantly different across asd and non-asd
# groups when we take into account age and sex.

nos_manova <- manova(cbind(PaT_Num_Fibers, IP_Num_Fibers, ICP_Num_Fibers, MCP_Num_Fibers, SCP_Num_Fibers) ~ scrn_asd  + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183, 
                     data = abcd_data_filtered_commondx, weights = acs_raked_propensity_score)
summary(nos_manova)
summary.aov(nos_manova)

nos_manova <- manova(cbind(PaT_Num_Fibers, IP_Num_Fibers, ICP_Num_Fibers, MCP_Num_Fibers, SCP_Num_Fibers) ~ scrn_asd  + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183, 
                     data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
summary(nos_manova)
summary.aov(nos_manova)


nos_manova_commondx <- manova(cbind(PaT_Num_Fibers, IP_Num_Fibers, ICP_Num_Fibers, MCP_Num_Fibers, SCP_Num_Fibers) ~ scrn_asd  + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected  + dmdtifp1_1183, 
                     data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
summary(nos_manova_commondx)
summary.aov(nos_manova_commondx)

nos_manova_commondx_interaction <- manova(cbind(PaT_Num_Fibers, IP_Num_Fibers, ICP_Num_Fibers, MCP_Num_Fibers, SCP_Num_Fibers) ~ scrn_asd + scrn_commondx  + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected  + dmdtifp1_1183, 
                                          data = abcd_data_filtered_commondx, weights = acs_raked_propensity_score)
summary(nos_manova_commondx_interaction)
summary.aov(nos_manova_commondx_interaction)


nos_manova_commondx_interaction <- manova(cbind(PaT_Num_Fibers, IP_Num_Fibers, ICP_Num_Fibers, MCP_Num_Fibers, SCP_Num_Fibers) ~ scrn_asd * scrn_commondx  + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected  + dmdtifp1_1183, 
                              data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
summary(nos_manova_commondx_interaction)
summary.aov(nos_manova_commondx_interaction)

nos_manova_commondx_both <- manova(cbind(PaT_Num_Fibers, IP_Num_Fibers, ICP_Num_Fibers, MCP_Num_Fibers, SCP_Num_Fibers) ~ scrn_asd  + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected, 
                                          data = abcd_data_filtered_commondx_both, weights = acs_raked_propensity_score)
summary(nos_manova_commondx_both)
summary.aov(nos_manova_commondx_both)


#average NoS in SCP ASD group
ASD_group <- abcd_data_filtered_no_NA$scrn_asd == 1
ASD_data <- abcd_data_filtered_no_NA[ASD_group ,]
ASD_data <- na.omit(ASD_data)

mean(ASD_data$SCP_Num_Fibers)
sd(ASD_data$SCP_Num_Fibers)

#descriptive stats for ASD group 
mean(ASD_data$interview_age.x)
sd(ASD_data$interview_age.x)

table(ASD_data$sex.x)

mean(ASD_data$nihtbx_totalcomp_uncorrected)
sd(ASD_data$nihtbx_totalcomp_uncorrected)

mean(ASD_data$sci_score)
sd(ASD_data$sci_score)

mean(ASD_data$rrb_score)
sd(ASD_data$rrb_score)

table(ASD_data$scrn_commondx)

#descriptive stats for non-ASD group
no_ASD_group <- abcd_data_filtered_no_NA$scrn_asd == 0
no_ASD_data <- abcd_data_filtered_no_NA[no_ASD_group ,]
no_ASD_data <- na.omit(no_ASD_data)

# SCP NoS controls
mean(no_ASD_data$SCP_Num_Fibers)
sd(no_ASD_data$SCP_Num_Fibers)

#descriptive stats for non-ASD group 
mean(no_ASD_data$interview_age.x)
sd(no_ASD_data$interview_age.x)

table(no_ASD_data$sex.x)

mean(no_ASD_data$nihtbx_totalcomp_uncorrected)
sd(no_ASD_data$nihtbx_totalcomp_uncorrected)

mean(no_ASD_data$sci_score)
sd(no_ASD_data$sci_score)

mean(no_ASD_data$rrb_score)
sd(no_ASD_data$rrb_score)

table(no_ASD_data$scrn_commondx)

# Fractional Anisotropy Mancova
fa_manova <- manova(cbind(PaT_FA_tensor1, IP_FA_tensor1, ICP_FA_tensor1, MCP_FA_tensor1, SCP_FA_tensor1) ~ scrn_asd + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183, 
                    data = abcd_data_filtered_commondx, weights = acs_raked_propensity_score)
summary(fa_manova)
summary.aov(fa_manova, test="Pillai")

fa_manova_commondx <- manova(cbind(PaT_FA_tensor1, IP_FA_tensor1, ICP_FA_tensor1, MCP_FA_tensor1, SCP_FA_tensor1) ~ scrn_asd*scrn_commondx + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183, 
                    data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
summary(fa_manova_commondx)
summary.aov(fa_manova_commondx, test="Pillai")


# Mean Diffusivity Mancova
md_manova <- manova(cbind(PaT_MD_tensor1, IP_MD_tensor1, ICP_MD_tensor1, MCP_MD_tensor1, SCP_MD_tensor1) ~ scrn_asd + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183, 
                    data = abcd_data_filtered_commondx, weights = acs_raked_propensity_score)
summary(md_manova)
summary.aov(md_manova, test="Pillai")

md_manova_commondx <- manova(cbind(PaT_MD_tensor1, IP_MD_tensor1, ICP_MD_tensor1, MCP_MD_tensor1, SCP_MD_tensor1) ~ scrn_asd + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183 + scrn_commondx, 
                    data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
summary(md_manova_commondx)
summary.aov(md_manova_nocommondx, test="Pillai")



###### Regression ######
#FA regressions for social communication impairment (SCI)
sci_lm_fa_pat <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_fa_ip <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_fa_icp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_fa_mcp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_fa_scp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)

# old p.adjust(c(0.000926, 0.99568, 0.72457, 0.21567, 0.38876), method = 'fdr')
p.adjust(c(0.00177, 0.62490, 0.69819, 0.23510, 0.23393), method = 'fdr')

#FA regressions for restrictive repetitive behavior (RRB)
rrb_lm_fa_pat <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_fa_ip <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_fa_icp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_fa_mcp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_fa_scp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)

# interaction between PaT FA and ASD is significant
summary(sci_lm_fa_pat) 

# all other models do not have this significant interaction 
summary(sci_lm_fa_ip) 
summary(sci_lm_fa_icp)
summary(sci_lm_fa_mcp)
summary(sci_lm_fa_scp)

summary(rrb_lm_fa_pat)
summary(rrb_lm_fa_ip)
summary(rrb_lm_fa_icp)
summary(rrb_lm_fa_mcp)
summary(rrb_lm_fa_scp)

#p.adjust(c(, , , , ), method = 'fdr')


#### plot interaction effect ####
# install and load effect package
install.packages('effects') 
library("effects")
# select predictor effect of interest and relevant model
pat_fa_sci_predictor_effect <- predictorEffect("PaT_FA_tensor1", sci_lm_fa_pat)
plot(pat_fa_sci_predictor_effect) # plot the predictor effect
# plot it a bit more nicely
plot(pat_fa_sci_predictor_effect, lines=list(multiline = TRUE), confint=list(style="auto"),
     axes=list(
      grid=TRUE,
       x=list(rotate=30,
                rug=TRUE,
                PaT_FA_tensor1=list(
                              lim=c(min(abcd_data_filtered_no_NA$PaT_FA_tensor1), max(abcd_data_filtered_no_NA$PaT_FA_tensor1)))
                )))

#NoS regressions for social communication impairment (SCI)
sci_lm_NoS_pat <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_NoS_ip <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_NoS_icp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_NoS_mcp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_NoS_scp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)

#old p.adjust(c(0.47444, 0.01004, 0.000428, 0.36290, 0.00262), method = 'fdr')
p.adjust(c(0.36592, 0.00749, 0.000157, 0.53855, 0.000759), method = 'fdr')

#NoS regressions for restrictive repetitive behavior (RRB)
rrb_lm_NoS_pat <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_NoS_ip <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_NoS_icp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_NoS_mcp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_NoS_scp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)

# old p.adjust(c(0.3444, 0.6151, 0.314, 0.412, 8.26e-06), method = 'fdr')
p.adjust(c(0.300, 0.388, 0.0423, 0.595, 2.83e-07), method = 'fdr')

#significant interactions
summary(sci_lm_NoS_ip) 
summary(sci_lm_NoS_icp)
summary(sci_lm_NoS_scp)

summary(rrb_lm_NoS_scp)

#not significant 
summary(sci_lm_NoS_pat)
summary(sci_lm_NoS_mcp)

summary(rrb_lm_NoS_pat)
summary(rrb_lm_NoS_ip)
summary(rrb_lm_NoS_icp)
summary(rrb_lm_NoS_mcp)

# select predictor effect of interest and relevant model
pat_md_sci_predictor_effect <- predictorEffect("PaT_MD_tensor1", sci_lm_md_pat)
plot(pat_md_sci_predictor_effect) # plot the predictor effect
# plot it a bit more nicely
plot(pat_md_sci_predictor_effect, lines=list(multiline = TRUE), confint=list(style="auto"),
     axes=list(
       grid=TRUE,
       x=list(rotate=30,
              rug=TRUE,
              SCP_Num_Fibers=list(
                lim=c(min(abcd_data_filtered_no_NA$PaT_MD_tensor1), max(abcd_data_filtered_no_NA$PaT_MD_tensor1)))
       )))

#non-significant interactions 
summary(sci_lm_NoS_pat)
summary(sci_lm_NoS_mcp)

summary(rrb_lm_NoS_pat)
summary(rrb_lm_NoS_ip)
summary(rrb_lm_NoS_icp)
summary(rrb_lm_NoS_mcp)


#MD regressions for social communication impairment (SCI)
sci_lm_md_pat <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_md_ip <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_md_icp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_md_mcp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
sci_lm_md_scp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)

# old p.adjust(c(0.010857, 0.28925, 0.11111, 0.25059, 0.47162), method = 'fdr')
p.adjust(c(0.013002, 0.17629, 0.09147, 0.44463, 0.21743), method = 'fdr')

#MD regressions for restrictive repetitive behavior (RRB)
rrb_lm_md_pat <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_md_ip <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_md_icp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_md_mcp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
rrb_lm_md_scp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)

#old p.adjust(c(0.4818, 0.158, 0.0013, 0.8677, 0.753), method = 'fdr')
p.adjust(c(0.577, 0.0906, 0.00115, 0.616, 0.620), method = 'fdr')

#significant interactions 
summary(sci_lm_md_pat)
summary(rrb_lm_md_icp)


#interactions that are not significant 
summary(sci_lm_md_ip) 
summary(sci_lm_md_icp)
summary(sci_lm_md_mcp)
summary(sci_lm_md_scp)

summary(rrb_lm_md_pat)
summary(rrb_lm_md_ip)
summary(rrb_lm_md_mcp)
summary(rrb_lm_md_scp)





write.csv(no_ASD_data, "no_ASD_data.csv")
write.csv(ASD_data, "ASD_data.csv")
write.csv(abcd_data_filtered_no_NA, "abcd_data_filtered_no_NA.csv")











