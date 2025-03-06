
####### START ######

setwd()

# Import DDM output
ddmOutput <- read.csv("ddm_output_data.csv")

#Create sublist
# verify that there is the expected number of subjects
subList <- unique(ddmOutput$subject)



############ Reject anyone with less than 20 trials in post-error or post-correct ###############

#read in the cleaned pre-ddm sheet
clean_pre_ddm_df <- read.csv("clean_flanker_data.csv")


# Add a column for number of trials to ddmOutput
ddmOutput$numTrials <- NA

for (i in 1:length(subList)){
  
  #make a df where we only look at the i-th sub
  singlesubDat <- subset(clean_pre_ddm_df, clean_pre_ddm_df$subject==subList[i])
  
  #calculate number of post-errors (100)
  numPostError <- nrow(singlesubDat[singlesubDat$priorAccuracy == 100, ])
  #calculate number of post-corrects (111)
  numPostCorrect <- nrow(singlesubDat[singlesubDat$priorAccuracy == 111, ])
  
  # when subject in ddmoutput is the one specified in the loop, and whichcondition in ddmOutput is 100, put numPostError (defined above) in it
  ddmOutput$numTrials[subList[i]==ddmOutput$subject & ddmOutput$whichCondition==100] <- numPostError
  # when subject in ddmoutput is the one specified in the loop, and whichcondition in ddmOutput is 111, put numPostCorrect (defined above) in it
  ddmOutput$numTrials[subList[i]==ddmOutput$subject & ddmOutput$whichCondition==111] <- numPostCorrect

}

# Put NAs for people with too few trials in each condition
ddmOutput$numTrials[ddmOutput$numTrials<20] <-NA


############# NA bad fitstats & iternum ###################


#put an NA for error fitstats that are above 200 
ddmOutput$fitStat[ddmOutput$fitStat>200] <-NA

# rejecting anyone with a ceiling number of iterations
ddmOutput$iterNum[ddmOutput$iterNum==200] <-NA


############ Create a new df that's more conducive to subtraction ###################


#initialize the df
analysisDat<-  data.frame(matrix(ncol=26, nrow=0))
colnames(analysisDat) <- c("subject","a_err", "a_corr", "a_diff", "ter_err", "ter_corr", "ter_diff", 
                           "p_err", "p_corr", "p_diff", "rd_err", "rd_corr", "rd_diff", 
                           "sda_err", "sda_corr", "sda_diff", "sda_rd_ratio_err", "sda_rd_ratio_corr", "sda_rd_ratio_diff",
                           "fitStat_err", "fitStat_corr", "fitStat_diff", "iterNum_err", "iterNum_corr",  
                           "numTrials_postErr", "numTrials_postCorr")

# adding the data from ddmOutput to analysisDat
for (i in 1:length(subList)){
  
  # subject variable- attaching 100 to it so it pulls the subject number only once instead of storing a two variable list
  subject <- ddmOutput$subject[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  
  # pull a when it's post-error and post-correct
  a_err <- ddmOutput$a[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  a_corr <- ddmOutput$a[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  # It is easy to do the subtraction here, but I want to do it after we check for outliers, bc it's easier to mark the diff as NA that way
  a_diff <- NA
  
  # pull ter when it's post-error and post-correct
  ter_err <- ddmOutput$ter[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  ter_corr <- ddmOutput$ter[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  ter_diff <- NA #will check for outliers before doing subtraction
  
  # pull p when it's post-error and post-correct
  p_err <- ddmOutput$p[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  p_corr <- ddmOutput$p[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  p_diff <- NA #will check for outliers before doing subtraction
  
  # pull rd when it's post-error and post-correct
  rd_err <- ddmOutput$rd[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  rd_corr <- ddmOutput$rd[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  rd_diff <- NA #will check for outliers before doing subtraction
  
  # pull sda when it's post-error and post-correct
  sda_err <- ddmOutput$sda[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  sda_corr <- ddmOutput$sda[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  sda_diff <- NA #will check for outliers before doing subtraction
  
  # make sda/rd ratios
  sda_rd_ratio_err <- NA #will check for outliers before doing calculation
  sda_rd_ratio_corr <- NA #will check for outliers before doing calculation
  sda_rd_ratio_diff <- NA #will check for outliers before doing calculation
  
  # pull fitStat when it's post-error and post-correct
  fitStat_err <- ddmOutput$fitStat[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  fitStat_corr <- ddmOutput$fitStat[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  fitStat_diff <- fitStat_err-fitStat_corr # can do subtraction here bc fitStat threshold is dealt with above
  
  # pull iterNum when it's post-error and post-correct
  iterNum_err <- ddmOutput$iterNum[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  iterNum_corr <- ddmOutput$iterNum[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  
  # pull numTrials when it's post-error and post-correct
  numTrials_postErr <- ddmOutput$numTrials[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  numTrials_postCorr <- ddmOutput$numTrials[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  
  # make new row with values created above
  newRow <- data.frame(subject, a_err, a_corr, a_diff, ter_err, ter_corr, ter_diff, p_err, p_corr, p_diff, rd_err, rd_corr, rd_diff, 
                       sda_err, sda_corr, sda_diff, sda_rd_ratio_err, sda_rd_ratio_corr, sda_rd_ratio_diff,
                       fitStat_err, fitStat_corr, fitStat_diff, iterNum_err, iterNum_corr,  
                       numTrials_postErr, numTrials_postCorr)
  colnames(newRow)<- c("subject","a_err", "a_corr", "a_diff", "ter_err", "ter_corr", "ter_diff", 
                       "p_err", "p_corr", "p_diff", "rd_err", "rd_corr", "rd_diff", 
                       "sda_err", "sda_corr", "sda_diff", "sda_rd_ratio_err", "sda_rd_ratio_corr", "sda_rd_ratio_diff",
                       "fitStat_err", "fitStat_corr", "fitStat_diff", "iterNum_err", "iterNum_corr",
                       "numTrials_postErr", "numTrials_postCorr")
  
  # add row to the df
  analysisDat <- rbind(analysisDat, newRow)
  
}




############# add in SCARED, age, stim_med, gender, CDI ###################

collectiveDat <- read.csv("survey_data.csv")

subList <- unique(analysisDat$subject)

#new column for child age
analysisDat[ , "age"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers and entering age data
  analysisDat$age[analysisDat$subject== subList[i]] <- collectiveDat$age[collectiveDat$subject== subList[i]]
}

#new column for child gender
analysisDat[ , "chgender"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers and entering gender data
  analysisDat$chgender[analysisDat$subject== subList[i]] <- collectiveDat$chgender[collectiveDat$subject== subList[i]]
}

# SCARED child data
analysisDat[ , c("SCARED_01_Ch_PRE","SCARED_02_Ch_PRE","SCARED_03_Ch_PRE","SCARED_04_Ch_PRE","SCARED_05_Ch_PRE","SCARED_06_Ch_PRE","SCARED_07_Ch_PRE","SCARED_08_Ch_PRE","SCARED_09_Ch_PRE","SCARED_10_Ch_PRE","SCARED_11_Ch_PRE","SCARED_12_Ch_PRE","SCARED_13_Ch_PRE","SCARED_14_Ch_PRE","SCARED_15_Ch_PRE","SCARED_16_Ch_PRE","SCARED_17_Ch_PRE","SCARED_18_Ch_PRE","SCARED_19_Ch_PRE","SCARED_20_Ch_PRE","SCARED_21_Ch_PRE","SCARED_22_Ch_PRE","SCARED_23_Ch_PRE","SCARED_24_Ch_PRE","SCARED_25_Ch_PRE","SCARED_26_Ch_PRE","SCARED_27_Ch_PRE","SCARED_28_Ch_PRE","SCARED_29_Ch_PRE","SCARED_30_Ch_PRE","SCARED_31_Ch_PRE","SCARED_32_Ch_PRE","SCARED_33_Ch_PRE","SCARED_34_Ch_PRE","SCARED_35_Ch_PRE","SCARED_36_Ch_PRE","SCARED_37_Ch_PRE","SCARED_38_Ch_PRE","SCARED_39_Ch_PRE","SCARED_40_Ch_PRE","SCARED_41_Ch_PRE")] <- NA
for (i in 1:length(subList)){
  #matching sub numbers and entering SCARED data
  analysisDat[analysisDat$subject== subList[i], c(29:69)] <- collectiveDat[collectiveDat$subject== subList[i], c(3:43)]
}

#new column for child SCARED
analysisDat[ , "SCARED_total_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat$SCARED_total_Ch_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_total_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED GAD
analysisDat[ , "SCARED_GAD_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat$SCARED_GAD_Ch_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_GAD_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SAD separation
analysisDat[ , "SCARED_SAD_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat$SCARED_SAD_Ch_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$subject==subList[i]]
}

# SCARED parent data
analysisDat[ , c("SCARED_01_Par_PRE","SCARED_02_Par_PRE","SCARED_03_Par_PRE","SCARED_04_Par_PRE","SCARED_05_Par_PRE","SCARED_06_Par_PRE","SCARED_07_Par_PRE","SCARED_08_Par_PRE","SCARED_09_Par_PRE","SCARED_10_Par_PRE","SCARED_11_Par_PRE","SCARED_12_Par_PRE","SCARED_13_Par_PRE","SCARED_14_Par_PRE","SCARED_15_Par_PRE","SCARED_16_Par_PRE","SCARED_17_Par_PRE","SCARED_18_Par_PRE","SCARED_19_Par_PRE","SCARED_20_Par_PRE","SCARED_21_Par_PRE","SCARED_22_Par_PRE","SCARED_23_Par_PRE","SCARED_24_Par_PRE","SCARED_25_Par_PRE","SCARED_26_Par_PRE","SCARED_27_Par_PRE","SCARED_28_Par_PRE","SCARED_29_Par_PRE","SCARED_30_Par_PRE","SCARED_31_Par_PRE","SCARED_32_Par_PRE","SCARED_33_Par_PRE","SCARED_34_Par_PRE","SCARED_35_Par_PRE","SCARED_36_Par_PRE","SCARED_37_Par_PRE","SCARED_38_Par_PRE","SCARED_39_Par_PRE","SCARED_40_Par_PRE","SCARED_41_Par_PRE")] <- NA
for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat[analysisDat$subject== subList[i], c(73:113)] <- collectiveDat[collectiveDat$subject== subList[i], c(50:90)]
}


#new column for parent SCARED total
analysisDat[ , "SCARED_total_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat$SCARED_total_Par_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_total_Par_PRE[collectiveDat$subject==subList[i]]
}

#new column for parent SCARED GAD
analysisDat[ , "SCARED_GAD_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat$SCARED_GAD_Par_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_GAD_Par_PRE[collectiveDat$subject==subList[i]]
}

#new column for parent SAD separation
analysisDat[ , "SCARED_SAD_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering SCARED data
  analysisDat$SCARED_SAD_Par_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$subject==subList[i]]
}


# ACS data
analysisDat[ , c("ACS_total_Ch_PRE_15item",	"ACS_attnFocus_Ch_PRE_15item",	"ACS_attnShift_Ch_PRE_15item")] <- NA
for (i in 1:length(subList)){
  #matching sub numbers  and entering ACS data
  analysisDat[analysisDat$subject== subList[i], c(117:119)] <- collectiveDat[collectiveDat$subject== subList[i], c(132:134)]
}

#new column for stimulant medications
analysisDat[ , "stim_meds_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering stim_meds data
  analysisDat$stim_meds_PRE[analysisDat$subject==subList[i]] <- collectiveDat$stim_meds_PRE[collectiveDat$subject==subList[i]]
}

#new column for diagnoses
analysisDat[ , c("finaldx1_PRE",	"finaldx2_PRE",	"finaldx3_PRE",	"finaldx4_PRE",	"finaldx5_PRE",	"finaldx6_PRE")] <- NA

for (i in 1:length(subList)){
  #matching sub numbers  and entering diagnosis data
  analysisDat$finaldx1_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx1_PRE[collectiveDat$subject==subList[i]]
 
  analysisDat$finaldx2_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx2_PRE[collectiveDat$subject==subList[i]]
 
  analysisDat$finaldx3_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx3_PRE[collectiveDat$subject==subList[i]]
  
  analysisDat$finaldx4_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx4_PRE[collectiveDat$subject==subList[i]]
  
  analysisDat$finaldx5_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx5_PRE[collectiveDat$subject==subList[i]]
  
  analysisDat$finaldx6_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx6_PRE[collectiveDat$subject==subList[i]]
}


# read in csv for Acc and RT
RT_Acc_df <- read.csv("RT_Acc_df_for_clean_flanker_data.csv")

analysisDat[ , c("Acc_avg", "RT_avg", "cong_Acc_avg", "incong_Acc_avg", 
                 "corr_cong_RT_avg", "corr_incong_RT_avg","err_cong_RT_avg","err_incong_RT_avg",
                 "num_corr_cong_trials","num_corr_incong_trials", "num_err_cong_trials", "num_err_incong_trials", 
                 "PE_Acc_avg", "PC_Acc_avg", "PE_RT_avg", "PC_RT_avg", "PE_corr_RT_avg", "PC_corr_RT_avg")] <- NA

for (i in 1:length(subList)){
  #matching sub numbers, entering data
  analysisDat[analysisDat$subject==subList[i], c(127:144)] <- RT_Acc_df[RT_Acc_df$subject == subList[i],c(2:19)]
}

#new columns for medication type classification

analysisDat[ , c("childmedpre","med1_name", "med1_dosage","med1_length",
                     "med1_change","med1_stopped","med2_name","med2_dosage",
                     "med2_length","med2_change","med2_stopped","med3_name",
                     "med3_dosage","med3_length","med3_change","med3_stopped",
                     "med1_type", "med2_type","med3_type", "psychoactive_meds_PRE" )] <- NA

for (i in 1:length(subList)){
  #adding data 
  analysisDat[analysisDat$subject==subList[i], c(145:164)] <- collectiveDat[collectiveDat$subject == subList[i],c("childmedpre","med1_name", "med1_dosage","med1_length",
                                                                                                                          "med1_change","med1_stopped","med2_name","med2_dosage",
                                                                                                                          "med2_length","med2_change","med2_stopped","med3_name",
                                                                                                                          "med3_dosage","med3_length","med3_change","med3_stopped",
                                                                                                                  "med1_type", "med2_type","med3_type", "psychoactive_meds_PRE")]
} # end of for loop for this sub

analysisDat1 <- analysisDat


################# Recode gender and stim_meds #######################

library("plyr")
# original gender is male == 1 and female == 2. This will code it so male == -1 and female == 1
analysisDat1$chgender <- mapvalues(analysisDat1$chgender, from = c(1,2,NA), to = c(-1,1,NA))

# original stim_med_PRE is "not on stimulants" == 0 and "taking stimulants" == 1. This will code 
# it so "not on stimulants" == -1 and "taking stimulants" == 1
analysisDat1$stim_meds_PRE <- mapvalues(analysisDat1$stim_meds_PRE, from = c(0,1,NA), to = c(-1,1,NA))


################# Reject based on age #######################

subList<- unique(analysisDat1$subject)

# remove people under 7 and over 17

for(i in 1:length(subList)){
  if(analysisDat1$age[analysisDat1$subject==subList[i]]<7 | analysisDat1$age[analysisDat1$subject==subList[i]]>17){
    analysisDat1 <- subset(analysisDat1, analysisDat1$subject!=subList[i])
  }
}

######## Remove incomplete flanker file #########

rawFlankerLength <- read.csv("rawFlankerLength.csv")

for(i in 1:length(subList)){
  if(rawFlankerLength$flankerLength[rawFlankerLength$subject==subList[i]]!=352){
    analysisDat1 <- subset(analysisDat1, analysisDat1$subject!=subList[i])
  }
}



####### Reject all parms for condition #####

#placeholder
analysisDat2 <- analysisDat1

subList<- unique(analysisDat2$subject)


# If a person has NAs for numTrials (lower than 20), NAs for fitStat (greater than 200), or NAs for iterNum (ceiling value of 200),
# then their post-error parms or post-correct parms are marked with NA.
# Ex. numTrials_postErr is lower than 20, meaning that all of the post-error parms are invalid since there's not enough trials to get a good fit.
# Since you fit post error and post correct separately, they could still have valid parms for one condition but not the other
# Included column 22 (fitStat_diff) because the diff was calculated above, meaning that the diff would 
# remain even tho the post-err/corr fitStat was labeled with NA for low numTrials or ceiling iterNum.
for (i in 1:length(subList)){
  
  # if number of trials for post-err is NA, fitStat_err is NA, or iterNum_err is ceiling (200), then all parms for post-error are NA
  if(is.na(analysisDat2$numTrials_postErr[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$fitStat_err[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$iterNum_err[analysisDat2$subject==subList[i]])){
    analysisDat2[analysisDat2$subject==subList[i],c(2,5,8,11,14,17,20,22)] <- NA
  }
  
  # if number of trials for post-corr is NA, fitStat_corr is NA, or iterNum_corr is ceiling (200), then all parms for post-error are NA
  if(is.na(analysisDat2$numTrials_postCorr[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$fitStat_corr[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$iterNum_corr[analysisDat2$subject==subList[i]])){
    analysisDat2[analysisDat2$subject==subList[i],c(3,6,9,12,15,18,21,22)] <- NA
  }
}


################# Ter- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "ter" for post-error. 
ter_meanminus3sd_err <- mean(analysisDat2$ter_err, na.rm=TRUE)- 3*sd(analysisDat2$ter_err, na.rm=TRUE)
ter_meanplus3sd_err <- mean(analysisDat2$ter_err, na.rm=TRUE)+ 3*sd(analysisDat2$ter_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$ter_err))
# mark NA for any outliers in ter for post-error
analysisDat2$ter_err[analysisDat2$ter_err<ter_meanminus3sd_err | analysisDat2$ter_err>ter_meanplus3sd_err] <-NA


## calculate the SD of "ter" for post-correct
ter_meanminus3sd_corr <- mean(analysisDat2$ter_corr, na.rm=TRUE)- 3*sd(analysisDat2$ter_corr, na.rm=TRUE)
ter_meanplus3sd_corr <- mean(analysisDat2$ter_corr, na.rm=TRUE)+ 3*sd(analysisDat2$ter_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$ter_corr))
# mark NA for any outliers in ter for post-correct
analysisDat2$ter_corr[analysisDat2$ter_corr<ter_meanminus3sd_corr | analysisDat2$ter_corr>ter_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$ter_diff <- analysisDat2$ter_err-analysisDat2$ter_corr

#ter subtraction outlier rejection
ter_diff_meanminus3sd <- mean(analysisDat2$ter_diff, na.rm=TRUE)- 3*sd(analysisDat2$ter_diff, na.rm=TRUE)
ter_diff_meanplus3sd <- mean(analysisDat2$ter_diff, na.rm=TRUE)+ 3*sd(analysisDat2$ter_diff, na.rm=TRUE)
analysisDat2$ter_diff[analysisDat2$ter_diff<ter_diff_meanminus3sd | analysisDat2$ter_diff>ter_diff_meanplus3sd] <-NA


################# a- reject outliers, subtract, reject diff outliers ############


## calculate the outliers for "a" for post-error. 
a_meanminus3sd_err <- mean(analysisDat2$a_err, na.rm=TRUE)- 3*sd(analysisDat2$a_err, na.rm=TRUE)
a_meanplus3sd_err <- mean(analysisDat2$a_err, na.rm=TRUE)+ 3*sd(analysisDat2$a_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$a_err))
# mark NA for any outliers in a for post-error
analysisDat2$a_err[analysisDat2$a_err<a_meanminus3sd_err | analysisDat2$a_err>a_meanplus3sd_err] <-NA
table(is.na(analysisDat2$a_err))


## calculate the SD of "a" for post-correct
a_meanminus3sd_corr <- mean(analysisDat2$a_corr, na.rm=TRUE)- 3*sd(analysisDat2$a_corr, na.rm=TRUE)
a_meanplus3sd_corr <- mean(analysisDat2$a_corr, na.rm=TRUE)+ 3*sd(analysisDat2$a_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$a_corr))
# mark NA for any outliers in a for post-correct
analysisDat2$a_corr[analysisDat2$a_corr<a_meanminus3sd_corr | analysisDat2$a_corr>a_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$a_corr))


#doing the subtraction
analysisDat2$a_diff <- analysisDat2$a_err-analysisDat2$a_corr

#a subtraction outlier rejection
a_diff_meanminus3sd <- mean(analysisDat2$a_diff, na.rm=TRUE)- 3*sd(analysisDat2$a_diff, na.rm=TRUE)
a_diff_meanplus3sd <- mean(analysisDat2$a_diff, na.rm=TRUE)+ 3*sd(analysisDat2$a_diff, na.rm=TRUE)
table(is.na(analysisDat2$a_diff))
analysisDat2$a_diff[analysisDat2$a_diff<a_diff_meanminus3sd | analysisDat2$a_diff>a_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$a_diff))



################# p- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "p" for post-error. 
p_meanminus3sd_err <- mean(analysisDat2$p_err, na.rm=TRUE)- 3*sd(analysisDat2$p_err, na.rm=TRUE)
p_meanplus3sd_err <- mean(analysisDat2$p_err, na.rm=TRUE)+ 3*sd(analysisDat2$p_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$p_err))
# mark NA for any outliers in p for post-error
analysisDat2$p_err[analysisDat2$p_err<p_meanminus3sd_err | analysisDat2$p_err>p_meanplus3sd_err] <-NA
table(is.na(analysisDat2$p_err))


## calculate the SD of "p" for post-correct
p_meanminus3sd_corr <- mean(analysisDat2$p_corr, na.rm=TRUE)- 3*sd(analysisDat2$p_corr, na.rm=TRUE)
p_meanplus3sd_corr <- mean(analysisDat2$p_corr, na.rm=TRUE)+ 3*sd(analysisDat2$p_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$p_corr))
# mark NA for any outliers in p for post-correct
analysisDat2$p_corr[analysisDat2$p_corr<p_meanminus3sd_corr | analysisDat2$p_corr>p_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$p_corr))


#doing the subtraction
analysisDat2$p_diff <- analysisDat2$p_err-analysisDat2$p_corr

#p subtraction outlier rejection
p_diff_meanminus3sd <- mean(analysisDat2$p_diff, na.rm=TRUE)- 3*sd(analysisDat2$p_diff, na.rm=TRUE)
p_diff_meanplus3sd <- mean(analysisDat2$p_diff, na.rm=TRUE)+ 3*sd(analysisDat2$p_diff, na.rm=TRUE)
table(is.na(analysisDat2$p_diff))
analysisDat2$p_diff[analysisDat2$p_diff<p_diff_meanminus3sd | analysisDat2$p_diff>p_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$p_diff))



################# rd- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "rd" for post-error. 
rd_meanminus3sd_err <- mean(analysisDat2$rd_err, na.rm=TRUE)- 3*sd(analysisDat2$rd_err, na.rm=TRUE)
rd_meanplus3sd_err <- mean(analysisDat2$rd_err, na.rm=TRUE)+ 3*sd(analysisDat2$rd_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$rd_err))
# mark NA for any outliers in rd for post-error
analysisDat2$rd_err[analysisDat2$rd_err<rd_meanminus3sd_err | analysisDat2$rd_err>rd_meanplus3sd_err] <-NA
table(is.na(analysisDat2$rd_err))


## calculate the SD of "rd" for post-correct
rd_meanminus3sd_corr <- mean(analysisDat2$rd_corr, na.rm=TRUE)- 3*sd(analysisDat2$rd_corr, na.rm=TRUE)
rd_meanplus3sd_corr <- mean(analysisDat2$rd_corr, na.rm=TRUE)+ 3*sd(analysisDat2$rd_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$rd_corr))
# mark NA for any outliers in rd for post-correct
analysisDat2$rd_corr[analysisDat2$rd_corr<rd_meanminus3sd_corr | analysisDat2$rd_corr>rd_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$rd_corr))


#doing the subtraction
analysisDat2$rd_diff <- analysisDat2$rd_err-analysisDat2$rd_corr

#rd subtraction outlier rejection
rd_diff_meanminus3sd <- mean(analysisDat2$rd_diff, na.rm=TRUE)- 3*sd(analysisDat2$rd_diff, na.rm=TRUE)
rd_diff_meanplus3sd <- mean(analysisDat2$rd_diff, na.rm=TRUE)+ 3*sd(analysisDat2$rd_diff, na.rm=TRUE)
table(is.na(analysisDat2$rd_diff))
analysisDat2$rd_diff[analysisDat2$rd_diff<rd_diff_meanminus3sd | analysisDat2$rd_diff>rd_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$rd_diff))


################# sda- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "sda" for post-error. 
sda_meanminus3sd_err <- mean(analysisDat2$sda_err, na.rm=TRUE)- 3*sd(analysisDat2$sda_err, na.rm=TRUE)
sda_meanplus3sd_err <- mean(analysisDat2$sda_err, na.rm=TRUE)+ 3*sd(analysisDat2$sda_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_err))
# mark NA for any outliers in sda for post-error
analysisDat2$sda_err[analysisDat2$sda_err<sda_meanminus3sd_err | analysisDat2$sda_err>sda_meanplus3sd_err] <-NA
table(is.na(analysisDat2$sda_err))


## calculate the SD of "sda" for post-correct
sda_meanminus3sd_corr <- mean(analysisDat2$sda_corr, na.rm=TRUE)- 3*sd(analysisDat2$sda_corr, na.rm=TRUE)
sda_meanplus3sd_corr <- mean(analysisDat2$sda_corr, na.rm=TRUE)+ 3*sd(analysisDat2$sda_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_corr))
# mark NA for any outliers in sda for post-correct
analysisDat2$sda_corr[analysisDat2$sda_corr<sda_meanminus3sd_corr | analysisDat2$sda_corr>sda_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$sda_corr))


#doing the subtraction
analysisDat2$sda_diff <- analysisDat2$sda_err-analysisDat2$sda_corr

#sda subtraction outlier rejection
sda_diff_meanminus3sd <- mean(analysisDat2$sda_diff, na.rm=TRUE)- 3*sd(analysisDat2$sda_diff, na.rm=TRUE)
sda_diff_meanplus3sd <- mean(analysisDat2$sda_diff, na.rm=TRUE)+ 3*sd(analysisDat2$sda_diff, na.rm=TRUE)
table(is.na(analysisDat2$sda_diff))
analysisDat2$sda_diff[analysisDat2$sda_diff<sda_diff_meanminus3sd | analysisDat2$sda_diff>sda_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$sda_diff))


################# sda/rd- reject outliers, subtract, reject diff outliers ############

## calculate the ratio for error
analysisDat2$sda_rd_ratio_err <- (analysisDat2$sda_err)/(analysisDat2$rd_err)
## calculate the ratio for correct
analysisDat2$sda_rd_ratio_corr <- (analysisDat2$sda_corr)/(analysisDat2$rd_corr)


## calculate the outliers for "sda/rd" for post-error. 
sdard_meanminus3sd_err <- mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)
sdard_meanplus3sd_err <- mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_rd_ratio_err))
# mark NA for any outliers in ratio for post-error
analysisDat2$sda_rd_ratio_err[analysisDat2$sda_rd_ratio_err<sdard_meanminus3sd_err | analysisDat2$sda_rd_ratio_err>sdard_meanplus3sd_err] <-NA
table(is.na(analysisDat2$sda_rd_ratio_err))


## calculate the SD of "sda/rd" for post-correct
sdard_meanminus3sd_corr <- mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)
sdard_meanplus3sd_corr <- mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_rd_ratio_corr))
# mark NA for any outliers in ter for post-correct
analysisDat2$sda_rd_ratio_corr[analysisDat2$sda_rd_ratio_corr<sdard_meanminus3sd_corr | analysisDat2$sda_rd_ratio_corr>sdard_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$sda_rd_ratio_corr))


#doing the subtraction
analysisDat2$sda_rd_ratio_diff <- analysisDat2$sda_rd_ratio_err-analysisDat2$sda_rd_ratio_corr

#sda/rd subtraction outlier rejection
sdard_diff_meanminus3sd <- mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)
sdard_diff_meanplus3sd <- mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)
table(is.na(analysisDat2$sda_rd_ratio_corr))
analysisDat2$sda_rd_ratio_diff[analysisDat2$sda_rd_ratio_diff<sdard_diff_meanminus3sd | analysisDat2$sda_rd_ratio_diff>sdard_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$sda_rd_ratio_corr))


# triple checking that all NA are actually NA
analysisDat2[analysisDat2=="NA"] <- NA


################ Converting wide data to long #############################

# in order to mixed effects models, the data needs to be long instead of wide. 

analysisDat3 <- analysisDat2

analysisDat3_long <- reshape(analysisDat3, idvar="subject", direction="long", 
        varying=list(a=c(2,3), ter=c(5,6), p=c(8,9), rd=c(11,12), sda=c(14,15), sda_rd_ratio=c(17,18)),
        # -1 is post-correct and 1 is post-error
        times=c('1', '-1'),
        v.names = c("a_measurement", "ter_measurement", "p_measurement", "rd_measurement", "sda_measurement", "sda_rd_ratio_measurement"))

#renaming the column called "time" to "PE_PC"
colnames(analysisDat3_long)[which(names(analysisDat3_long) == "time")] <- "PE_PC"

analysisDat3_long_unscaled <- analysisDat3_long

#converting the behavioral analyses into long format. This will be done in separate tables bc we do not run analyses with them together
# there are no PE_PC considerations (prior congruency, prior trial fast RT, or first trial of block) for these analyses
library("tidyverse")

# this one is for accuracy analyses based on congruency
analysisDat3_long_acc <- pivot_longer(analysisDat3, c(cong_Acc_avg,	incong_Acc_avg), names_to = "avgAcc_congruency",
                                      names_pattern = "(.*)_Acc_avg", values_to = "avgAcc_value")
# converting the names to binary (congruent == 1 and incongruent == -1)
analysisDat3_long_acc$avgAcc_congruency[analysisDat3_long_acc$avgAcc_congruency=="cong"] <- 1
analysisDat3_long_acc$avgAcc_congruency[analysisDat3_long_acc$avgAcc_congruency=="incong"] <- -1


# This one is for reaction time analyses based on congruent/incongruent for corrects only
analysisDat3_long_RT <- pivot_longer(analysisDat3, c(corr_cong_RT_avg,	corr_incong_RT_avg), names_to = "avgRT_congruency",
                                     names_pattern = "corr_(.*)_RT_avg", values_to = "avgRT_value")
analysisDat3_long_RT$avgRT_congruency[analysisDat3_long_RT$avgRT_congruency=="cong"] <- 1
analysisDat3_long_RT$avgRT_congruency[analysisDat3_long_RT$avgRT_congruency=="incong"] <- -1


# For post-trial accuracy
# -1 is post-correct and 1 is post-error
analysisDat3_long_PE_Acc <- pivot_longer(analysisDat3, c(PE_Acc_avg,	PC_Acc_avg), names_to = "PE_PC",
                                         names_pattern = "(.*)_Acc_avg", values_to = "PE_PC_Acc_value")
analysisDat3_long_PE_Acc$PE_PC[analysisDat3_long_PE_Acc$PE_PC=="PE"] <- 1
analysisDat3_long_PE_Acc$PE_PC[analysisDat3_long_PE_Acc$PE_PC=="PC"] <- -1

# For post-trial RT (corrects only)
# -1 is post-correct and 1 is post-error
analysisDat3_long_PE_corr_RT <- pivot_longer(analysisDat3, c(PE_corr_RT_avg,	PC_corr_RT_avg), names_to = "PE_PC",
                                             names_pattern = "(.*)_corr_RT_avg", values_to = "PE_PC_corr_RT_value")
analysisDat3_long_PE_corr_RT$PE_PC[analysisDat3_long_PE_corr_RT$PE_PC=="PE"] <- 1
analysisDat3_long_PE_corr_RT$PE_PC[analysisDat3_long_PE_corr_RT$PE_PC=="PC"] <- -1





#################### Prep for mixed effects models #############################


library(nlme)
library(emmeans)
library(ggplot2)
library(sjPlot)

# Factorize the categorical variables
analysisDat3_long$PE_PC <- as.factor(analysisDat3_long$PE_PC)
analysisDat3_long$chgender <- as.factor(analysisDat3_long$chgender)
analysisDat3_long$stim_meds_PRE <- as.factor(analysisDat3_long$stim_meds_PRE)

analysisDat3_long_acc$avgAcc_congruency <- as.factor(analysisDat3_long_acc$avgAcc_congruency)
analysisDat3_long_acc$chgender <- as.factor(analysisDat3_long_acc$chgender)
analysisDat3_long_acc$stim_meds_PRE <- as.factor(analysisDat3_long_acc$stim_meds_PRE)

analysisDat3_long_RT$avgRT_congruency <- as.factor(analysisDat3_long_RT$avgRT_congruency)
analysisDat3_long_RT$chgender <- as.factor(analysisDat3_long_RT$chgender)
analysisDat3_long_RT$stim_meds_PRE <- as.factor(analysisDat3_long_RT$stim_meds_PRE)


analysisDat3_long_PE_Acc$PE_PC <- as.factor(analysisDat3_long_PE_Acc$PE_PC)
analysisDat3_long_PE_Acc$chgender <- as.factor(analysisDat3_long_PE_Acc$chgender)
analysisDat3_long_PE_Acc$stim_meds_PRE <- as.factor(analysisDat3_long_PE_Acc$stim_meds_PRE)

analysisDat3_long_PE_corr_RT$PE_PC <- as.factor(analysisDat3_long_PE_corr_RT$PE_PC)
analysisDat3_long_PE_corr_RT$chgender <- as.factor(analysisDat3_long_PE_corr_RT$chgender)
analysisDat3_long_PE_corr_RT$stim_meds_PRE <- as.factor(analysisDat3_long_PE_corr_RT$stim_meds_PRE)


# Assign the sum contrast codes
# Even if the number in the column says -1, 1, the contrasts will automatically be set to 0,1. 
# So they must be defined
contrasts(analysisDat3_long$PE_PC) <- rev(contr.sum(2))
contrasts(analysisDat3_long$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long$stim_meds_PRE) <- rev(contr.sum(2))

contrasts(analysisDat3_long_acc$avgAcc_congruency) <- rev(contr.sum(2))
contrasts(analysisDat3_long_acc$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long_acc$stim_meds_PRE) <- rev(contr.sum(2))

contrasts(analysisDat3_long_RT$avgRT_congruency) <- rev(contr.sum(2))
contrasts(analysisDat3_long_RT$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long_RT$stim_meds_PRE) <- rev(contr.sum(2))

contrasts(analysisDat3_long_PE_Acc$PE_PC) <- rev(contr.sum(2))
contrasts(analysisDat3_long_PE_Acc$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long_PE_Acc$stim_meds_PRE) <- rev(contr.sum(2))

contrasts(analysisDat3_long_PE_corr_RT$PE_PC) <- rev(contr.sum(2))
contrasts(analysisDat3_long_PE_corr_RT$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long_PE_corr_RT$stim_meds_PRE) <- rev(contr.sum(2))


# Setting up to "reverse score" the sda/rd ratio
# This purely makes interpreting the graphs easier. the ratio represents attentional focus. previously it was 
# misleading/confusing because "more" attentional focus ends up being low on the y-axis (big ratio occurs due
# to big spotlight[sda] and small rate-of-spotlight-change[rd], which equals less attentional focus). Reversing
# it makes it so "more" attentional focus is high on the y-axis
analysisDat3_long$reverse_sda_rd_ratio_measurement <- analysisDat3_long$sda_rd_ratio_measurement
# multiplying this by -1 because we want to reverse score the ratio so the graphs make more sense
analysisDat3_long$reverse_sda_rd_ratio_measurement <- -1*scale(analysisDat3_long$sda_rd_ratio_measurement , center = TRUE, scale = TRUE)


#scale and center (z score) all continuous vars)
#putting a c() around it keeps it numeric instead of changing to matrix
analysisDat3_long$SCARED_SAD_Ch_PRE <- c(scale(analysisDat3_long$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat3_long$SCARED_SAD_Par_PRE <- c(scale(analysisDat3_long$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))
analysisDat3_long$age <- c(scale(analysisDat3_long$age , center = TRUE, scale = TRUE))
analysisDat3_long$a_measurement <- c(scale(analysisDat3_long$a_measurement , center = TRUE, scale = TRUE))
analysisDat3_long$p_measurement <- c(scale(analysisDat3_long$p_measurement , center = TRUE, scale = TRUE))
analysisDat3_long$ter_measurement <- c(scale(analysisDat3_long$ter_measurement , center = TRUE, scale = TRUE))
# We don't need this really since we are looking at the reverse score column, but good to have.
analysisDat3_long$sda_rd_ratio_measurement <- c(scale(analysisDat3_long$sda_rd_ratio_measurement , center = TRUE, scale = TRUE))
#ACS score/subscores child only
analysisDat3_long[,c(105,106,107)] <- lapply(analysisDat3_long[,c(105,106,107)], scale, center = TRUE, scale = TRUE)


# doing this again for the other dataframes. 
analysisDat3_long_acc$age <- c(scale(analysisDat3_long_acc$age , center = TRUE, scale = TRUE))
analysisDat3_long_acc$SCARED_SAD_Ch_PRE <- c(scale(analysisDat3_long_acc$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat3_long_acc$SCARED_SAD_Par_PRE <- c(scale(analysisDat3_long_acc$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))

analysisDat3_long_RT$age <- c(scale(analysisDat3_long_RT$age , center = TRUE, scale = TRUE))
analysisDat3_long_RT$SCARED_SAD_Ch_PRE <- c(scale(analysisDat3_long_RT$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat3_long_RT$SCARED_SAD_Par_PRE <- c(scale(analysisDat3_long_RT$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))

analysisDat3_long_PE_Acc$age <- c(scale(analysisDat3_long_PE_Acc$age , center = TRUE, scale = TRUE))
analysisDat3_long_PE_Acc$SCARED_SAD_Ch_PRE <- c(scale(analysisDat3_long_PE_Acc$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat3_long_PE_Acc$SCARED_SAD_Par_PRE <- c(scale(analysisDat3_long_PE_Acc$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))

analysisDat3_long_PE_corr_RT$age <- c(scale(analysisDat3_long_PE_corr_RT$age , center = TRUE, scale = TRUE))
analysisDat3_long_PE_corr_RT$SCARED_SAD_Ch_PRE <- c(scale(analysisDat3_long_PE_corr_RT$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat3_long_PE_corr_RT$SCARED_SAD_Par_PRE <- c(scale(analysisDat3_long_PE_corr_RT$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))


################# child-reported SAD:age:PE/PC- Mixed effects models ############


ratio_ch_SAD_age_fit<- lme(reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE*PE_PC*age +stim_meds_PRE 
                           + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)
summary(ratio_ch_SAD_age_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE * PE_PC *      age + stim_meds_PRE + chgender 
#                                    Value  Std.Error  DF   t-value p-value
# (Intercept)                  -0.02478890 0.10194330 118 -0.243164  0.8083
# SCARED_SAD_Ch_PRE            -0.11733275 0.06168269 118 -1.902199  0.0596
# PE_PC1                       -0.24365491 0.06263386  75 -3.890147  0.0002
# age                           0.08179452 0.06337615 118  1.290620  0.1994
# stim_meds_PRE1               -0.00292865 0.09988655 118 -0.029320  0.9767
# chgender1                     0.00008027 0.06338743 118  0.001266  0.9990
# SCARED_SAD_Ch_PRE:PE_PC1     -0.13663749 0.06173956  75 -2.213127  0.0299
# SCARED_SAD_Ch_PRE:age         0.15261994 0.06593922 118  2.314555  0.0224
# PE_PC1:age                    0.07170193 0.06327093  75  1.133252  0.2607
# SCARED_SAD_Ch_PRE:PE_PC1:age  0.14242751 0.06582711  75  2.163660  0.0337

# plotting
plot_model(ratio_ch_SAD_age_fit, type = "pred", terms = c("SCARED_SAD_Ch_PRE", "PE_PC", "age [-1,0,1]"), colors = c("dodgerblue3", "firebrick2"))
plot_model(ratio_ch_SAD_age_fit, type = "pred", terms = c("SCARED_SAD_Ch_PRE", "PE_PC", "age [-1,0,1]"), colors = "bw")


#simple slopes
library("reghelper")
simple_slopes(ratio_ch_SAD_age_fit, levels= list(PE_PC = c(1), SCARED_SAD_Ch_PRE= 'sstest', age= c(-1,0,1)))
#   SCARED_SAD_Ch_PRE PE_PC age Test Estimate Std. Error t value  df  Pr(>|t|) Sig.
# 1            sstest     1  -1       -0.5490     0.1402 -3.9159 118 0.0001513  ***
# 2            sstest     1   0       -0.2540     0.0945 -2.6871 118 0.0082493   **
# 3            sstest     1   1        0.0411     0.1365  0.3009 118 0.7640057     



a_ch_SAD_age_fit<- lme(a_measurement ~ SCARED_SAD_Ch_PRE*age*PE_PC +stim_meds_PRE 
                       + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)

summary(a_ch_SAD_age_fit)
# Fixed effects:  a_measurement ~ SCARED_SAD_Ch_PRE * PE_PC + age * PE_PC + SCARED_SAD_Ch_PRE *      age * PE_PC + stim_meds_PRE + chgender 
# Value  Std.Error  DF   t-value p-value
# (Intercept)                  -0.0228505 0.12604402 119 -0.181290  0.8564
# SCARED_SAD_Ch_PRE             0.1204664 0.07654644 119  1.573769  0.1182
# age                          -0.4164612 0.07761150 119 -5.365973  0.0000
# PE_PC1                        0.0281935 0.05098148  77  0.553014  0.5819
# stim_meds_PRE1               -0.2024744 0.12397203 119 -1.633226  0.1051
# chgender1                     0.1675089 0.07748909 119  2.161710  0.0326
# SCARED_SAD_Ch_PRE:age        -0.0160955 0.08148672 119 -0.197523  0.8438
# SCARED_SAD_Ch_PRE:PE_PC1     -0.0258953 0.05018622  77 -0.515985  0.6073
# age:PE_PC1                    0.0973228 0.05082154  77  1.914991  0.0592
# SCARED_SAD_Ch_PRE:age:PE_PC1 -0.0214942 0.05329745  77 -0.403288  0.6879

#plotting
plot_model(a_ch_SAD_age_fit, type = "pred", terms = c("PE_PC", "age [-1,0,1]"))



################ parent-reported SAD:age:PE/PC- Mixed effects models #############################

ratio_par_SAD_age_fit<- lme(reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Par_PRE*PE_PC + age*PE_PC + SCARED_SAD_Par_PRE*age*PE_PC +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)
summary(ratio_par_SAD_age_fit)
#                                     Value  Std.Error  DF   t-value p-value
# (Intercept)                   -0.01966989 0.12425624 116 -0.158301  0.8745
# SCARED_SAD_Par_PRE            -0.10298926 0.07574695 116 -1.359649  0.1766
# PE_PC1                        -0.27536618 0.07242203  76 -3.802243  0.0003
# age                            0.17384729 0.07574041 116  2.295304  0.0235
# stim_meds_PRE1                 0.03380460 0.11910720 116  0.283817  0.7771
# chgender1                      0.05761932 0.07760771 116  0.742443  0.4593
# SCARED_SAD_Par_PRE:PE_PC1     -0.08424613 0.07059185  76 -1.193426  0.2364
# PE_PC1:age                     0.15060682 0.07379696  76  2.040827  0.0447
# SCARED_SAD_Par_PRE:age         0.00014542 0.07272401 116  0.002000  0.9984
# SCARED_SAD_Par_PRE:PE_PC1:age  0.00785216 0.06999821  76  0.112177  0.9110

plot_model(ratio_par_SAD_age_fit, type = "pred", terms = c("PE_PC", "age [-1,0,1]"))

library(reghelper)
simple_slopes(ratio_par_SAD_age_fit, levels= list(PE_PC = c(1,-1),  age= "sstest", SCARED_SAD_Par_PRE = 0))




a_par_SAD_age_fit<- lme(a_measurement ~ SCARED_SAD_Par_PRE*PE_PC + age*PE_PC + SCARED_SAD_Par_PRE*age*PE_PC +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)
summary(a_par_SAD_age_fit)
#                                   Value  Std.Error  DF   t-value p-value
# (Intercept)                   -0.0440012 0.13156756 117 -0.334438  0.7386
# SCARED_SAD_Par_PRE             0.1460820 0.08065734 117  1.811143  0.0727
# PE_PC1                         0.0566414 0.05121301  78  1.105997  0.2721
# age                           -0.4775155 0.07975795 117 -5.987058  0.0000
# stim_meds_PRE1                -0.2335745 0.12707022 117 -1.838153  0.0686
# chgender1                      0.1360574 0.08186985 117  1.661874  0.0992
# SCARED_SAD_Par_PRE:PE_PC1      0.0624082 0.05014716  78  1.244501  0.2170
# PE_PC1:age                     0.0803078 0.05136583  78  1.563447  0.1220
# SCARED_SAD_Par_PRE:age        -0.0224821 0.07625238 117 -0.294838  0.7686
# SCARED_SAD_Par_PRE:PE_PC1:age -0.0302345 0.04850951  78 -0.623269  0.5349




################ ACS_total:SAD:age:PE/PC- Mixed effects models #############################

#subset the data to look at post-error trials only
analysisDat3_long_PEonly <- subset(analysisDat3_long, PE_PC == 1)

#FIFTEEN ITEM ACS w 20 trial cutoff 
ratio_ch_ACS_SAD_age_PEonly_fit<- lme(reverse_sda_rd_ratio_measurement ~  SCARED_SAD_Ch_PRE*age + ACS_total_Ch_PRE_15item*SCARED_SAD_Ch_PRE*age +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long_PEonly, na.action=na.omit)
summary(ratio_ch_ACS_SAD_age_PEonly_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE * age +      ACS_total_Ch_PRE_15item * SCARED_SAD_Ch_PRE * age + stim_meds_PRE +      chgender 
#                                                    Value Std.Error DF    t-value p-value
# (Intercept)                                   -0.0578015 0.2461987 70 -0.2347759  0.8151
# SCARED_SAD_Ch_PRE                             -0.0915727 0.1708587 70 -0.5359560  0.5937
# age                                           -0.0372515 0.1740338 70 -0.2140477  0.8311
# ACS_total_Ch_PRE_15item                       -0.4505331 0.1941676 70 -2.3203303  0.0232
# stim_meds_PRE1                                 0.0554210 0.2342522 70  0.2365867  0.8137
# chgender1                                      0.0113022 0.1667797 70  0.0677673  0.9462
# SCARED_SAD_Ch_PRE:age                          0.1861666 0.1752636 70  1.0622092  0.2918
# SCARED_SAD_Ch_PRE:ACS_total_Ch_PRE_15item     -0.3715942 0.1803202 70 -2.0607466  0.0430
# age:ACS_total_Ch_PRE_15item                    0.1791721 0.1793185 70  0.9991835  0.3211
# SCARED_SAD_Ch_PRE:age:ACS_total_Ch_PRE_15item  0.3922052 0.1913644 70  2.0495198  0.0442



plot_model(ratio_ch_ACS_SAD_age_PEonly_fit, type = "pred", terms = c("SCARED_SAD_Ch_PRE", "age [-1,1]", "ACS_total_Ch_PRE_15item [-1,1]"))


simple_slopes(ratio_ch_ACS_SAD_age_PEonly_fit, levels= list(ACS_total_Ch_PRE_15item = c(-1,0,1), SCARED_SAD_Ch_PRE= 'sstest', age= c(-1,0,1)))


#subset the data to look at PC trials only
analysisDat3_long_PConly <- subset(analysisDat3_long, PE_PC == -1)

ratio_ch_ACS_SAD_age_PConly_fit<- lme(reverse_sda_rd_ratio_measurement ~  SCARED_SAD_Ch_PRE*age + ACS_total_Ch_PRE_15item*SCARED_SAD_Ch_PRE*age +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long_PConly, na.action=na.omit)
summary(ratio_ch_ACS_SAD_age_PConly_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE * age +      ACS_total_Ch_PRE_15item * SCARED_SAD_Ch_PRE * age + stim_meds_PRE +      chgender 
#                                                     Value  Std.Error  DF   t-value p-value
# (Intercept)                                    0.20023073 0.02469269 109  8.108906  0.0000
# SCARED_SAD_Ch_PRE                              0.01449950 0.01570271 109  0.923376  0.3579
# age                                            0.01540659 0.01495707 109  1.030054  0.3053
# ACS_total_Ch_PRE_15item                        0.00070975 0.01680739 109  0.042229  0.9664
# stim_meds_PRE1                                -0.03852858 0.02460289 109 -1.566018  0.1202
# chgender1                                      0.01796580 0.01458613 109  1.231704  0.2207
# SCARED_SAD_Ch_PRE:age                          0.00772875 0.01545981 109  0.499926  0.6181
# SCARED_SAD_Ch_PRE:ACS_total_Ch_PRE_15item     -0.01556006 0.01568332 109 -0.992140  0.3233
# age:ACS_total_Ch_PRE_15item                    0.00215569 0.01689428 109  0.127599  0.8987
# SCARED_SAD_Ch_PRE:age:ACS_total_Ch_PRE_15item -0.00619514 0.01817577 109 -0.340846  0.7339



############ behav t-test Acc RT ########

# ttest of accuracy when congruency = -1 vs acc when cong. = 1
t.test(analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency==-1], analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency==1], paired=TRUE)
# data:  analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency == -1] and analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency == 1]
# t = -14.017, df = 145, p-value < 2.2e-16
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.11859756 -0.08928562
# sample estimates:
#   mean difference 
# -0.1039416 
mean(analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency==-1], na.rm=TRUE)
mean(analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency==1], na.rm=TRUE)

#correct congruent vs. correct incongruent (NOT post-error/correct)
t.test(analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency==1], analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency==-1], paired = TRUE)
# data:  analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency == 1] and analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency == -1]
# t = -17.308, df = 145, p-value < 2.2e-16
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -66.81598 -53.12025
# sample estimates:
#   mean difference 
# -59.96812 
mean(analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency==1],na.rm=TRUE)
mean(analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency==-1],na.rm=TRUE)

###### SCARED SAD histograms, skewness, reliability #######

# histograms for R1
SCARED_SAD_histogram<-ggplot(analysisDat3, aes(x=SCARED_SAD_Ch_PRE)) + 
  geom_histogram(color="black", fill="darkgray") +
  scale_x_continuous(breaks = seq(0,15, by = 2)) +
  ylim(0, 15)

SCARED_SAD_Par_histogram<-ggplot(analysisDat3, aes(x=SCARED_SAD_Par_PRE)) + 
  geom_histogram(color="black", fill="darkgray") +
  scale_x_continuous(breaks = seq(0,20, by = 2)) +
  ylim(0, 15)

library(moments) # for skewness function
skewness(analysisDat3$SCARED_SAD_Ch_PRE, na.rm=TRUE)
# 0.03647715
skewness(analysisDat3$SCARED_SAD_Par_PRE, na.rm=TRUE) 
# 0.003988732 

library(psych) #reliability
# reliability of child-reported SCARED SAD
# items 3, 10, 26, 32, 39, 40, 41
alpha(collectiveDat[,c(5,12,28,34,41,42,43)])
# reliability of parent-reported SCARED SAD
# items 3, 10, 26, 32, 39, 40, 41
alpha(collectiveDat[,c(52,59,75,81,88,89,90)])


##### Were there associations between RT/Acc and social anxiety scores? ####

# For all these analyses, I did the sheet setup up above in the "converting wide data to long" 
# and "Prep for mixed effects models" 

# Post-trial accuracy w child-report SCARED
Acc_PEPC_SAD_age_fit<- lme(PE_PC_Acc_value ~ SCARED_SAD_Ch_PRE*PE_PC*age +stim_meds_PRE 
                           + chgender, random = ~1 | subject, data = analysisDat3_long_PE_Acc, na.action=na.omit)
summary(Acc_PEPC_SAD_age_fit)
#                                   Value   Std.Error  DF  t-value p-value
# (Intercept)                   0.8223634 0.011949860 120 68.81782  0.0000
# SCARED_SAD_Ch_PRE            -0.0000065 0.007299981 120 -0.00089  0.9993
# PE_PC1                       -0.0449980 0.006351117  79 -7.08506  0.0000
# age                           0.0147648 0.007465005 120  1.97787  0.0502
# stim_meds_PRE1               -0.0177026 0.011705440 120 -1.51234  0.1331
# chgender1                     0.0011666 0.007417623 120  0.15728  0.8753
# SCARED_SAD_Ch_PRE:PE_PC1     -0.0090058 0.006318035  79 -1.42541  0.1580
# SCARED_SAD_Ch_PRE:age         0.0093050 0.007794273 120  1.19383  0.2349
# PE_PC1:age                    0.0128417 0.006448930  79  1.99130  0.0499
# SCARED_SAD_Ch_PRE:PE_PC1:age  0.0034831 0.006713680  79  0.51881  0.6053

# Post-trial accuracy w parent-report SCARED
Acc_PEPC_SAD_par_age_fit<- lme(PE_PC_Acc_value ~ SCARED_SAD_Par_PRE*PE_PC*age +stim_meds_PRE 
                               + chgender, random = ~1 | subject, data = analysisDat3_long_PE_Acc, na.action=na.omit)
summary(Acc_PEPC_SAD_par_age_fit)
#                                    Value   Std.Error  DF  t-value p-value
# (Intercept)                    0.8162366 0.012140486 118 67.23261  0.0000
# SCARED_SAD_Par_PRE             0.0025482 0.007462018 118  0.34148  0.7333
# PE_PC1                        -0.0449276 0.006484744  79 -6.92820  0.0000
# age                            0.0147953 0.007418523 118  1.99437  0.0484
# stim_meds_PRE1                -0.0225221 0.011654098 118 -1.93254  0.0557
# chgender1                     -0.0011172 0.007631189 118 -0.14640  0.8839
# SCARED_SAD_Par_PRE:PE_PC1      0.0009149 0.006345912  79  0.14417  0.8857
# SCARED_SAD_Par_PRE:age         0.0120141 0.007124997 118  1.68619  0.0944
# PE_PC1:age                     0.0132002 0.006607887  79  1.99764  0.0492
# SCARED_SAD_Par_PRE:PE_PC1:age  0.0052957 0.006223172  79  0.85096  0.3974

# Post-trial RT w child-report SCARED
RT_PEPC_SAD_age_fit<- lme(PE_PC_corr_RT_value ~ SCARED_SAD_Ch_PRE*PE_PC*age +stim_meds_PRE 
                          + chgender, random = ~1 | subject, data = analysisDat3_long_PE_corr_RT, na.action=na.omit)
summary(RT_PEPC_SAD_age_fit)
#                                 Value Std.Error  DF  t-value p-value
# (Intercept)                  600.0483 16.729967 120 35.86668  0.0000
# SCARED_SAD_Ch_PRE             12.8766  9.989629 120  1.28900  0.1999
# PE_PC1                        35.0370  4.629663  81  7.56793  0.0000
# age                          -81.3588 10.199832 120 -7.97649  0.0000
# stim_meds_PRE1               -19.0083 16.508858 120 -1.15140  0.2519
# chgender1                     29.8553 10.169302 120  2.93582  0.0040
# SCARED_SAD_Ch_PRE:PE_PC1      -1.0955  4.564291  81 -0.24001  0.8109
# SCARED_SAD_Ch_PRE:age         -8.6938 10.698109 120 -0.81265  0.4180
# PE_PC1:age                     6.4482  4.645205  81  1.38814  0.1689
# SCARED_SAD_Ch_PRE:PE_PC1:age  -1.1127  4.874433  81 -0.22828  0.8200

# Post-trial RT w parent-report SCARED
RT_PEPC_SAD_par_age_fit<- lme(PE_PC_corr_RT_value ~ SCARED_SAD_Par_PRE*PE_PC*age +stim_meds_PRE 
                              + chgender, random = ~1 | subject, data = analysisDat3_long_PE_corr_RT, na.action=na.omit)
summary(RT_PEPC_SAD_par_age_fit)
#                                  Value Std.Error  DF  t-value p-value
# (Intercept)                   598.4391 17.272405 118 34.64712  0.0000
# SCARED_SAD_Par_PRE             17.0583 10.457124 118  1.63126  0.1055
# PE_PC1                         35.4534  4.645421  81  7.63191  0.0000
# age                           -85.4499 10.405034 118 -8.21236  0.0000
# stim_meds_PRE1                -21.7091 16.744196 118 -1.29652  0.1973
# chgender1                      25.9228 10.635571 118  2.43737  0.0163
# SCARED_SAD_Par_PRE:PE_PC1       1.0652  4.484119  81  0.23755  0.8128
# SCARED_SAD_Par_PRE:age         -6.5502  9.946280 118 -0.65856  0.5115
# PE_PC1:age                      7.4539  4.706160  81  1.58386  0.1171
# SCARED_SAD_Par_PRE:PE_PC1:age   0.3632  4.373493  81  0.08305  0.9340



#### Binary SAD diagnosis analyses ########

subList <- unique(analysisDat3$subject)

analysisDat4 <- analysisDat3

analysisDat4$binary_SAD <-NA

for (i in 1:length(subList)){
  
  #if they have a diagnosis (first diagnosis is not NA) then...  (this filters out the NAs so the else works)
  if(!is.na(analysisDat4$finaldx1_PRE[analysisDat4$subject==subList[i]])){
    # if any of the finaldxs have a 2 (social phobia) and are not NA, then they get marked 1. Else, 0
    if ((!is.na(analysisDat4$finaldx1_PRE[analysisDat4$subject==subList[i]]) && analysisDat4$finaldx1_PRE[analysisDat4$subject==subList[i]]==2) | 
        (!is.na(analysisDat4$finaldx2_PRE[analysisDat4$subject==subList[i]]) && analysisDat4$finaldx2_PRE[analysisDat4$subject==subList[i]]==2) |
        (!is.na(analysisDat4$finaldx3_PRE[analysisDat4$subject==subList[i]]) && analysisDat4$finaldx3_PRE[analysisDat4$subject==subList[i]]==2) | 
        (!is.na(analysisDat4$finaldx4_PRE[analysisDat4$subject==subList[i]]) && analysisDat4$finaldx4_PRE[analysisDat4$subject==subList[i]]==2) |
        (!is.na(analysisDat4$finaldx5_PRE[analysisDat4$subject==subList[i]]) && analysisDat4$finaldx5_PRE[analysisDat4$subject==subList[i]]==2) | 
        (!is.na(analysisDat4$finaldx6_PRE[analysisDat4$subject==subList[i]]) && analysisDat4$finaldx6_PRE[analysisDat4$subject==subList[i]]==2) ){
      analysisDat4$binary_SAD[analysisDat4$subject==subList[i]] <- 1
    }else{
      analysisDat4$binary_SAD[analysisDat4$subject==subList[i]] <- 0
    }
  }
  
}

# Just checking that it worked- kept NAs as NAs, 1s and 0s correctly marked
#test <- data.frame(analysisDat4$subject, analysisDat4[,c("finaldx1_PRE","finaldx2_PRE","finaldx3_PRE","finaldx4_PRE","finaldx5_PRE", "finaldx6_PRE", "binary_SAD")])

analysisDat4_long <- reshape(analysisDat4, idvar="subject", direction="long", 
                             varying=list(a=c(2,3), ter=c(5,6), p=c(8,9), rd=c(11,12), sda=c(14,15), sda_rd_ratio=c(17,18)),
                             # -1 is post-correct and 1 is post-error
                             times=c('1', '-1'),
                             v.names = c("a_measurement", "ter_measurement", "p_measurement", "rd_measurement", "sda_measurement", "sda_rd_ratio_measurement"))

#renaming the column called "time" to "PE_PC"
colnames(analysisDat4_long)[which(names(analysisDat4_long) == "time")] <- "PE_PC"


# Factorize the categorical variables
analysisDat4_long$PE_PC <- as.factor(analysisDat4_long$PE_PC)
analysisDat4_long$binary_SAD <- as.factor(analysisDat4_long$binary_SAD)
analysisDat4_long$chgender <- as.factor(analysisDat4_long$chgender)
analysisDat4_long$stim_meds_PRE <- as.factor(analysisDat4_long$stim_meds_PRE)

# Assign the sum contrast codes
contrasts(analysisDat4_long$PE_PC) <-rev(contr.sum(2))
contrasts(analysisDat4_long$binary_SAD) <-rev(contr.sum(2))
contrasts(analysisDat4_long$chgender) <- rev(contr.sum(2))
contrasts(analysisDat4_long$stim_meds_PRE) <- rev(contr.sum(2))

# Setting up to "reverse score" the sda/rd ratio
analysisDat4_long$reverse_sda_rd_ratio_measurement <- analysisDat4_long$sda_rd_ratio_measurement

#scale and center (z score) all continuous vars)
#putting a c() around it keeps it numeric instead of changing to matrix
analysisDat4_long$SCARED_SAD_Ch_PRE <- c(scale(analysisDat4_long$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat4_long$SCARED_SAD_Par_PRE <- c(scale(analysisDat4_long$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))
analysisDat4_long$age <- c(scale(analysisDat4_long$age , center = TRUE, scale = TRUE))
analysisDat4_long$a_measurement <- c(scale(analysisDat4_long$a_measurement , center = TRUE, scale = TRUE))
analysisDat4_long$p_measurement <- c(scale(analysisDat4_long$p_measurement , center = TRUE, scale = TRUE))
analysisDat4_long$ter_measurement <- c(scale(analysisDat4_long$ter_measurement , center = TRUE, scale = TRUE))
analysisDat4_long$sda_rd_ratio_measurement <- c(scale(analysisDat4_long$sda_rd_ratio_measurement , center = TRUE, scale = TRUE))

# multiplying this by -1 because we want to reverse score the ratio so the graphs make more sense (see above 
# when the column is added). Reversing it before the scaling had no effect.
analysisDat4_long$reverse_sda_rd_ratio_measurement <- -1*scale(analysisDat4_long$sda_rd_ratio_measurement , center = TRUE, scale = TRUE)


# sda/rd ratio binary_SAD and age
ratio_BSA_age_fit<- lme(reverse_sda_rd_ratio_measurement ~ binary_SAD*PE_PC*age +stim_meds_PRE 
                        + chgender, random = ~1 | subject, data = analysisDat4_long, na.action=na.omit)
summary(ratio_BSA_age_fit)
#                              Value  Std.Error  DF   t-value p-value
# (Intercept)            -0.01575251 0.12080976 118 -0.130391  0.8965
# binary_SAD1            -0.15534567 0.07545201 118 -2.058867  0.0417
# PE_PC1                 -0.23689283 0.07264250  77 -3.261077  0.0017
# age                     0.19654665 0.08801992 118  2.232979  0.0274
# stim_meds_PRE1         -0.00603759 0.11507532 118 -0.052466  0.9582
# chgender1               0.04493413 0.07399942 118  0.607223  0.5449
# binary_SAD1:PE_PC1     -0.13912538 0.07257651  77 -1.916948  0.0590
# binary_SAD1:age        -0.03000950 0.08859744 118 -0.338717  0.7354
# PE_PC1:age              0.17380449 0.08571584  77  2.027682  0.0461
# binary_SAD1:PE_PC1:age -0.03281902 0.08568728  77 -0.383009  0.7028

# a (boundary sep) binary_SAD and age
a_BSA_age_fit<- lme(a_measurement ~ binary_SAD*PE_PC*age +stim_meds_PRE 
                    + chgender, random = ~1 | subject, data = analysisDat4_long, na.action=na.omit)
summary(a_BSA_age_fit)
#                             Value  Std.Error  DF   t-value p-value
# (Intercept)            -0.0542062 0.13023668 119 -0.416213  0.6780
# binary_SAD1             0.0243692 0.08166376 119  0.298409  0.7659
# PE_PC1                  0.0459900 0.05228841  79  0.879545  0.3818
# age                    -0.4636305 0.09237459 119 -5.019027  0.0000
# stim_meds_PRE1         -0.2132228 0.12385697 119 -1.721525  0.0878
# chgender1               0.1734879 0.07899992 119  2.196051  0.0300
# binary_SAD1:PE_PC1      0.0710596 0.05227233  79  1.359411  0.1779
# binary_SAD1:age         0.0649944 0.09295746 119  0.699184  0.4858
# PE_PC1:age              0.0882386 0.06280389  79  1.404987  0.1639
# binary_SAD1:PE_PC1:age -0.0450913 0.06281200  79 -0.717877  0.4750


##### full vs final vs excluded sample ######

# Make a new column where full vs final sample is marked
# Needed to easily examine the excluded population (n=66)
finalsubList <- unique(analysisDat3$subject)
collectiveDat$finalsample <- 0
collectiveDat$finalsample[collectiveDat$subject %in% finalsubList] <- 1


## mean & sd for child SCARED-Social
# with full 214
round(mean(collectiveDat$SCARED_SAD_Ch_PRE, na.rm = TRUE),2) # 7.00
round(sd(collectiveDat$SCARED_SAD_Ch_PRE, na.rm = TRUE),2) # 4.24
# with 148
round(mean(collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$finalsample==1], na.rm = TRUE),2) # 7.03
round(sd(analysisDat3$SCARED_SAD_Ch_PRE[collectiveDat$finalsample==1], na.rm = TRUE),2) # 4.36
# with 66
round(mean(collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$finalsample==0], na.rm = TRUE),2) # 6.95
round(sd(collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$finalsample==0], na.rm = TRUE),2) # 4.09
#ttest comparing final sample to excluded sample 
t.test(collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$finalsample==1], collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$finalsample==0])
# t = 0.12069, df = 121.58, p-value = 0.9041
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.176103  1.328821
# sample estimates:
#   mean of x mean of y 
# 7.027972  6.951613 


## mean & sd for parent SCARED-Social
# with full 214
round(mean(collectiveDat$SCARED_SAD_Par_PRE, na.rm = TRUE),2) # 7.22
round(sd(collectiveDat$SCARED_SAD_Par_PRE, na.rm = TRUE),2) # 4.31
# with 148
round(mean(collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$finalsample==1], na.rm = TRUE),2) # 7.15
round(sd(collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$finalsample==1], na.rm = TRUE),2) # 4.42
# with 66
round(mean(collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$finalsample==0], na.rm = TRUE),2) # 7.40
round(sd(collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$finalsample==0], na.rm = TRUE),2) # 4.07
#ttest comparing final sample to excluded sample
t.test(collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$finalsample==1], collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$finalsample==0])
# t = -0.3831, df = 115.91, p-value = 0.7024
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.546021  1.044888
# sample estimates:
#   mean of x mean of y 
# 7.145985  7.396552 

## mean & sd for age
# with full 214
round(mean(collectiveDat$age, na.rm = TRUE),2) # 10.28
round(sd(collectiveDat$age, na.rm = TRUE),2) # 2.67
# with final 148
round(mean(collectiveDat$age[collectiveDat$finalsample==1], na.rm = TRUE),2) # 10.80
round(sd(collectiveDat$age[collectiveDat$finalsample==1], na.rm = TRUE),2) # 2.66
# with excluded 66
round(mean(collectiveDat$age[collectiveDat$finalsample==0], na.rm = TRUE),2) # 9.11
round(sd(collectiveDat$age[collectiveDat$finalsample==0], na.rm = TRUE),2) # 2.33
#ttest for final vs excluded samples
t.test(collectiveDat$age[collectiveDat$finalsample==1], collectiveDat$age[collectiveDat$finalsample==0])
# t = 4.6923, df = 141.5, p-value = 6.308e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.9787111 2.4037623
# sample estimates:
#   mean of x mean of y 
# 10.797297  9.106061 

## mean & sd for gender
# with full 214
table(collectiveDat$chgender)
table(is.na(collectiveDat$chgender))
#  1   2  NA
# 118  90  6
# with 148
table(collectiveDat$chgender[collectiveDat$finalsample==1])
table(is.na(collectiveDat$chgender[collectiveDat$finalsample==1]))
# 1  2  NA
# 85 59  4
table(collectiveDat$chgender[collectiveDat$finalsample==0])
table(is.na(collectiveDat$chgender[collectiveDat$finalsample==0]))
# 1  2  NA
# 33 31  2
t.test(collectiveDat$chgender[collectiveDat$finalsample==1], collectiveDat$chgender[collectiveDat$finalsample==0])
# t = -0.99267, df = 118.7, p-value = 0.3229
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.22356799  0.07426244
# sample estimates:
#   mean of x mean of y 
# 1.409722  1.484375 


## mean & sd for current meds
# How many are on psychoactive medication currently?
# with full 214
table(collectiveDat$psychoactive_meds_PRE)
table(is.na(collectiveDat$psychoactive_meds_PRE))
#  0   1   NA
# 153  32  29

# with 148
table(collectiveDat$psychoactive_meds_PRE[collectiveDat$finalsample==1])
table(is.na(collectiveDat$psychoactive_meds_PRE[collectiveDat$finalsample==1]))
# 0   1    NA
# 107  25  16

# with 66
table(collectiveDat$psychoactive_meds_PRE[collectiveDat$finalsample==0])
table(is.na(collectiveDat$psychoactive_meds_PRE[collectiveDat$finalsample==0]))
# 0   1   NA
# 46  7   13

t.test(collectiveDat$psychoactive_meds_PRE[collectiveDat$finalsample==1], collectiveDat$psychoactive_meds_PRE[collectiveDat$finalsample==0])
# t = 0.98643, df = 109.68, p-value = 0.3261
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05783914  0.17247607
# sample estimates:
#   mean of x mean of y 
# 0.1893939 0.1320755 



#### diagnosis for full sample- Table S1 #####

# How many in full sample have a Social anxiety diagnosis?
table(c(collectiveDat$finaldx1_PRE==2 | collectiveDat$finaldx2_PRE==2 | collectiveDat$finaldx3_PRE==2 | 
          collectiveDat$finaldx4_PRE==2| collectiveDat$finaldx5_PRE==2| collectiveDat$finaldx6_PRE==2))
# FALSE  TRUE 
#     5   109 
table(is.na(collectiveDat$finaldx1_PRE))
# FALSE  TRUE 
# 202    11 
# 109 have a social anxiety diagnosis


# How many in full sample have a PRIMARY Social anxiety diagnosis?
# table(collectiveDat$finaldx1_PRE==2)
# FALSE  TRUE 
# 147    56 


# How many in full sample have a social anxiety dx AND at least one other dx?
# We know there's 109 across all 6 finaldx columns. If they have a 2 in any column other than
# the first column, then those have an additional diagnosis. If we look at how many have a 2
# in finaldx1 but nothing in other columns, then that is the number that have ONLY SA dx.
table(c(collectiveDat$finaldx1_PRE==2 & (collectiveDat$finaldx2_PRE==24 | is.na(collectiveDat$finaldx2_PRE))))
# FALSE  TRUE 
# 189    14 
# 14 have a SA dx but no other dxs
# 95 have at least one other dx in addition to SA dx (109-14)


# How many have a separation anxiety disorder dx?
# 1, Separation Anxiety Disorder
table(c(collectiveDat$finaldx1_PRE==1 | collectiveDat$finaldx2_PRE==1 | collectiveDat$finaldx3_PRE==1 | 
          collectiveDat$finaldx4_PRE==1| collectiveDat$finaldx5_PRE==1| collectiveDat$finaldx6_PRE==1))
# FALSE  TRUE 
#     6    67


# How many have a generalized anxiety disorder dx?
# 7, Generalized Anxiety Disorder
table(c(collectiveDat$finaldx1_PRE==7 | collectiveDat$finaldx2_PRE==7 | collectiveDat$finaldx3_PRE==7 | 
          collectiveDat$finaldx4_PRE==7| collectiveDat$finaldx5_PRE==7| collectiveDat$finaldx6_PRE==7))
# FALSE  TRUE 
#     2   123 


# How many have a specific phobia dx?
# 3, Specific Phobia
table(collectiveDat$finaldx1_PRE==3 | collectiveDat$finaldx2_PRE==3 | collectiveDat$finaldx3_PRE==3 | 
        collectiveDat$finaldx4_PRE==3| collectiveDat$finaldx5_PRE==3| collectiveDat$finaldx6_PRE==3)
# FALSE  TRUE 
# 7    72 


# How many have an other anxiety disorder dx?
# 4, Panic Disorder 
# 5, Panic Disorder with Agorophobia
# 6, Agorophobia without Panic 
# 8, Overanxious Disorder
# 18, Selective Mutism 
table(c(collectiveDat$finaldx1_PRE==4 | collectiveDat$finaldx2_PRE==4 | collectiveDat$finaldx3_PRE==4 | 
          collectiveDat$finaldx4_PRE==4| collectiveDat$finaldx5_PRE==4| collectiveDat$finaldx6_PRE==4) |
        c(collectiveDat$finaldx1_PRE==5 | collectiveDat$finaldx2_PRE==5 | collectiveDat$finaldx3_PRE==5 | 
            collectiveDat$finaldx4_PRE==5| collectiveDat$finaldx5_PRE==5| collectiveDat$finaldx6_PRE==5)|
        c(collectiveDat$finaldx1_PRE==6 | collectiveDat$finaldx2_PRE==6 | collectiveDat$finaldx3_PRE==6 | 
            collectiveDat$finaldx4_PRE==6| collectiveDat$finaldx5_PRE==6| collectiveDat$finaldx6_PRE==6) |
        c(collectiveDat$finaldx1_PRE==8 | collectiveDat$finaldx2_PRE==8 | collectiveDat$finaldx3_PRE==8 | 
            collectiveDat$finaldx4_PRE==8| collectiveDat$finaldx5_PRE==8| collectiveDat$finaldx6_PRE==8) |
        c(collectiveDat$finaldx1_PRE==18 | collectiveDat$finaldx2_PRE==18 | collectiveDat$finaldx3_PRE==18 | 
            collectiveDat$finaldx4_PRE==18| collectiveDat$finaldx5_PRE==18| collectiveDat$finaldx6_PRE==18))
# FALSE  TRUE 
# 11    13 


# How many have an ADHD dx?
# 13, Attention Deficit Hyperactivity Disorder (Innatentive Type)
# 14, Attention Deficit Hyperactivity Disorder (Hyperactive type)
# 15, Attention Deficit Hyperactivity Disorder (Combined Type)
table(c(collectiveDat$finaldx1_PRE==13 | collectiveDat$finaldx2_PRE==13 | collectiveDat$finaldx3_PRE==13 | 
          collectiveDat$finaldx4_PRE==13| collectiveDat$finaldx5_PRE==13| collectiveDat$finaldx6_PRE==13) |
        c(collectiveDat$finaldx1_PRE==14 | collectiveDat$finaldx2_PRE==14 | collectiveDat$finaldx3_PRE==14 | 
            collectiveDat$finaldx4_PRE==14| collectiveDat$finaldx5_PRE==14| collectiveDat$finaldx6_PRE==14)|
        c(collectiveDat$finaldx1_PRE==15 | collectiveDat$finaldx2_PRE==15 | collectiveDat$finaldx3_PRE==15 | 
            collectiveDat$finaldx4_PRE==15| collectiveDat$finaldx5_PRE==15| collectiveDat$finaldx6_PRE==15))
# FALSE  TRUE 
#   6    51 

# How many have a Mood disorder dx?
# 11, Dysthymia
# 12, Major Depression 
table(c(collectiveDat$finaldx1_PRE==11 | collectiveDat$finaldx2_PRE==11 | collectiveDat$finaldx3_PRE==11 | 
          collectiveDat$finaldx4_PRE==11| collectiveDat$finaldx5_PRE==11| collectiveDat$finaldx6_PRE==11) |
        c(collectiveDat$finaldx1_PRE==12 | collectiveDat$finaldx2_PRE==12 | collectiveDat$finaldx3_PRE==12 | 
            collectiveDat$finaldx4_PRE==12| collectiveDat$finaldx5_PRE==12| collectiveDat$finaldx6_PRE==12))
# FALSE  TRUE 
# 10    13 

# How many have an other non-anxiety dx?
# 9, Obsessive Compulsive Disorder 
# 10, Posttraumatic Stress Disorder
# 16, Conduct Disorder 
# 17, Oppositional Defiant Disorder
# 19, Enuresis
# 20, Sleep Terror Disorders
# 21, Substance Abuse
# 22, Schizophrenia
# 23, Trichollomania
# 26, Tourrette’s Syndrome
table(c(collectiveDat$finaldx1_PRE==9 | collectiveDat$finaldx2_PRE==9 | collectiveDat$finaldx3_PRE==9 | 
          collectiveDat$finaldx4_PRE==9| collectiveDat$finaldx5_PRE==9| collectiveDat$finaldx6_PRE==9) |
        c(collectiveDat$finaldx1_PRE==10 | collectiveDat$finaldx2_PRE==10 | collectiveDat$finaldx3_PRE==10 | 
            collectiveDat$finaldx4_PRE==10| collectiveDat$finaldx5_PRE==10| collectiveDat$finaldx6_PRE==10)|
        c(collectiveDat$finaldx1_PRE==16 | collectiveDat$finaldx2_PRE==16 | collectiveDat$finaldx3_PRE==16 | 
            collectiveDat$finaldx4_PRE==16| collectiveDat$finaldx5_PRE==16| collectiveDat$finaldx6_PRE==16) |
        c(collectiveDat$finaldx1_PRE==17 | collectiveDat$finaldx2_PRE==17 | collectiveDat$finaldx3_PRE==17 | 
            collectiveDat$finaldx4_PRE==17| collectiveDat$finaldx5_PRE==17| collectiveDat$finaldx6_PRE==17) |
        c(collectiveDat$finaldx1_PRE==19 | collectiveDat$finaldx2_PRE==19 | collectiveDat$finaldx3_PRE==19 | 
            collectiveDat$finaldx4_PRE==19| collectiveDat$finaldx5_PRE==19| collectiveDat$finaldx6_PRE==19)|
        c(collectiveDat$finaldx1_PRE==20 | collectiveDat$finaldx2_PRE==20 | collectiveDat$finaldx3_PRE==20 | 
            collectiveDat$finaldx4_PRE==20| collectiveDat$finaldx5_PRE==20| collectiveDat$finaldx6_PRE==20) |
        c(collectiveDat$finaldx1_PRE==21 | collectiveDat$finaldx2_PRE==21 | collectiveDat$finaldx3_PRE==21 | 
            collectiveDat$finaldx4_PRE==21| collectiveDat$finaldx5_PRE==21| collectiveDat$finaldx6_PRE==21) |
        c(collectiveDat$finaldx1_PRE==22 | collectiveDat$finaldx2_PRE==22 | collectiveDat$finaldx3_PRE==22 | 
            collectiveDat$finaldx4_PRE==22| collectiveDat$finaldx5_PRE==22| collectiveDat$finaldx6_PRE==22) |
        c(collectiveDat$finaldx1_PRE==23 | collectiveDat$finaldx2_PRE==23 | collectiveDat$finaldx3_PRE==23 | 
            collectiveDat$finaldx4_PRE==23| collectiveDat$finaldx5_PRE==23| collectiveDat$finaldx6_PRE==23) |
        c(collectiveDat$finaldx1_PRE==26 | collectiveDat$finaldx2_PRE==26 | collectiveDat$finaldx3_PRE==26 | 
            collectiveDat$finaldx4_PRE==26| collectiveDat$finaldx5_PRE==26| collectiveDat$finaldx6_PRE==26))
# FALSE  TRUE 
# 10    20 


# How many have no dx?
table(collectiveDat$finaldx1_PRE==24)
# FALSE  TRUE 
# 195     7 


# how many don't have dx info?
table(is.na(collectiveDat$finaldx1_PRE))
# FALSE  TRUE 
# 203    11 


##### Medication table #####

# We only want to consider meds that have a 1 in childmedpre AND have not been stopped
# Manually classified
# 1, Stimulant
# 2, non-stimulant ADHD med
# 3, antidepressants
# 4, anti-anxiety
# 5, anti-psychotic
# 6, mood stabilizer
# 7, other psychoactive
# 8, non-psychoactive med

# how many on stimulant?
table(c(collectiveDat$med1_type==1 | collectiveDat$med2_type==1 | collectiveDat$med3_type==1))
# FALSE  TRUE  
#   2    20     

# how many on non-stim ADHD?
table(c(collectiveDat$med1_type==2 | collectiveDat$med2_type==2 | collectiveDat$med3_type==2))
# FALSE  TRUE 
# 2       2 

# How many on 3, antidepressants?
table(c(collectiveDat$med1_type==3 | collectiveDat$med2_type==3 | collectiveDat$med3_type==3))
# FALSE  TRUE 
# 2    13 

# how many on 4, anti-anxiety?
table(c(collectiveDat$med1_type==4 | collectiveDat$med2_type==4 | collectiveDat$med3_type==4))
# FALSE  TRUE 
#   3     1 

# how many on 5, anti-psychotic
table(c(collectiveDat$med1_type==5 | collectiveDat$med2_type==5 | collectiveDat$med3_type==5))
# 0

# how many on 6, mood stabilizer
table(c(collectiveDat$med1_type==6 | collectiveDat$med2_type==6 | collectiveDat$med3_type==6))
# 0

# how many on 7, other psychoactive
table(c(collectiveDat$med1_type==7 | collectiveDat$med2_type==7 | collectiveDat$med3_type==7))
# FALSE  TRUE 
#   2     6 

# how many on 8, non-psychoactive med
table(c(collectiveDat$med1_type==8 | collectiveDat$med2_type==8 | collectiveDat$med3_type==8))
# FALSE  TRUE 
# 1       11 

# how many not on medication (childmedpre=0)
table(collectiveDat$childmedpre==0)
# FALSE  TRUE 
# 43     142 

# how many missing med info (childmedpre=NA)
table(is.na(collectiveDat$childmedpre))
# FALSE  TRUE 
# 185    29 


# how many started psychoactive meds in the last four weeks? med1_length = legnth of time taking med1 (1== less than 4 wks)
# where med1_length == 1 when med1_type is not NA and med1_type doesn't equal 8 (non psycho med)
table(c((collectiveDat$med1_length==1 & collectiveDat$med1_type!=8 & collectiveDat$childmedpre==1) |
          c(collectiveDat$med2_length==1 & collectiveDat$med2_type!=8 & collectiveDat$childmedpre==1) |
          c(collectiveDat$med3_length==1 & collectiveDat$med3_type!=8 & collectiveDat$childmedpre==1)))
# FALSE  TRUE 
# 145     2  (Manually checked- both stimulants)
# how many changed dosage of psychoactive meds in the last four weeks? med1_change = has there been a change of dosage for med 1 in last 4 wks? (1==yes)
# where med1_change == 1 when med1_type is not NA and med1_type doesn't equal 8 (non psycho med)
table(c((collectiveDat$med1_change==1 & collectiveDat$med1_type!=8 & collectiveDat$childmedpre==1) |
          c(collectiveDat$med2_change==1 & collectiveDat$med2_type!=8 & collectiveDat$childmedpre==1) |
          c(collectiveDat$med3_change==1 & collectiveDat$med3_type!=8 & collectiveDat$childmedpre==1)))
# FALSE  TRUE 
# 144     3   (Manually checked, this is a different 3 than the 2 that started in last 4 weeks; and 1 stimulant, 1 antidepr., 1 other psycho)






