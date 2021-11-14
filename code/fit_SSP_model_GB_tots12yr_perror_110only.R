## This script loads in  human data and then proceeds through the fitting Script to fit parameters to the data. 
## The first section of code is a function that call the rcpp code (seperate file), which contains the actual model,
## simulates data based on a set of parameters, and outputs a fit stat. The The second section of code is the "main script"
## which loads in data and starts the optimization routine, which then repeately calls the function (first section of code),
## and ultimately saved the best fitting params.
##
## To run, this script requires some r additional r packes to be installed, the rcpp file, as well as a data file.


start_time <- Sys.time()

#------------------------------------------------------------------------------
# Fit function for the SSP model
fitFunctionSSP <- function(parms, nTrials, cutPoints, humanProps, HumanTrialCounts){
  
  #source the rcpp function that will be used for simulating the model (implemented in cpp to improve speed)
  # The rcpp file contains the actual "model"; all of the code in this script is for loading data, calling 
  # the model to simulate, fit the data, etc.
  Rcpp::sourceCpp("C:/Users/gbuzzell/Documents/George/compModels/SSP_deOptim/simSSP_model_GB_noScale.cpp")
  
  # initialise empty matrix for simulation data with three columns (RT, accuracy, congruency)
  # and with rows = nTrials
  SimData <- matrix(0, nrow = nTrials * 2, ncol = 3)
  colnames(SimData) <- c("rt", "accuracy", "congruency")
  SimData <- data.frame(SimData)
  
  #simulate the ssp model, using current parms and returnn simulated data (trial level RT and accuracy)
  #to simulate data, call an rcpp function (rcpp used to speed up simulation, and in turn, model fitting)
  #Call simulation function twice, once for congruent, once for incongruent
  # 
  #simulate congruent trials and save simulated trial-level data
  set.seed(42) # Before running simulation for this param set, set random number seed, so psuedo-ranndomnass is identical across param sets
  SimData[1:nTrials, 1:2] <-simSSP_model_GBnoScale(parms, trialType = 1, nTrials, dt = 0.001, vari = 0.01)
  SimData[1:nTrials, 3] <- 0 #congruent
  set.seed(as.numeric(Sys.time())) #now that simulation has been run, reset the random seed to system time (to not mess up other functions using random numbers)

  #Now simulate incongruent trials and save simulated trial-level data
  set.seed(42) # Before running simulation for this param set, set random number seed, so psuedo-ranndomnass is identical across param sets
  SimData[(nTrials + 1):(nTrials * 2), 1:2] <- simSSP_model_GBnoScale(parms, trialType = 2, nTrials, dt = 0.001, vari = 0.01)
  SimData[(nTrials + 1):(nTrials * 2), 3] <- 1 #incongruent
  set.seed(as.numeric(Sys.time())) #now that simulation has been run, reset the random seed to system time (to not mess up other functions using random numbers)
  
  #Identify the proportions in each (human-based) cutoff, for the simulated data
  #
  #initialize variable to hold bin proportions for sim data
  #(1=congruent-correct;2=congruent-error;3=incongruent-correct;4=incongruent-error)
  simProps <- list()
  #
  #initialize variable to hold bin proportion differences for human and sim data
  #(1=congruent-correct;2=congruent-error;3=incongruent-correct;4=incongruent-error)
  binDiffs <- list()
  #
  #initialize variable (vector) to hold sums of bin proportion differences for human and sim data
  #(1=congruent-correct;2=congruent-error;3=incongruent-correct;4=incongruent-error)
  sum_binDiffs <- numeric(4)
  #
  #initialize counter variable
  counter <- numeric(1)
  #
  #Loop over conditions % number of conditions = 2 (congruent, incongruent)
  for(i in 1:2){
    
    #clear out variable for this iteration of loop
    sim_condData <- NULL
    
    #select congruent/incongruent condition
    if(i == 1){
      sim_condData <- subset(SimData, SimData$congruency == 0)
    } else {
      sim_condData <- subset(SimData, SimData$congruency == 1)
    }
    
    #loop over accuracy (correct, error)
    for(j in 1:2){
      
      #select correct/error data for this condition
      if(j == 1){
        trialAccuracy = 1 #select correct trials
        counter <- counter+1
      } else {
        trialAccuracy = 0 #select error trials
        counter <- counter+1
      }
      sim_accCondData <- subset(sim_condData, sim_condData$accuracy == trialAccuracy) #select the data 
      
      #determine number of bins (not quantiles) based on which condition and if this is error or correct
      nBins <- length(cutPoints[[counter]])+1 #add 1 to number of cutPoints (or quantiles) to get bins
      
      #initialize temporary vector to hold proportions; this will be wrtten into the simProps list after the bin loop
      temp_simProps<-NULL
      temp_simProps<-numeric(nBins)
      
      #initialize temporary vector to bin proportion difference between human and sim data;
      #this will be wrtten into the binDiffs list after the bin loop
      temp_binDiffs<-NULL
      temp_binDiffs<-numeric(nBins)
      
      # loop over each bin, find the bin boundaries, and calculate the proportion of RT in each bin
      #PROPORTIONS ARE NUMBER TRIALS IN THIS BIN, FOR THIS CONDITION, DEVIDED BY TOTAL TRIALS IN THIS CONDITION (CORRECT + ERROR)
      #[ (# trials in this bin) / (# trials in all correct bins for this condition + # trials in all error bins for this condition) ]
      #for simulated data, number of trials inn each condition = nTrials
      for(k in 1:nBins){
        
        #initialize variable to hold data for this bin
        binData <-NULL
        
        # first bin
        if(k == 1){
          # get the data in the current bin
          binData <- subset(sim_accCondData, sim_accCondData$rt <= cutPoints[[counter]][k])
          # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
          #if the proportion is zero, convert to a very small number (neccesary for later chi-square computation)
          if(length(binData[, 1]) > 0){
            temp_simProps[k] <- length(binData[, 1]) / nTrials #save actual prop if non-zero
          } else {
            temp_simProps[k] <- .0000001 #default to very small value if prop is zero
          }
          
          #(for chi square): compute differnce between human and sim props, square, divide
          #by sim props, multiply by number of trials for this condition (in human data)
          ### IS IT CORRECT TO MULTIPLY BY THE NUMBER OF HUMAN TRIALS???
          temp_binDiffs[k] <-  HumanTrialCounts[i]*((humanProps[[counter]][k] - temp_simProps[k])^2/temp_simProps[k])
          
        }
        
        # middle bins
        if(k > 1 & k <= nBins){
          # get the data in the current bin
          binData <- subset(sim_accCondData, sim_accCondData$rt > cutPoints[[counter]][k-1] & sim_accCondData$rt <= cutPoints[[counter]][k])
          # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
          #if the proportion is zero, convert to a very small number (neccesary for later chi-square computation)
          if(length(binData[, 1]) > 0){
            temp_simProps[k] <- length(binData[, 1]) / nTrials #save actual prop if non-zero
          } else {
            temp_simProps[k] <- .0000001 #default to very small value if prop is zero
          }
          
          #(for chi square): compute differnce between human and sim props, square, divide
          #by sim props, multiply by number of trials for this condition (in human data)
          ### IS IT CORRECT TO MULTIPLY BY THE NUMBER OF HUMAN TRIALS???
          temp_binDiffs[k] <-  HumanTrialCounts[i]*((humanProps[[counter]][k] - temp_simProps[k])^2/temp_simProps[k])         
        }
        
        # last bin
        if(k == nBins){
          # get the data in the current bin
          binData <- subset(sim_accCondData, sim_accCondData$rt > cutPoints[[counter]][k-1])
          # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
          #if the proportion is zero, convert to a very small number (neccesary for later chi-square computation)
          if(length(binData[, 1]) > 0){
            temp_simProps[k] <- length(binData[, 1]) / nTrials #save actual prop if non-zero
          } else {
            temp_simProps[k] <- .0000001 #default to very small value if prop is zero
          }
          
          #(for chi square): compute differnce between human and sim props, square, divide
          #by sim props, multiply by number of trials for this condition (in human data)
          ### IS IT CORRECT TO MULTIPLY BY THE NUMBER OF HUMAN TRIALS???
          temp_binDiffs[k] <-  HumanTrialCounts[i]*((humanProps[[counter]][k] - temp_simProps[k])^2/temp_simProps[k])
        }
        
      } #END Loop over bins (k), for this correct/error (j), for this condition (i)
      
      #save the human props (from temp_simProps vector) into simProps list
      simProps[[counter]]<-temp_simProps
      
      #save the bin proportion differences between human and sim data (from temp_binDiffs vector) into binDiffs list
      binDiffs[[counter]]<-temp_binDiffs
      #sum over the bin proportion differences (for chi square)
      sum_binDiffs[[counter]] <- sum(temp_binDiffs)
      
    } #END Loop over correct/error (j), for this condition (i)
    
  }#END Loop over congruent/incongruent (i)
  
  #compute the chi-squre statistic by summing across all the bin proportion differences (diff between human and sim)
  chiSquare <- sum(sum_binDiffs)
  
  #return the chiSuare from the fitFunctionSSP, unless chiSquare is infinate, then return really large value instead
  if(chiSquare == Inf){
    return(.Machine$double.xmax)
  } else {
    return(chiSquare)
  }
  
}# End of fitFunctionSSP
#------------------------------------------------------------------------------










library(DEoptim)
library("Rcpp")

#switch to the desired workng directory
setwd('C:/Users/gbuzzell/Documents/George/compModels/SSP_deOptim/')

#set up default parms and upper/lower values (for now, not using default values, just upper/lower with DEoptim)
Upper <- c(0.232, 0.420, 0.630, 0.067,  3.093); #mean of white 2011 plus 5 sd
Lower <- c(0.032, 0.180, 0.130, 0.0001, 0.493); #mean of white 2011 minus 5 sd
numParams <- length(Upper)

# how many trials to simulate per condition 
nTrials = 1000

#names files
HumanDataName <-"flankr_data_priorAcc_SocialContext_new.csv"
FitOutputName <-"1000trialFits_5sdBounds_For_flankr_data_priorAcc_SocialContext_new_Subs101toEND_"

#get the desired humann data to fit the ssp model to
importDat = read.csv(HumanDataName, header = FALSE) #data does not have header
#add headers to imported data file
colnames(importDat) <- c("subject", "condition", "rt", "accuracy", "congruency")

#get sub list
subList <- (unique(importDat$subject))

#loop through and calculate seperate fits for the conditions of interest (conditions other than congruency)
for(c in 1:2){
  
  #100: postError,NonSocial
  #110: postCorr,NonSocial
  #101: postError,Social
  #111: postCorr,Social
  
  if(c == 1){
    whichCondition <- 110
  } 
  if(c == 2){
    whichCondition <- 111
  }
  
  # initialise output vector for all the fits for this condition
  fitOutput <- matrix(0, nrow = length(subList), ncol = 7)
  colnames(fitOutput) <- c("subject", "a", "ter", "p", "rd", "sda","fitStat")
  fitOutput <- data.frame(fitOutput)
  
  ## loop through all subs, fit data, store best fits
  #for(s in 1:length(subList)){
  for(s in 101:length(subList)){
      
    #pull out data for this subject
    subData <- subset(importDat, importDat$subject == subList[s])
    #data = subData
    
    #pull out data for this condition (for this subject)
    data <- subset(subData, subData$condition == whichCondition)

    #Identify the trial counts, bin cuttoffs, and proportions for the HUMAN data
    #
    #initialize vector to save trial counts (1=congruent; 2=incongruent)
    HumanTrialCounts <- numeric(2)
    
    #initialize variables to hold human quantiles, cutpoints, bin proportions
    #(1=congruent-correct;2=congruent-error;3=incongruent-correct;4=incongruent-error)
    quantiles <- list()
    cutPoints <- list()
    humanProps <- list()
    
    #initialize counter variable
    counter <- numeric(1)
    
    #Loop over conditions % number of conditions = 2 (congruent, incongruent)
    for(i in 1:2){
      
      #clear out variable for this iteration of loop
      condData <- NULL
      
      #select congruent/incongruent condition
      if(i == 1){
        condData <- subset(data, data$congruency == 0)
      } else {
        condData <- subset(data, data$congruency == 1)
      }
      
      #For this condition, and based on the HUMAN data, save the total number of trials
      #in this condition (errors and corrects)
      HumanTrialCounts[i] <- nrow(condData)
      
      #For this condition, and based on the HUMAN data, determine whether the number of trials (t)
      #is <= 5, 5 < t <= 10, Ne > 10. Set the number of  bins to be: 2 (.5), 4 (.1, .5, .9),
      #or 6 (.1, .3, .5, .7, .9) and identify the RT cutoffs that will divide the  trial data
      #into the appropriate number of bins. If there are no trials for this condition, then data
      #are not included in chi-square for this condition.
      #
      #loop over accuracy (correct, error)
      for(j in 1:2){
        
        #select correct/error data for this condition
        if(j == 1){
          trialAccuracy = 1 #select correct trials
          counter <- counter+1
        } else {
          trialAccuracy = 0 #select error trials
          counter <- counter+1
        }
        accCondData <- subset(condData, condData$accuracy == trialAccuracy) #select the data 
        
        #determine the proper number of quantiles/bins based on number of trials for this condition-accuracy
        if(nrow(accCondData)<=5){
          quantiles[[counter]] = c(.5)
        }
        if(nrow(accCondData)>5 & nrow(accCondData)<=10){
          quantiles[[counter]] = c(.1, .5, .9)
        }
        if(nrow(accCondData)>10){
          quantiles[[counter]] = c(.1, .3, .5, .7, .9)
        }
        
        # calculate RT cutoffs corresponding to quantiles 
        cutPoints[[counter]] <- as.numeric(quantile(accCondData$rt, quantiles[[counter]]))
        
        #determine number of bins (not quantiles) based on which condition and if this is error or correct
        nBins <- length(cutPoints[[counter]])+1 #add 1 to number of cutPoints (or quantiles) to get bins
        
        #initialize temporary vector to hold proportions; this will be wrtten into the humanProps list after the bin loop
        temp_humanProps<-NULL
        temp_humanProps<-numeric(nBins)
        
        # loop over each bin, find the bin boundaries, and calculate the proportion of RT in each bin
        #PROPORTIONS ARE NUMBER TRIALS IN THIS BIN, FOR THIS CONDITION, DEVIDED BY TOTAL TRIALS IN THIS CONDITION (CORRECT + ERROR)
        #[ (# trials in this bin) / (# trials in all correct bins for this condition + # trials in all error bins for this condition) ]
        for(k in 1:nBins){
          
          #initialize variable to hold data for this bin
          binData <-NULL
          
          # first bin
          if(k == 1){
            # get the data in the current bin
            binData <- subset(accCondData, accCondData$rt <= cutPoints[[counter]][k])
            # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
            #if the proportion is zero, convert to a very small number (neccesary for later chi-square computation)
            if(length(binData[, 1]) > 0){
              temp_humanProps[k] <- length(binData[, 1]) / HumanTrialCounts[i]
            } else {
              temp_humanProps[k] <- .0000001 #default to very small value if prop is zero
            }
          }
          
          # middle bins
          if(k > 1 & k <= nBins){
            # get the data in the current bin
            binData <- subset(accCondData, accCondData$rt > cutPoints[[counter]][k-1] & accCondData$rt <= cutPoints[[counter]][k])
            # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
            #if the proportion is zero, convert to a very small number (neccesary for later chi-square computation)
            if(length(binData[, 1]) > 0){
              temp_humanProps[k] <- length(binData[, 1]) / HumanTrialCounts[i]
            } else {
              temp_humanProps[k] <- .0000001 #default to very small value if prop is zero
            }        
          }
          
          # last bin
          if(k == nBins){
            # get the data in the current bin
            binData <- subset(accCondData, accCondData$rt > cutPoints[[counter]][k-1])
            # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
            #if the proportion is zero, convert to a very small number (neccesary for later chi-square computation)
            if(length(binData[, 1]) > 0){
              temp_humanProps[k] <- length(binData[, 1]) / HumanTrialCounts[i]
            } else {
              temp_humanProps[k] <- .0000001 #default to very small value if prop is zero
            }        
          }
          
        } #END Loop over bins (k), for this correct/error (j), for this condition (i)
        
        #save the human props (from temp_humanProps vector) into humanProps list
        humanProps[[counter]]<-temp_humanProps
        
      } #END Loop over correct/error (j), for this condition (i)
      
    }#END Loop over congruent/incongruent (i)
    
    modelStart <- "Brace yourself... Model Fit Running; Please Wait..."
    print(modelStart)
    
    # perform the fit
    OptimFitResults <- DEoptim(fitFunctionSSP, lower = Lower, upper = Upper, control = DEoptim.control(parallelType = 1, packages = c("Rcpp"), parVar = c("nTrials","cutPoints","humanProps","HumanTrialCounts")), nTrials = nTrials, cutPoints = cutPoints, humanProps = humanProps, HumanTrialCounts = HumanTrialCounts, fnMap=NULL)
  
    modelFinished <- "Woohoo! Model Fit Finished!!"
    print(modelFinished)
    
    #pull out the best fitting parms
    bestParms <- as.numeric(OptimFitResults$optim$bestmem)
    fitOutput[s,1] <- subList[s];
    fitOutput[s,2:6] <- bestParms;
    
    #pull out the best fitting parms
    bestFitStat <- as.numeric(OptimFitResults$optim$bestval)
    fitOutput[s,7] <- bestFitStat;
    
  }#end loop through subs

  #Write the simulated data to disk
  write.csv(fitOutput, paste0(FitOutputName, whichCondition,".csv"), row.names=FALSE, na="", quote = F) # generated parms

}#end loop through conditions


#end_time <-Sys.time()
#end_time-start_time
