## This script loads in  human data and then proceeds through the fitting Script to fit parameters to the data. 
## The first section of code is a function that call the rcpp code (separate file), which contains the actual model,
## simulates data based on a set of parameters, and outputs a fit stat. The second section of code is the "main script"
## which loads in data and starts the optimization routine, which then repeatedly calls the function (first section of code),
## and ultimately saved the best fitting params.
##
## To run, this script requires some r additional r packages to be installed, the rcpp file, as well as a data file.

start_time <- Sys.time()

#------------------------------------------------------------------------------
# Fit function for the SSP model
fitFunctionSSP <- function(parms, nTrials, cutPoints, humanProps, HumanTrialCounts){
  
  #source the rcpp function that will be used for simulating the model (implemented in cpp to improve speed)
  # The rcpp file contains the actual "model"; all of the code in this script is for loading data, calling 
  # the model to simulate, fit the data, etc.
  Rcpp::sourceCpp("simSSP_model_GB_noScale.cpp")

  
  # initialize empty matrix for simulation data with three columns (RT, accuracy, congruency)
  # and with rows = nTrials
  SimData <- matrix(0, nrow = nTrials * 2, ncol = 3)
  colnames(SimData) <- c("rt", "accuracy", "congruency")
  SimData <- data.frame(SimData)
  
  #simulate the ssp model, using current parms and return simulated data (trial level RT and accuracy)
  #to simulate data, call an rcpp function (rcpp used to speed up simulation, and in turn, model fitting)
  #Call simulation function twice, once for congruent, once for incongruent
  # 
  #simulate congruent trials and save simulated trial-level data
  set.seed(42) # Before running simulation for this param set, set random number seed, so psuedo-ranndomness is identical across param sets
  #make sure that the "simSSP_model_GBnoScale" bit is called whatever it calls itself inside the actual .cpp file
  SimData[1:nTrials, 1:2] <-simSSP_model_GBnoScale(parms, trialType = 1, nTrials, dt = 0.001, vari = 0.01) 
  SimData[1:nTrials, 3] <- 0 #congruent
  set.seed(as.numeric(Sys.time())) #now that simulation has been run, reset the random seed to system time (to not mess up other functions using random numbers)

  #Now simulate incongruent trials and save simulated trial-level data
  set.seed(42) # Before running simulation for this param set, set random number seed, so psuedo-ranndomeass is identical across param sets
  #make sure that the "simSSP_model_GBnoScale" bit is called whatever it calls itself inside the actual .cpp file
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
      
      #initialize temporary vector to hold proportions; this will be written into the simProps list after the bin loop
      temp_simProps<-NULL
      temp_simProps<-numeric(nBins)
      
      #initialize temporary vector to bin proportion difference between human and sim data;
      #this will be written into the binDiffs list after the bin loop
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
          
          #(for chi square): compute difference between human and sim props, square, divide
          #by sim props, multiply by number of trials for this condition (in human data)
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
          
          #(for chi square): compute difference between human and sim props, square, divide
          #by sim props, multiply by number of trials for this condition (in human data)
          temp_binDiffs[k] <-  HumanTrialCounts[i]*((humanProps[[counter]][k] - temp_simProps[k])^2/temp_simProps[k])         
        }
        
        # last bin
        if(k == nBins){
          # get the data in the current bin
          binData <- subset(sim_accCondData, sim_accCondData$rt > cutPoints[[counter]][k-1])
          # find the proportion of data in this bin (proportion out of both error and correct, for this condition)
          #if the proportion is zero, convert to a very small number (necessary for later chi-square computation)
          if(length(binData[, 1]) > 0){
            temp_simProps[k] <- length(binData[, 1]) / nTrials #save actual prop if non-zero
          } else {
            temp_simProps[k] <- .0000001 #default to very small value if prop is zero
          }
          
          #(for chi square): compute difference between human and sim props, square, divide
          #by sim props, multiply by number of trials for this condition (in human data)
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











library("DEoptim")
library("Rcpp")

#switch to the desired working directory
setwd("")


#set up default parms and upper/lower values (for now, not using default values, just upper/lower with DEoptim)
Upper <- c(0.232, 0.420, 0.630, 0.067,  3.093); #mean of white 2011 plus 5 sd
Lower <- c(0.032, 0.180, 0.130, 0.0001, 0.493); #mean of white 2011 minus 5 sd
numParams <- length(Upper)

# how many trials to simulate per condition 
nTrials = 10000

#names files
HumanDataName <-"clean_flanker_data.csv"
FitOutputName <-"ddm_output_data"

#get the desired humann data to fit the ssp model to
importDat = read.csv(HumanDataName, header = TRUE) #data has header

#convert rt values from ms to secs to be consistent with rest of script
importDat$rt <- as.numeric(importDat$rt / 1000)

#get sub list
subList <- (unique(importDat$subject))

# initialize output vector 
fitOutput <-data.frame(matrix(ncol=9, nrow=0))
colnames(fitOutput) <- c("subject", "a", "ter", "p", "rd", "sda","fitStat","iterNum", "whichCondition")
fitOutput <- data.frame(fitOutput)
#also write a .csv to append parms to at end of loops
write.csv(fitOutput, paste0(FitOutputName, ".csv"), row.names=FALSE, na="", quote = F)



# #loop through and calculate separate fits for the conditions of interest (conditions other than congruency)
for(c in 1:2){

  #100: postError
  #111: postCorr

  if(c == 1){
    whichCondition <- 100
  }
  if(c == 2){
    whichCondition <- 111
  }

  
  ## loop through all subs, fit data, store best fits
  for(s in 1:length(subList)){
  #for(s in 2:4){
      
    #pull out data for this subject
    subData <- subset(importDat, importDat$subject == subList[s])
   
    ## commenting out selection of conditions bc only doing one condition for now
    ##pull out data for this condition (for this subject)
    data <- subset(subData, subData$priorAccuracy == whichCondition) #this is for having post-err/corr conditions
    #data = subData; #need this to stay consistent with rest of code since commenting out conditions if not running post-err/corr condition
    
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
      # OAS- changed to 1,2 from 0,1 in order to match the data file
      if(i == 1){
        condData <- subset(data, data$congruency == 1)
      } else {
        condData <- subset(data, data$congruency == 2)
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
    
    modelStart <- "Wow, nice model you got there! I better start fitting it... Model Fit Running; Please Wait..."
    print(c(modelStart, s, subList[s], whichCondition))
    
    # perform the fit
    OptimFitResults <- DEoptim(fitFunctionSSP, lower = Lower, upper = Upper, control = DEoptim.control(itermax = 200, steptol = 20, parallelType = 1, packages = c("Rcpp"), parVar = c("nTrials","cutPoints","humanProps","HumanTrialCounts")), nTrials = nTrials, cutPoints = cutPoints, humanProps = humanProps, HumanTrialCounts = HumanTrialCounts, fnMap=NULL)
    
    modelFinished <- "Woohoo! Model Fit Finished!!"
    print(c(modelFinished, s, subList[s], whichCondition))
    
    subject <- subList[s]
    
    #pull best fitting parms- 1/5- a
    bestParms1 <- as.numeric(OptimFitResults$optim$bestmem[1])
    #pull best fitting parms- 2/5- ter
    bestParms2 <- as.numeric(OptimFitResults$optim$bestmem[2])
    #pull best fitting parms- 3/5- p
    bestParms3 <- as.numeric(OptimFitResults$optim$bestmem[3])
    #pull best fitting parms- 4/5- rd
    bestParms4 <- as.numeric(OptimFitResults$optim$bestmem[4])
    #pull best fitting parms- 5/5- sda
    bestParms5 <- as.numeric(OptimFitResults$optim$bestmem[5])

    #pull out the best fitting fitStat
    bestFitStat <- as.numeric(OptimFitResults$optim$bestval)
    
    #pull out stop iteration
    iter <- as.numeric(OptimFitResults$optim$iter)
    
    newRow<- NULL 
    #make new row with values created above
    newRow <- data.frame(subject, bestParms1, bestParms2, bestParms3, bestParms4, bestParms5, bestFitStat, iter, whichCondition)
    
    # appending the parms to the csv created before the loops
    write.table(newRow, file=paste0(FitOutputName, ".csv"), sep=",", append=TRUE, col.names = FALSE, row.names = FALSE)

  }#end loop through subs

## commenting out loop over conditions bc only doing one condition for now
}#end loop through conditions


end_time <-Sys.time()
end_time-start_time



