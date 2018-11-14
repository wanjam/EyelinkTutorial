## This file serves as a template for preprocessing Eyelink eyetracking data in
## R.Only works on Linux. Check dependencies of edfR online.
## author: Wanja Moessing, November, 2018, moessing@wwu.de

# The basic idea is that you ran an experiment in python or matlab and during
# each trial, you sent messages (e.g., with EyelinkSendTabMsg from my
# repository). Those messages should always mark interesting timepoints, just
# like TTL triggers in EEG recordings do. In addition you might have sent one
# or more, messages per trial, that included the behavioral data of that trial.
# The goal of this script is to parse the edf file, extract the behavioral data
# , downsample the eyetracking data, align behavioral and eyetracking data in a
# long table format, interpolate blinks, cut out useless time intervals (like
# between two trials), and finally store all subjects data in one huge file. In
# a last step, Eyetracking data are "downsampled" to one datum per trial, to get
# an additional copy with behavioral data only.

################################################################################
######################## STEP 1: necessary packages ############################
################################################################################
## We need a couple of packages. Some are not available on CRAN, but only from
## Github. Some of those (like mine) are simply not well enough documented and
## maintained to be made *that* public. Others, like edfr, depend on third-party
## software (SR's devpack). To install from github, we need the install_github()
## function from the devtools-package.

# install.packages('devtools','doParallel','data.table','ggplot2','lme4','dplyr','afex')
# devtools::install_github('hedderik/pR')
# devtools::install_github('wanjam/wm_utilities/helper-funs/wmR')
# devtools::install_github('hedderik/hvR')
# devtools::install_github('jashubbard/edfR')
# devtools::install_github("jwdink/eyetrackingR") #check http://www.eyetracking-r.com for elaborate tutorial

#Differences EDF2R/edfR: 
## WHILE EDF2R works on all OS, edfR does not work on windows. However, it 
##   imports more stuff (eg saccades etc).
## edf2R only imports the frst onset of a recordingblock, edfR all (extract first to get meaningful info)
## edf2R sets missing gaze info (e.g., during blinks) to NA. edfR to 1e+08 (=100000000).
## edf2R samples are a few rows longer because edfR deletes nonsense info at end automatically
## rownames are a bit different but generally quite intuitive in both packages.
## edf2R only reads ttl triggers in wanja's modified version. edfR can simulate 
## this via edf.events(file,type="INPUTEVENT",fields = c("input","sttime"))

################################################################################
######################## STEP 2: Preparation & Parameters ######################
################################################################################
## load libraries
pkgs <- c('data.table', 'hvR', 'pR', 'doParallel', 'edfR', 'stringr')
sapply(pkgs, require, character.only = TRUE)

## Define baselineperiod & Hz for downsampling
# Below, you can define to what the time used by the baseline will be erelative. 
# (e.g., trial onset). So the numbers here always depend on whatever you write 
# below. Look for function 'normalize' ~ line 299.
baselineperiod <- c(-100, 0)
outHz          <- 100

## Extracting behavioral data is always the same for all subjects. That is, this 
## task is "embarassingly parallel". You can activate these lines to run things
## in parallel, once you have many subjects to process.
## Start workers (i.e., R-Sessions) on other cores & load packages in these sessions.
cl <- makeCluster(2, outfile = "")
registerDoParallel(cl)

## Specify in- and output
EDFpath     <- "EDF/" # adjust this to the path where your .edf files are stored
RDATpath    <- "EDF/Rdat" # output directory for temporary parsed files
OUTpath     <- "Eyedata/" # output directory
EDFpattern  <- "TE_\\d+.[edf|EDF]" # pattern used to match your filenames (see https://github.com/rstudio/cheatsheets/raw/master/strings.pdf)
RDATpattern <- "TE_\\d+.Rdat"
OUTprefix   <- "TutorialExperiment" 

# create output folders
if (!dir.exists(RDATpath)) {dir.create(RDATpath)}
if (!dir.exists(OUTpath)) {dir.create(OUTpath)}

################################################################################
######################## STEP 3: Parse EDF files ###############################
################################################################################
## Check the directories from above for existing edf files and also for existing
## Rdat files. We check for Edat files as well, to avod parsing already parsed 
## files again.
edfs  <- str_replace_all(list.files(path = EDFpath, pattern = EDFpattern),
                         '.edf', '')
Rdats <- str_replace_all(list.files(path = RDATpath, pattern = RDATpattern),
                         '.Rdat', '')


## convert EDFs that haven't been processed yet to Rdats. This step is Linux only!
# compare existing edfs with existing Rdats
unprocessed_edfs <- edfs[!str_detect(edfs, Rdats)]
if (length(unprocessed_edfs) > 0) {
  # run a parallel for loop across remaining edfs. tip: you can change %dopar% to %do% to make this a regular for-loop
  foreach(fname = unprocessed_edfs, .packages = c('data.table','edfR')) %dopar% {
    curfile              <- paste0(EDFpath, fname, '.edf')
    
    # edf.all parses 'all' possible data.types in the file. You can also just
    # parse parts of it. Usually that's not gonna be necessary, though.
    curdat               <- edf.all(curfile, samples = TRUE, eventmask = TRUE)
    
    # The file header contains some meta-info (tracked eye, Hz, etc). get that:
    curdat$recordingInfo <- edf.recordings(curfile)
    curdat$recordingInfo <- curdat$recordingInfo[1,]  # we only need the info once
    
    # If you did EEG as well, you might want to also parse the TTL triggers.
    curdat$ttl           <- edf.events(curfile, type = "INPUTEVENT", 
                                       fields = c("sttime","input"))
    save(file = paste0(RDATpath, fname, ".Rdat"), curdat)
  }
  
  # if we preprocessed some new files, check what Rdat files do exist now
  Rdats <- str_replace_all(list.files(path = RDATpath, pattern = RDATpattern),
                           '.Rdat', '')
} else {
  cat("Files seem to be converted already!")
}


################################################################################
######################## STEP 4: Extract Behavioral Data #######################
################################################################################
## Read the RDAT files, and process them:
cat('\nPerforming coregistration of Eyetracking and behavioral data for all ',
    'subjects.\nThis might take a while...')

## For each subject, we loop over all messages sent to the Eyelink during a
## session. Once we find a message that starts with '<', we know that it's
## special, as we added that to all our special messaged during the experiment.
## We split up the tab-delimited messages following the '<' and extract the
## relevant data. From our experimental code, we know what infomation is at what
## position.

# note that we use <.combine = 'rbind'> here. That means, all results of the
# independent instances of the loop will be stacked by rows. Thus we can assign
# the single resulting data.table to one variable called 'samples'.
samples <- foreach(
  fname = Rdats, .packages = c('data.table','pR'),
  .combine = 'rbind') %do% {
    # load the current subject file:
    load(paste0(RDATpath, fname, ".Rdat"))
    
    # convert to data.table for more efficient syntax, speed, and RAM use.
    samples <- data.table(curdat$samples) #curdat is the data.table of the single subject's raw data
    curdat$samples <- NULL # save RAM
    setkey(samples, time) # keys samples by time (speed)
    
    ## Find out which eye was used and delete the nonsense columns of the other eye
    if (curdat$recordingInfo$eye == 2) {
      setnames(samples, c("Time", "Flags", "remove1", "remove2", "remove3", "X",
                          "Y", "Dil", "Blink", "Fixation", "Saccade"))
    } else if (curdat$recordingInfo$eye == 1) {
      setnames(samples, c("Time", "Flags", "X", "Y", "Dil", "remove1", "remove2",
                          "remove3", "Blink", "Fixation", "Saccade"))
    } else {
      stop("I cannot determine which Eye has been tracked\n") # talk to me if you catch this error. There's a seldom problem with a workaround...
    }  
    
    # delete the nonsense columns of the other eye
    samples[, c('remove1', 'remove2', 'remove3') := NULL]
    
    ## Assign trial information to all the samples. Register the start time of a trial,
    ## store the relevant variables, and then when we know the end time, assign these variables
    ## to the output data.table "samples".
    numTrial <- 0
    
    # in case this loop gives the warning "input string 1 is invalid in this
    # locale", use Sys.setlocale(locale="C")
    
    ## now we loop over all messages, including all the stuff that's auto-sent
    ## by eyelink.
    for (i in 1:nrow(curdat$messages)) {
      # store the current message in e
      e <- curdat$messages[i,]
      # split it by tabs
      msgSplit <- strsplit(e$msg,"\t")[[1]]
      
      # our special informative messages have a common '>' as first character
      # skip iteration if not present
      if (msgSplit[1] != '>') {next}
      
      # we only get here if it's a special message. now we need to check which
      # one. That's usually written in the second argument. e.g., if you sent 
      # something like EyelinkSendTabMsg('TrlOn',ID,itrial) in Matlab, you'd be
      # looking for 'TrlOn'
      if (msgSplit[2] == 'TrlOn') {
        # yay! we found the onset of a new trial
        numTrial  <- numTrial + 1
        cat("Working on trial ", numTrial, "\t of file:", fname,
            "\t on worker:", Sys.getpid(), "\n")
        # as the messages are already aligned with the eyetrackig data, we can
        # use the eyetrackers time vector to store the absolute time when this
        # message was sent.
        tmp.startT <- as.numeric(e$time)
        foundNewTrl <- TRUE
      }
      
      
      ## once we found that starting message, we keep on looping, trying to find 
      ## more messages for the current trial.
      if (foundNewTrl) {
        if (msgSplit[2] == "FixOn") {
          tmp.FixOn <- as.numeric(e$time)
        }
        if (msgSplit[2] == "FixOff") {
          tmp.FixOff <- as.numeric(e$time)
        }
        
        
        ## the "Info" message is, in my case, special for two reasons:
        ## 1. it contains all behavioral data
        ## 2. it is the last message in a trial and hence marks the end of a trial
        ## Because it marks the end of a trial, whenever we catch this message
        ## we add all our temporary variables to our output data.table
        if (msgSplit[2] == "Info") {
          tmp.SubjNr  <- as.integer(msgSplit[3])
          tmp.TrlNr   <- as.integer(msgSplit[4]) # if on python, keep in mind that it starts counting from 0
          tmp.endT    <- as.numeric(e$time)
          
          # add information coded in messages in long format to our 1000Hz samples-DT
          # we now start & end time of the current trial in eyetracker-time,
          # so we assign all behavioral values to the respective part of the 
          # huge data.table
          samples[Time %between% list(startT, endT), ':='(
            Subject           = tmp.SubjNr,
            Trial             = tmp.TrlNr,
            startTime         = tmp.startT,
            endTime           = tmp.endT,
            RelTime           = Time - tmp.startT,
            FixationOnset     = tmp.FixOn - tmp.startT,
            RespOnset         = tmp.RespOn - tmp.startT,
            RespOffset        = tmp.RespOff - tmp.startT
          )]
          
          # Deleting the temporary variables after each trial makes sure we 
          # don't mix values between trials. 
          allvars <- ls()
          tmpvars <- allvars[str_detect(allvars, 'tmp.')]
          rm(list = tmpvars)
          rm(allvars, tmpvars)
          
          # we're done with the trial, so set this to false again, until we find
          # another trial start
          foundNewTrl = FALSE
        }
      }
    }
    
    ############################################################################
    ######################## STEP 5: Massage Eyetracking Data ##################
    ############################################################################
    ## add mean and peak velocity per saccade
    # set keys, so data.table can do a fast vector scan
    setkey(samples, Time)
    saccades <- as.data.table(curdat$saccades)
    setkey(saccades, sttime, entime)
    
    # extract only those saccades that happened during a trial
    saccades <- saccades[sttime %in% samples[!is.na(Subject), Time] | 
                           entime %in% samples[!is.na(Subject), Time],
                         ]
    
    cat("Adding saccade velocity to samples Data.Table\n")
    for (i in 1:nrow(curdat$saccades)) {
      samples[Time %between% list(saccades[i, sttime], saccades[i, entime]),
              ':='(
                AverageVelocity       = saccades[i, avel],
                PeakVelocity          = saccades[i, pvel],
                IthSaccadeThisSubject = i)
              ]
    }
    
    ## add TTL to long format
    curdat$ttl <- as.data.table(curdat$ttl)
    cat("\nAdding TTL triggers to samples data.table\n")
    for (i in 1:nrow(curdat$ttl)) {
      ttlOnset <- curdat$ttl[i, sttime]
      ttl      <- curdat$ttl[i, input]
      samples[Time == ttlOnset, ':='(TTL = ttl)]
    }
    
    ## remove temporary variables
    rm('curdat', 'ttl', 'ttlOnset', 'saccades')
    
    ## Remove the samples that weren't between a TrialStart and a TrialEnd 
    ## message (and hence don't have a subject-Nr). Conserves RAM & HDD
    setkey(samples, Subject)
    samples <- samples[!is.na(Subject),]
    
    cat('\nDetecting blinks') # I found 20 to be a good value. Check the functions documentation if you want more info
    samplesB <- detectblinks(samples, maxDeltaDilation = 20)
    cat('\nExpanding blinks')
    samplesB <- expandblinks(samplesB, 50)
    cat('\nPerforming linear interpolation for expanded blinks')
    samplesB <- interpolateblinks(samplesB)
    cat('\nBaseline correction of pupil dilation data [baseline-period:',
        baselineperiod[1], '-', baselineperiod[2], 'ms]')
    samples  <- normalize(samplesB, samplesB$RelTime >= baselineperiod[1] & 
                            samplesB$RelTime <= baselineperiod[2])
    
    ## We don't need the absolute time. So call the time relative to trial onset 
    ## 'Time'. Likewise overwrite non-normalized Dilation with the normalized one.
    samples[, ':='(Time = RelTime, Dil = DilN)]
    
    ## Downsample data to outHz (as specified at the top). My fast_downsample()
    ## function is vectorized and therefore about 600x faster than
    ## wmR::my_downsample() and pR::downsample(). However, the input data.table
    ## needs a column called 'Trial'. If you don't have trials, simply add this
    ## column and make it all <1>.
    cat('\ndownsampling...')
    samples <- wmR::fast_downsample(samples,
                                    by = c(
                                      'Subject', 'Trial', 'startTime', 
                                      'endTime', 'RelTime','FixationOnset'),
                                    Hz = outHz)
    return(samples)
  }



################################################################################
######################## STEP 6: Store Dataset #################################
################################################################################
cat('\nsaving...')
save(file = paste0(OUTpath, OUTprefix, "_AllSubjectEyeData.Rdat"), samples)


################################################################################
######################## STEP 7: Create a copy with behavioral data only #######
################################################################################
## create a copy of samples without eyetracking information. That is just 
## behavioral data. This will save so much memory, we'll be able to run analyses
## on laptops...
cat('\nDownsampling for behavior only copy...')
samples <- samples[,.(Dil = mean(Dil, na.rm = T),
                      X = mean(X, na.rm = T),
                      Y = mean(Y, na.rm = T)),
                   by = .(Subject, Trial, startTime, RelTime, FixationOnset)]

cat('\nsaving...')
save(file = paste0(OUTpath, OUTprefix, "_AllSubjectBehavData.Rdat"), samples)
stopCluster(cl)
cat('\ndone!')