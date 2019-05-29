## Parses Eyetracking + behavioral data for analysis in R.
## Only works on Linux. Check dependencies of edfR online.
## author: Wanja Moessing, April, 2019, moessing@wwu.de

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


################################################################################
######################## STEP 2: Preparation & Parameters ######################
################################################################################
## load libraries
wmR::libraries(data.table, hvR, pR, doParallel, edfR, stringr, wmR)

## Define baselineperiod & Hz for downsampling
# Below, you can define what the variable 'Time' actually means (i.e., what is zero?)
# (e.g., trial onset). So the numbers here always depend on whatever you write 
# below. Look for function 'normalize' ~ line 299.
P = list()
P$baselineperiod <- c(-100, 0)
P$outHz          <- 100
P$runinpar       <- FALSE
P$verbosity      <- TRUE

## Specify in- and output
P$EDFpath     <- "EDF/" # adjust this to the path where your .edf files are stored
P$RDATpath    <- "EDF/Rdat/" # output directory for temporary parsed files
P$OUTpath     <- "Eyedata/" # output directory
P$EDFpattern  <- "ROSE\\w\\d+.[edf|EDF]" # pattern used to match your filenames (see https://github.com/rstudio/cheatsheets/raw/master/strings.pdf)
P$RDATpattern <- "ROSE\\w\\d+.Rdat"
P$RDAT_base_pattern <- "ROSET\\d+.Rdat"
P$OUTprefix   <- "ROSA_Exp1_EYE" 

# create output folders
if (!dir.exists(P$RDATpath)) {dir.create(P$RDATpath)}
if (!dir.exists(P$OUTpath)) {dir.create(P$OUTpath)}


## Extracting behavioral data is always the same for all subjects. That is, this 
## task is "embarassingly parallel". You can activate these lines to run things
## in parallel, once you have many subjects to process.
## 
## Start workers (i.e., R-Sessions) on other cores & load packages in these sessions.
if (P$runinpar) {
  if (computername() == 'wanjas-yoga-2-pro') {
    ncores <- 2
  } else if (computername() == 'LABSERVER1') {
    ncores <- 20
  }
  cl <- makeCluster(ncores, outfile = "")
  registerDoParallel(cl)
  `%runlikethis%` <- `%dopar%`
  parprogress <- function(n) setTxtProgressBar(parpb, n)
  opts <- list(progress = parprogress)
} else {
  `%runlikethis%` <- `%do%`
}

################################################################################
######################## STEP 3: Parse EDF files ###############################
################################################################################
## Check the directories from above for existing edf files and also for existing
## Rdat files. We check for Edat files as well, to avod parsing already parsed 
## files again.
edfs  <- str_replace_all(list.files(path = P$EDFpath, pattern = P$EDFpattern),
                         '.edf', '')
Rdats <- str_replace_all(list.files(path = P$RDATpath, pattern = P$RDATpattern),
                         '.Rdat', '')


## convert EDFs that haven't been processed yet to Rdats. This step is Linux only!
# compare existing edfs with existing Rdats
unprocessed_edfs <- edfs[!(edfs %in% Rdats)]

if (length(unprocessed_edfs) > 0) {
  # run a parallel for loop across remaining edfs. tip: you can change %dopar% to %do% to make this a regular for-loop
  foreach(fname = unprocessed_edfs, .packages = c('data.table','edfR')) %runlikethis% {
    curfile              <- paste0(P$EDFpath, fname, '.edf')
    
    # edf.all parses 'all' possible data.types in the file. You can also just
    # parse parts of it. Usually that's not gonna be necessary, though.
    curdat               <- edf.all(curfile, samples = TRUE, eventmask = TRUE)
    
    # The file header contains some meta-info (tracked eye, Hz, etc). get that:
    curdat$recordingInfo <- edf.recordings(curfile)
    curdat$recordingInfo <- curdat$recordingInfo[1,]  # we only need the info once
    
    # If you did EEG as well, you might want to also parse the TTL triggers.
    curdat$ttl           <- edf.events(curfile, type = "INPUTEVENT", 
                                       fields = c("sttime","input"))
    save(file = paste0(P$RDATpath, fname, ".Rdat"), curdat)
  }
  
  # if we preprocessed some new files, check what Rdat files do exist now
  Rdats <- str_replace_all(list.files(path = P$RDATpath, pattern = P$RDATpattern),
                           '.Rdat', '')
} else {
  cat("Files seem to be converted already!")
}

Rdats <- str_replace_all(list.files(path = P$RDATpath, pattern = P$RDAT_base_pattern),
                         '.Rdat', '')
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
source('Analysis/EYE/ROSA_preproc_edf_sub.R')
# Using lapply is internally parallelizing similarly well as foreach and memory
# allocation is *much* better. Plus, we can get better status messages.
samples <- rbindlist(lapply(Rdats, preprocess_edf, P = P, verbose = P$verbosity))
cat('\nmerging subjects done!')

################################################################################
######################## STEP 6: Store Dataset #################################
################################################################################
cat('\nsaving...')
save(file = paste0(P$OUTpath, P$OUTprefix, "_AllSubjectEyeData.Rdat"), samples)


################################################################################
######################## STEP 7: Create a copy with behavioral data only #######
################################################################################
## create a copy of samples without eyetracking information. That is just 
## behavioral data. This will save so much memory, we'll be able to run analyses
## on laptops...
cat('\nDownsampling for behavior only copy...')
cols <- names(samples)
uniquecols <- cols[!(cols %in% c('TTL', 'IthSaccadeThisSubject', 'IthBlink',
                                 'Fixation', 'Saccade', 'AverageVelocity',
                                 'PeakVelocity', 'Time', 'Dil', 'ID', 'Trial',
                                 'Blink','X','Y'))]
samples <- samples[, lapply(.SD, unique), by = .(ID, Trial),
                   .SDcols = uniquecols]

cat('\nsaving...')
save(file = paste0(P$OUTpath, P$OUTprefix, "_AllSubjectBehavData.Rdat"), samples)
stopCluster(cl)
cat('\ndone!')
