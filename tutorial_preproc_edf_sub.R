## trials that were aborted due to badgazes or badpresses will not contain all
## information (e.g., no info about a response because there's no response). 
## define a function to simply assign NA in those cases
ie <- function(type, ...) {
  if (type == 'd') {
    type = 'double'
  } else if (type == 'l') {
    type = 'logical'
  } else if (type == 'c') {
    type = 'character'
  } else if (type == 'i') {
    type = 'integer'
  }
  if (!exists(as.character(as.list(quote(...))))) {
    return(as(NA, type))
  } else {
    inp = list(...)
    return(inp[[1]])
  }
}

preprocess_edf <- function(fname, P, verbose = TRUE) {
  # check if there are more files for this subject (i.e., a session was interrupted)
  id = str_extract(fname, '(?<=ROSET)\\d+')
  extrafiles <- list.files(P$RDATpath, paste0('ROSE[A-S]', id), full.names = T)
  
  # load the current subject file:
  load(paste0(P$RDATpath, fname, ".Rdat"))
  
  if (length(extrafiles) > 0) {
    extrafiles <- str_sort(extrafiles)
    initdat <- curdat
    for (i in 1:length(extrafiles)) {
      load(extrafiles[i])
      # increase all extra files' time vectors by an arbitrary constant to avoid
      # conflicts
      curdat$samples$time = curdat$samples$time + i*1e15
      curdat$blinks$sttime = curdat$blinks$sttime + i*1e15
      curdat$blinks$entime = curdat$blinks$entime + i*1e15
      curdat$saccades$sttime = curdat$saccades$sttime + i*1e15
      curdat$saccades$entime = curdat$saccades$entime + i*1e15
      curdat$fixations$sttime = curdat$fixations$sttime + i*1e15
      curdat$fixations$entime = curdat$fixations$entime + i*1e15
      curdat$ttl$sttime = curdat$ttl$sttime + i*1e15
      curdat$messages$time = curdat$messages$time + i*1e15
      initdat$samples <- rbind(initdat$samples, curdat$samples)
      initdat$blink <- rbind(initdat$blinks, curdat$blinks)
      initdat$saccades <- rbind(initdat$saccades, curdat$saccades)
      initdat$fixations <- rbind(initdat$fixations, curdat$fixations)
      initdat$ttl <- rbind(initdat$ttl, curdat$ttl)
      initdat$messages <- rbind(initdat$messages, curdat$messages)
    }
    curdat <- initdat
    initdat <- NULL
  }
  
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
    stop("I cannot determine which Eye has been tracked\n") 
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
  foundNewTrl <- FALSE
  trial_end_reached <- FALSE
  if (verbose) {
    cat(sprintf('\nNow working on subject %s\n', fname))
    cat('parsing messages:\n')
    pb = txtProgressBar(0, nrow(curdat$messages), style = 3,
                        title = 'parsing messages')
  }
  tmp <- maketmp()
  for (i in 1:nrow(curdat$messages)) {
    if (verbose) {setTxtProgressBar(pb, i)}
    # store the current message in e
    e <- curdat$messages[i,]
    # split it by tabs
    msgSplit <- strsplit(e$msg,"\\\\t")[[1]]
    
    # our special informative messages have a common '>' as first character
    # skip iteration if not present
    if (!str_starts(msgSplit[1], '>')) {next}
    # we only get here if it's a special message. now we need to check which
    # one. That's usually written in the second argument. e.g., if you sent 
    # something like EyelinkSendTabMsg('TrlOn',ID,itrial) in Matlab, you'd be
    # looking for 'TrlOn'
    if (msgSplit[1] == '>trial_on') {
      # yay! we found the onset of a new trial
      numTrial  <- numTrial + 1
      # if (verbose) {
      #   cat("Working on trial ", numTrial, "\t of file:", fname,
      #       "\t on worker:", Sys.getpid(), "\n")
      # }
      # as the messages are already aligned with the eyetrackig data, we can
      # use the eyetrackers time vector to store the absolute time when this
      # message was sent.
      tmp$startT <- as.numeric(e$time)
      foundNewTrl <- TRUE
      tmp$badgaze <- FALSE
      tmp$badpress <- FALSE
    }
    
    ## once we found that starting message, we keep on looping, trying to find 
    ## more messages for the current trial.
    if (foundNewTrl) {
      if (msgSplit[1] == '>faulty_fix') {
        # subject couldn't fix in time
        numTrial  <- numTrial - 1
        foundNewTrl <- FALSE
      }
      if (msgSplit[1] == ">targets_on") {
        tmp$TarOn <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">targets_off") {
        tmp$TarOff <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">saccue_on") {
        tmp$SaccueOn <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">saccue_off") {
        tmp$SaccueOff <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">sacc_on") {
        tmp$SaccOn <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">probe_fixed") {
        tmp$probeFixed <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">probe_on") {
        tmp$probeOn <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">response") {
        tmp$RespOn <- as.numeric(e$time)
      }
      if (msgSplit[1] == ">badgaze") {
        tmp$badgaze_T <- as.numeric(e$time)
        tmp$badgaze <- TRUE
      }
      if (msgSplit[1] == ">badpress") {
        tmp$badpress_T <- as.numeric(e$time)
        tmp$badpress <- TRUE
      }
      
      ## The messages "TrialInfo", "probe_color_rgb", "target_color_rgb", &
      ## 'target_color_idx' are special. They contain tab-delimited info
      ## on the trial itself
      if (msgSplit[1] == ">TrialInfo") {
        tmp$InfoTime <- as.numeric(e$time)
        if (tmp$badgaze || tmp$badpress) {
          tmp$SubjName  <- msgSplit[4]
          tmp$SubjNr <- as.integer(str_extract(msgSplit[4], '(?<=ROSET)\\d+'))
          tmp$isFemale  <- msgSplit[5] == 'W'
          tmp$visioncorrected  <- as.logical(as.integer(msgSplit[6]))
          tmp$age  <- as.integer(msgSplit[7])
          tmp$DominantEyeIsRight  <- msgSplit[8] == 'R'
          tmp$trialNr <- as.integer(msgSplit[9])
          tmp$change <- as.numeric(msgSplit[10])
          tmp$changedeg <- as.numeric(msgSplit[11])
          tmp$probedUpperHalf <- msgSplit[12] == '1'
          cues <- c('L', 'C', 'R')
          tmp$saccue <- cues[as.integer(msgSplit[13])]
          cues <- c('MemU', 'MemR', 'MemD', 'MemL', 'MemV', 'MemH')
          tmp$tarcue <- cues[as.integer(msgSplit[14])]
          tmp$trialIdx  <- as.integer(msgSplit[15])
          tmp$T_trl_on <- as.numeric(msgSplit[16])
          trial_end_reached = TRUE
        } else {
          tmp$SubjName  <- msgSplit[2]
          tmp$SubjNr <- as.integer(str_extract(msgSplit[2], '(?<=ROSET)\\d+'))
          tmp$isFemale  <- msgSplit[3] == 'W'
          tmp$visioncorrected  <- as.logical(as.integer(msgSplit[4]))
          tmp$age  <- as.integer(msgSplit[5])
          tmp$DominantEyeIsRight  <- msgSplit[6] == 'R'
          tmp$trialNr <- as.integer(msgSplit[7])
          tmp$change <- as.numeric(msgSplit[8])
          tmp$changedeg <- as.numeric(msgSplit[9])
          tmp$probedUpperHalf <- msgSplit[10] == '1'
          tmp$respondedUpperHalf <- msgSplit[11] == '1'
          tmp$responseCorrect <- msgSplit[12] == '1'
          tmp$RT <- as.numeric(msgSplit[13])
          cues <- c('L', 'C', 'R')
          tmp$saccue <- cues[as.integer(msgSplit[14])]
          cues <- c('MemU', 'MemR', 'MemD', 'MemL', 'MemV', 'MemH')
          tmp$tarcue <- cues[as.integer(msgSplit[15])]
          tmp$threshEstim <- as.numeric(msgSplit[16])
          tmp$threshSD <- as.numeric(msgSplit[17])
          tmp$trialCompleted  <- as.logical(as.integer(msgSplit[18]))
          tmp$trialIdx  <- as.integer(msgSplit[19])
          tmp$T_trl_on <- as.numeric(msgSplit[20])
          tmp$T_probe_on <- as.numeric(msgSplit[21])
          tmp$T_sacue_off <- as.numeric(msgSplit[22])
          tmp$T_sacue_on <- as.numeric(msgSplit[23])
          tmp$T_targets_off <- as.numeric(msgSplit[24])
          tmp$T_targets_on <- as.numeric(msgSplit[25])
          tmp$T_fix_on <- as.numeric(msgSplit[26])
          tmp$sacc_rt <- as.numeric(msgSplit[27])
          tmp$T_sacc_on <- as.numeric(msgSplit[28])
          tmp$T_probe_fix_on <- as.numeric(msgSplit[29])
          tmp$fix_rt <- as.numeric(msgSplit[30])
          tmp$T_feedb_on <- as.numeric(msgSplit[31])
          tmp$T_feedb_off <- as.numeric(msgSplit[32])
          tmp$endT    <- as.numeric(e$time)
        }
      }
      if (msgSplit[1] == ">probe_color_rgb") { 
        tmp$upR <- as.numeric(msgSplit[2])
        tmp$upG <- as.numeric(msgSplit[3])
        tmp$upB <- as.numeric(msgSplit[4])
        tmp$downR <- as.numeric(msgSplit[5])
        tmp$downG <- as.numeric(msgSplit[6])
        tmp$downB <- as.numeric(msgSplit[7])
      }
      if (msgSplit[1] == ">target_color_rgb") {#pos, up/do, r/g/b, now upuprgb,updownrgb,rigturgb etc   
        tmp$UupR <- as.numeric(msgSplit[2])
        tmp$UupG <- as.numeric(msgSplit[3])
        tmp$UupB <- as.numeric(msgSplit[4])
        tmp$UdownR <- as.numeric(msgSplit[5])
        tmp$UdownG <- as.numeric(msgSplit[6])
        tmp$UdownB <- as.numeric(msgSplit[7])
        tmp$RupR <- as.numeric(msgSplit[8])
        tmp$RupG <- as.numeric(msgSplit[9])
        tmp$RupB <- as.numeric(msgSplit[10])
        tmp$RdownR <- as.numeric(msgSplit[11])
        tmp$RdownG <- as.numeric(msgSplit[12])
        tmp$RdownB <- as.numeric(msgSplit[13])
        tmp$DupR <- as.numeric(msgSplit[14])
        tmp$DupG <- as.numeric(msgSplit[15])
        tmp$DupB <- as.numeric(msgSplit[16])
        tmp$DdownR <- as.numeric(msgSplit[17])
        tmp$DdownG <- as.numeric(msgSplit[18])
        tmp$DdownB <- as.numeric(msgSplit[19])
        tmp$LupR <- as.numeric(msgSplit[20])
        tmp$LupG <- as.numeric(msgSplit[21])
        tmp$LupB <- as.numeric(msgSplit[22])
        tmp$LdownR <- as.numeric(msgSplit[23])
        tmp$LdownG <- as.numeric(msgSplit[24])
        tmp$LdownB <- as.numeric(msgSplit[25])
      }
      
      ## the "target_color_idx" message is the last message in a trial and 
      ## hence marks the end of a trial Because it marks the end of a trial, 
      ## whenever we catch this message we add all our temporary variables to 
      ## our output data.table
      if (msgSplit[1] == ">target_color_idx") {
        tmp$tarcolidxUu <- as.integer(msgSplit[2])
        tmp$tarcolidxRu <- as.integer(msgSplit[3])
        tmp$tarcolidxDu <- as.integer(msgSplit[4])
        tmp$tarcolidxLu <- as.integer(msgSplit[5])
        tmp$tarcolidxUd <- as.integer(msgSplit[6])
        tmp$tarcolidxRd <- as.integer(msgSplit[7])
        tmp$tarcolidxDd <- as.integer(msgSplit[8])
        tmp$tarcolidxLd <- as.integer(msgSplit[9])
        trial_end_reached = TRUE
      }
      # add information coded in messages in long format to our 1000Hz samples-DT
      # we now start & end time of the current trial in eyetracker-time,
      # so we assign all behavioral values to the respective part of the 
      # huge data.table
      if (trial_end_reached) {
        # when a session was aborted in between, we might have received messages 
        # but don't have data for the last trial before it aborted. in that case
        # add some rows
        if (nrow(samples[Time %between% list(tmp$startT, tmp$InfoTime),] ) == 0){
          tmpsamples <- data.table(Time = tmp$startT:tmp$InfoTime)
          samples <- merge(samples, tmpsamples, by = 'Time', all = T)
        }  
        samples[Time %between% list(tmp$startT, tmp$InfoTime), ':='(
          Subject = tmp$SubjName,
          ID = tmp$SubjNr,
          isFemale = tmp$isFemale,
          visioncorrected = tmp$visioncorrected,
          age = tmp$age,
          DominantEyeIsRight = tmp$DominantEyeIsRight,
          Trial = tmp$trialNr,
          change = tmp$change,
          changedeg = tmp$changedeg,
          probedUpperHalf = tmp$probedUpperHalf,
          respondedUpperHalf = tmp$respondedUpperHalf,
          responseCorrect = tmp$responseCorrect,
          RT = tmp$RT,
          saccadecue = tmp$saccue,
          targetcue = tmp$tarcue,
          Threshold_Estimate = tmp$threshEstim,
          Threshold_SD = tmp$threshSD,
          Trial_Completed = tmp$trialCompleted,
          Trial_idx = tmp$trialIdx,
          T_trial_on = tmp$T_trl_on,
          T_probe_on = tmp$T_probe_on,
          T_sacue_off = tmp$T_sacue_off,
          T_sacue_on = tmp$T_sacue_on,
          T_targets_off = tmp$T_targets_off,
          T_targets_on = tmp$T_targets_on,
          T_fix_on = tmp$T_fix_on,
          Saccadic_RT = tmp$sacc_rt,
          T_sacc_on = tmp$T_sacc_on,
          T_probe_fixation_on = tmp$T_probe_fix_on,
          Fixation_RT = tmp$fix_rt,
          T_feedback_on = tmp$T_feedb_on,
          T_feedback_off = tmp$T_feedb_off,
          Probe_col_up_R = tmp$upR,
          Probe_col_up_G = tmp$upG,
          Probe_col_up_B = tmp$upB,
          Probe_col_down_R = tmp$downR,
          Probe_col_down_G = tmp$downG,
          Probe_col_down_B = tmp$downB,
          Target_col_Up_up_R = tmp$UupR,
          Target_col_Up_up_G = tmp$UupG,
          Target_col_Up_up_B = tmp$UupB,
          Target_col_Up_down_R = tmp$UdownR,
          Target_col_Up_down_G = tmp$UdownG,
          Target_col_Up_down_B = tmp$UdownB,
          Target_col_R_up_R = tmp$RupR,
          Target_col_R_up_G = tmp$RupG,
          Target_col_R_up_B = tmp$RupB,
          Target_col_R_down_R = tmp$RdownR,
          Target_col_R_down_G = tmp$RdownG,
          Target_col_R_down_B = tmp$RdownB,
          Target_col_Down_up_R = tmp$DupR,
          Target_col_Down_up_G = tmp$DupG,
          Target_col_Down_up_B = tmp$DupB,
          Target_col_Down_down_R = tmp$DdownR,
          Target_col_Down_down_G = tmp$DdownG,
          Target_col_Down_down_B = tmp$DdownB,
          Target_col_L_up_R = tmp$LupR,
          Target_col_L_up_G = tmp$LupG,
          Target_col_L_up_B = tmp$LupB,
          Target_col_L_down_R = tmp$LdownR,
          Target_col_L_down_G = tmp$LdownG,
          Target_col_L_down_B = tmp$LdownB,
          Target_col_deg_Up_up = tmp$tarcolidxUu,
          Target_col_deg_R_up = tmp$tarcolidxRu,
          Target_col_deg_Down_up = tmp$tarcolidxDu,
          Target_col_deg_L_up = tmp$tarcolidxLu,
          Target_col_deg_Up_down = tmp$tarcolidxUd,
          Target_col_deg_R_down = tmp$tarcolidxRd,
          Target_col_deg_Down_down = tmp$tarcolidxDd,
          Target_col_deg_L_down = tmp$tarcolidxLd,
          ET_T_trial_on = tmp$startT - tmp$TarOn,
          ET_T_Target_on = tmp$TarOn - tmp$TarOn,
          ET_T_Target_off = tmp$TarOff - tmp$TarOn,
          ET_T_Saccadecue_on = tmp$SaccueOn - tmp$TarOn,
          ET_T_Saccadecue_off = tmp$SaccueOff - tmp$TarOn,
          ET_T_Saccade_on = tmp$SaccOn - tmp$TarOn,
          ET_T_Probe_fixation_on = tmp$probeFixed - tmp$TarOn,
          ET_T_Probe_on = tmp$probeOn - tmp$TarOn,
          ET_T_Response = tmp$RespOn - tmp$TarOn,
          ET_T_InfoTime = tmp$InfoTime - tmp$TarOn,
          RelTime = Time - tmp$TarOn
        )]
        # We set all values back to NA after a trial to avoid writinbg data from
        # a previous trial into the next one
        rm(tmp)
        tmp <- maketmp()
        rm('msgSplit')
        # we're done with the trial, so set this to false again, until we find
        # another trial start
        foundNewTrl = FALSE
        trial_end_reached = FALSE
      }
    }
  }
  ############################################################################
  ######################## STEP 5: Massage Eyetracking Data ##################
  ############################################################################
  ## add mean and peak velocity per saccade
  # set keys, so data.table can do a fast vector scan
  setkey(samples, Time)
  if (verbose) cat("\nAdding saccade velocity to samples Data.Table\n")
  saccades <- as.data.table(curdat$saccades)
  setkey(saccades, sttime, entime)
  
  # extract only those saccades that happened during a trial
  saccades <- saccades[sttime %in% samples[!is.na(ID), Time] | 
                         entime %in% samples[!is.na(ID), Time],]
  # melt the saccades data.table, so we have a column "Time", matching the one 
  # of the samples data.table
  saccades <- melt(saccades, measure.vars = c('sttime', 'entime'),
                   variable.name = 'StartEnd', value.name = 'Time')
  # enumerate the saccades
  saccades[StartEnd == 'sttime', IthSaccadeThisSubject :=  .I]
  # we don't need the endtime information, it's already included in samples' 
  # "Saccade" column
  saccades <- saccades[StartEnd == 'sttime']
  # remove all redundant columns
  set(saccades, j = c('gstx', 'gsty', 'genx', 'geny', 'StartEnd'), value = NULL)
  names(saccades) <- c(
    'AverageVelocity', 'PeakVelocity', 'Time', 'IthSaccadeThisSubject')
  # merge with samples
  cols <- c('AverageVelocity', 'PeakVelocity', 'IthSaccadeThisSubject')
  samples <- merge(samples, saccades, by = 'Time', all.x = TRUE)
  # fill NAs with "last observation carried forward"
  samples[Saccade == 1,
          (cols) := lapply(.SD, nafill, type = "locf"), .SDcols = cols]
  
  ## add TTL to long format
  if (verbose) cat("Adding TTL triggers to samples data.table\n")
  curdat$ttl <- as.data.table(curdat$ttl)
  names(curdat$ttl) <- c('Time', 'TTL')
  samples = merge(samples, curdat$ttl, all.x = TRUE)
  
  ## remove temporary variables
  rm('curdat', 'saccades')
  
  ## Remove the samples that weren't between a TrialStart and a TrialEnd 
  ## message (and hence don't have a subject-Nr). Conserves RAM & HDD
  setkey(samples, ID, Trial, Time)
  samples <- na.omit(samples, cols = 'ID')
  
  if (verbose) {cat('Detecting and expanding blinks\n')}
  samplesB <- blink.detect(samples, SR_Blink_Col = 'Blink',
                           use_SR = TRUE, verbose = verbose, 
                           expandblinks = 0)
  if (verbose) {
    cat('Performing cubic spline interpolation for dilation during blinks\n')
  }
  samplesB <- blink.interpolate(samplesB)
  if (verbose) {
    cat('Baseline correction of pupil dilation data [baseline-period:',
        P$baselineperiod[1], '-', P$baselineperiod[2], 'ms]\n')
  }
  samples  <- normalize(samplesB, samplesB$RelTime >= P$baselineperiod[1] & 
                          samplesB$RelTime <= P$baselineperiod[2])
  
  ## We don't need the absolute time. So call the time relative to trial onset 
  ## 'Time'. Likewise overwrite non-normalized Dilation with the normalized one.
  samples[, ':='(Time = RelTime, Dil = DilN)]
  
  ## Downsample data to outHz (as specified at the top). My fast_downsample()
  ## function is vectorized and therefore about 600x faster than
  ## wmR::my_downsample() and pR::downsample(). However, the input data.table
  ## needs a column called 'Trial'. If you don't have trials, simply add this
  ## column and make it all <1>.
  if (verbose) {cat('downsampling...\n')}
  
  BY = colnames(samples)
  BY = BY[!(BY %in% c('Dil', 'DilN', 'X', 'Y', 'Flags', 'Blink', 'DilDiff',
                      'Fixation', 'Saccade', 'TTL', 'RelTime', 'Baseline',
                      'IthSaccadeThisSubject', 'AverageVelocity',
                      'PeakVelocity', 'Time','Subject'))]
  samples <- fast_downsample(samples, by = BY, Hz = P$outHz, useref = TRUE)
  return(samples)
}

maketmp <- function(){
  return(data.table(startT = NA_real_, badgaze = NA, badpress = NA,
                   TarOn = NA_real_, TarOff = NA_real_, SaccueOn = NA_real_,
                   SaccueOff = NA_real_, SaccOn = NA_real_,
                   probeFixed = NA_real_, probeOn = NA_real_, RespOn = NA_real_,
                   badgaze_T = NA_real_, badgaze = NA, badpress_T = NA_real_,
                   badpress = NA, InfoTime = NA_real_, SubjName= NA_character_,
                   SubjNr = NA_integer_, isFemale  = NA, visioncorrected  = NA,
                   age  = NA_integer_, DominantEyeIsRight  = NA,
                   trialNr = NA_integer_, change = NA_real_,
                   changedeg = NA_real_, probedUpperHalf = NA,
                   respondedUpperHalf = NA, responseCorrect = NA,
                   RT = NA_real_, saccue = NA_integer_, tarcue = NA_integer_,
                   threshEstim = NA_real_, threshSD = NA_real_,
                   trialCompleted  = NA, trialIdx  = NA_integer_,
                   T_trl_on = NA_real_, T_probe_on = NA_real_, 
                   T_sacue_off = NA_real_, T_sacue_on = NA_real_,
                   T_targets_off = NA_real_, T_targets_on = NA_real_, 
                   T_fix_on = NA_real_, sacc_rt = NA_real_, T_sacc_on = NA_real_,
                   T_probe_fix_on = NA_real_, fix_rt = NA_real_, 
                   T_feedb_on = NA_real_, T_feedb_off = NA_real_,
                   endT = NA_real_, upR = NA_real_, upG = NA_real_,
                   upB = NA_real_, downR = NA_real_, downG = NA_real_,
                   downB = NA_real_, UupR = NA_real_, UupG = NA_real_,
                   UupB = NA_real_, UdownR = NA_real_, UdownG = NA_real_,
                   UdownB = NA_real_, RupR = NA_real_, RupG = NA_real_,
                   RupB = NA_real_, RdownR = NA_real_, RdownG = NA_real_,
                   RdownB = NA_real_, DupR = NA_real_, DupG = NA_real_,
                   DupB = NA_real_, DdownR = NA_real_, DdownG = NA_real_,
                   DdownB = NA_real_, LupR = NA_real_, LupG = NA_real_,
                   LupB = NA_real_, LdownR = NA_real_, LdownG = NA_real_,
                   LdownB = NA_real_, tarcolidxUu = NA_integer_,
                   tarcolidxRu = NA_integer_, tarcolidxDu = NA_integer_,
                   tarcolidxLu = NA_integer_, tarcolidxUd = NA_integer_,
                   tarcolidxRd = NA_integer_, tarcolidxDd = NA_integer_,
                   tarcolidxLd = NA_integer_))
}
