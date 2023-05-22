
# Want to clear project and start over? 
#rm(list = ls())

# Setup: 

# Load packages
load_packages <- function() {
  
  if(!require(rgl)) {
    install.packages("rgl")
    library(rgl)
  }
  if(!require(hms)) {
    install.packages("hms")
    library(hms)
  }
  # if(!require(ggplot2)) {
  #   install.packages("ggplot2")
  #   library(ggplot2)
  # }
  # if(!require(ggpubr)) {
  #   install.packages("ggpubr")
  #   library(ggpubr)
  # }
  # if(!require(tidyverse)) {
  #   install.packages("tidyverse")
  #   library(tidyverse)
  # }
  if(!require(readr)) {
    install.packages("readr")
    library(readr)
  }
  if(!require(dtw)) {
    install.packages("dtw")
    library(dtw)
  }
  if (!require(rstatix)) {
    install.packages("rstatix")
    library(rstatix)
  }

}
load_packages()

# Basic parameters of data set:
load_dataset_parameters <- function() {
  
  last_no_feedback_trial_num <<- 25 # first how many reaches are random motion with no auditory feedback?
  significant_digitsOT <<- 1 # raw data in meters and will be imported as cm, so this is down to the mm
  significant_digitsSU <<- 5 # SU returns floats, 5 ensures all digits are significant
  reach_min_length <<- 400 # SU recordings below this number of samples are likely twitches, not real reaches
  overrun_length <<- 1400 # SU only records 1400 samples, so if this number is recorded, 
                          # participant likely failed to pause at end of reach, and reach
                          # end needs to be estimated.
  first_postlearning_feedback_trial_num <<- 51 # first post-learning trial number
  
}
load_dataset_parameters()

# Import data
motionSU <- read_csv("reach-study-3-SUdata.csv", 
                     col_types = cols(time = col_time(format = "%H:%M:%OS"), 
                                      .default = col_double()))
motionOT <- read_csv("reach-study-3-OTdata.csv", 
                     col_types = cols(.default = col_double()))
pTable <- read_csv("reach-study-3-participant-table.csv", 
                   col_types = cols(Condition = col_character(), 
                                    SUStartTime = col_time(format = "%H:%M:%OS"),
                                    .default = col_double()))

# Dependency Functions

# Misc Functions
vdist <- function(a,b) sqrt(sum((a - b)^2)) 
rolling_mean <- function(v, n, roundto) {
  
  if (n >= length(v)) {
    stop("n must be less than the length of v")
  }
  
  output <- rep(NA, length(v))
  
  for ( i in 1:n ) {
    output[i] <- mean(v[1:i],na.rm=TRUE)
    if ( is.nan(output[i]) ) output[i] <- NA
  }
  
  for (i in (n+1):length(v)) {
    output[i] <- mean(v[(i-n):(i-1)],na.rm=TRUE)
    if ( is.nan(output[i]) ) output[i] <- NA
  }
  
  output <- round(output,roundto)
  
  return(output)
  
}
index_outliers <- function(x,a) {
  
  # IQR method
  
  #x2 <- x[which(!is.na(x))]
  #x2s <- summary(x2)
  #IQRx2 <- x2s[5] - x2s[2] #IQR(x2)
  
  #lower <- x2s[2] - (1.5 * IQRx2)
  #upper <- x2s[5] + (1.5 * IQRx2)
  
  # Mean & SD
  
  m <- mean(x,na.rm = TRUE)
  sdv <- sd(x,na.rm = TRUE)
  
  lower <- m - ( a * sdv )
  upper <- m + ( a * sdv )
  
  index <- which(x < lower | x > upper)
  
  return(index)
  
}
# For DTW
DTW_model_indexesOT_Vel <- function(p,r) {
  
  m_num <- pTable$model[which(pTable$number==p)]
  r1i <- which(motionOT$participant==p & motionOT$reach==r)
  rv <- motionOT$SpatialVelocity[r1i]
  mi <- which(motionOT$participant==p & motionOT$reach==m_num)
  mv <- motionOT$SpatialVelocity[mi]
  
  rv <- na.omit(rv)
  mv <- na.omit(mv)
  
  d <- dtw(mv,rv)
  
  # the reach velocity series is y (reference), the model velocity series is x (query).
  
  return(d)
  
} # For OptiTrack data: takes a participant and reach num 
                                               # and outputs the DTW structure of that reach (reference)
                                               # warped by velocity to the model (query).
DTW_model_indexesSU_Vel <- function(p,r) {
  
  m_num <- pTable$model[which(pTable$number==p)]
  r1i <- which(motionSU$participant==p & motionSU$reach==r)
  rv <- motionSU$RotVelocity[r1i]
  mi <- which(motionSU$participant==p & motionSU$reach==m_num)
  mv <- motionSU$RotVelocity[mi]
  
  rv <- na.omit(rv)
  mv <- na.omit(mv)
  
  d <- dtw(mv,rv)
  
  # the reach velocity series is y (reference), the model velocity series is x (query).
  
  return(d)
  
} # ibid, for Son System data
# Used in chop_data: chop raw OptiTrack data into discrete reaches using Son Unit data;
# function cannot be run on its own; must be run inside chop_data
ParticipantChopOT <- function(noisy_print, # not so noisy
                              model, # reach number of the model
                              OTsampleslide, # initial guess at OT/SU sync, in OT samples
                              NOWtime, # time from SU of button press sync
                              ppp, # participant number
                              IPrun, # how many OT samples at begining of OT run should be averaged to estimate IP? 
                              float_cutoff, # after how far from OT data IP is a reach IP considered too far away? in meters
                              psv # participant summary vector, returned at end of function; gives info about the chopping
) {
  
  # Begin chopping
  
  psv <- c( psv, ppp ) # add participant number to psv vector
  
  # SU = Sonification system, OT = OptiTrack data
  
  SUreaches <- motionSU$reach[which(motionSU$participant==ppp)]
  SUtimesRAW <- motionSU$time[which(motionSU$participant==ppp)]
  
  SUtimes <- rep(NA,length(SUtimesRAW))
  
  # Raw time stamps from SU are in HH:MM:SS, need to convert into seconds from the sync prompt
  for ( i in 2:length(SUtimes) ) {
    SUtimes[i] <- difftime( SUtimesRAW[i], as_hms(NOWtime), units="secs")
  }
  SUtimes[1] <- SUtimes[2]
  
  # Code should check all steps for unexpected values
  if ( any(is.na(SUtimes)) ) {
    cat("\n WARNING! Unexpected NA in SUtimes, participant:", ppp,"\n")
  }
  
  # Go through SU data (motionSU) and find the rows where a new reach starts
  SUstarts <- 1
  for ( i in 1:(length(SUreaches)-1) ) {
    if (is.na(SUreaches[i]) || is.na(SUreaches[i+1]) ) {
      cat("\n WARNING! Unexpected NA in SUreaches, participant:", ppp, "index:", i,"\n")
    }
    if ( SUreaches[i] != SUreaches[i+1] ) {
      SUstarts <- c( SUstarts, i+1 )
    }
  }
  SUnumofreaches <- length(SUstarts)
  if ( length(unique(SUreaches)) != SUnumofreaches || 
       SUnumofreaches != max(SUreaches,na.rm=TRUE) ) {
    cat("\n WARNING! Something went wrong counting reaches for particippant:", ppp,"\n")
  }
  if (noisy_print) cat(", R: ", SUnumofreaches) 
  psv <- c( psv, SUnumofreaches )
  
  # Go through SU data (motionSU) and compute the rows where each reach ends
  SUends <- rep(NA,SUnumofreaches)
  for ( i in 1:(length(SUends)-1) ) {
    SUends[i] <- SUstarts[i+1] - 1
  }
  SUends[length(SUends)] <- length(SUreaches)
  if ( any(is.na(SUends)) ) {
    cat("\n WARNING! Unexpected NA in SUends, participant:", ppp,"\n")
  }
  
  # Compute lengths of reaches in SU data (rows)
  SUlengths <- rep(NA,SUnumofreaches)
  for ( i in 1:length(SUends) ) {
    SUlengths[i] <- (SUends[i] - SUstarts[i]) + 1
  }
  if ( any(is.na(SUlengths)) ) {
    cat("\n WARNING! Unexpected NA in SUlengths, participant:", ppp,"\n")
  }
  
  # check for bad samples in SU data.
  SUqax <- motionSU$qax[which(motionSU$participant==ppp)]
  SUqay <- motionSU$qay[which(motionSU$participant==ppp)]
  SUqaz <- motionSU$qaz[which(motionSU$participant==ppp)]
  SUqar <- motionSU$qar[which(motionSU$participant==ppp)]
  SUqbx <- motionSU$qbx[which(motionSU$participant==ppp)]
  SUqby <- motionSU$qby[which(motionSU$participant==ppp)]
  SUqbz <- motionSU$qbz[which(motionSU$participant==ppp)]
  SUqbr <- motionSU$qbr[which(motionSU$participant==ppp)]
  bad_SU_samples <- NA
  if ( any(is.na(SUqax)) || any(!is.numeric(SUqax)) ) {
    problem <- which(is.na(SUqax) | !is.numeric(SUqax) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqay)) || any(!is.numeric(SUqay)) ) {
    problem <- which(is.na(SUqay) | !is.numeric(SUqay) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqaz)) || any(!is.numeric(SUqaz)) ) {
    problem <- which(is.na(SUqaz) | !is.numeric(SUqaz) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqar)) || any(!is.numeric(SUqar)) ) {
    problem <- which(is.na(SUqar) | !is.numeric(SUqar) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqbx)) || any(!is.numeric(SUqbx)) ) {
    problem <- which(is.na(SUqbx) | !is.numeric(SUqbx) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqby)) || any(!is.numeric(SUqby)) ) {
    problem <- which(is.na(SUqby) | !is.numeric(SUqby) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqbz)) || any(!is.numeric(SUqbz)) ) {
    problem <- which(is.na(SUqbz) | !is.numeric(SUqbz) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( any(is.na(SUqbr)) || any(!is.numeric(SUqbr)) ) {
    problem <- which(is.na(SUqbr) | !is.numeric(SUqbr) )
    bad_SU_samples <- c( bad_SU_samples, problem )
  }
  if ( length(bad_SU_samples) > 1 ) {
    bad_SU_samples <- bad_SU_samples[2:length(bad_SU_samples)]
    bad_SU_samples <- unique(bad_SU_samples)
    if (noisy_print) cat(", BSUSs: ", length(bad_SU_samples))
    psv <- c( psv, length(bad_SU_samples) )
  } else {
    if (noisy_print) cat(", BSUSs: ", 0)
    psv <- c( psv, 0 )
  }
  
  # Find velocities in SU data
  SUvelocity <- rep(NA,length(SUreaches))
  for ( i in 1:SUnumofreaches ) {
    for ( j in (SUstarts[i]+1):SUends[i] ) {
      pa2 <- c( SUqax[j], SUqay[j], SUqaz[j], SUqar[j] )
      pb2 <- c( SUqbx[j], SUqby[j], SUqbz[j], SUqbr[j] )
      pa1 <- c( SUqax[j-1], SUqay[j-1], SUqaz[j-1], SUqar[j-1] )
      pb1 <- c( SUqbx[j-1], SUqby[j-1], SUqbz[j-1], SUqbr[j-1] )
      SUvelocity[j] <- vdist(pa1,pa2) + vdist(pb1,pb2) * 1000 # put into qu/s
      SUvelocity[j] <- round(SUvelocity[j],significant_digitsSU)
    }
    firstvel <- SUvelocity[SUstarts[i]+1] - abs(SUvelocity[SUstarts[i]+2] - SUvelocity[SUstarts[i]+1])
    if ( firstvel > 0 ) {
      SUvelocity[SUstarts[i]] <- round(firstvel,significant_digitsSU)
    } else {
      SUvelocity[SUstarts[i]] <- 0
    }
  }
  
  # This vector is gives rows where we expect bad SU velocity samples
  bad_SU_samplesV <- c(bad_SU_samples, bad_SU_samples+1)
  # Now check to make sure there are no other bad SU velocity samples
  if ( any(is.na(SUvelocity)) ) {
    problem <- which(is.na(SUvelocity)) 
    if ( !all(problem %in% bad_SU_samplesV) ) {
      cat("\n WARNING! Unexpected NA in SUvelocity, participant:", ppp,"samples:",problem,"known bad SU samples:",bad_SU_samples,"\n")
    }
  }
  if ( any(!is.numeric(SUvelocity)) ) {
    problem <- which(!(is.numeric(SUvelocity)))
    if ( !all(problem %in% bad_SU_samplesV) ) {
      cat("\n WARNING! At least one nonnumeric entry in SUvelocity, participant:", ppp,"sample:",problem,"known bad SU samples:",bad_SU_samples,"\n")
    }
  }
  
  # First, smooth SU velocity data w/ 50ms rolling average
  SUvelocity <- rolling_mean(SUvelocity,50,significant_digitsSU)
  # Save velocity to dataframe; This puts SU velocity in unit quats / ms
  motionSU$RotVelocity[which(motionSU$participant==ppp)] <<- SUvelocity
  
  # Find and fix overruns in SU data.
  overrun_count <- 0
  if ( length(SUlengths) != length(SUends) ) {
    cat("\n WARNING! about to check for overruns, but lengths of SUlengths and SUends doesn't match, participant:", ppp,"\n")
  }
  for ( i in 1:length(SUlengths) ) {
    if ( SUlengths[i] == overrun_length ) {
      # User most likely never paused, so, we're looking for the inflection point where they turned around.
      # Even if user really did overrun just from going very slow or far, min velocity should still be near the end.
      oldend <- SUends[i]
      SUends[i] <- (SUstarts[i]+499) + which.min(SUvelocity[(SUstarts[i]+500):SUends[i]])
      if ( !is.numeric(SUends[i]) || is.na(SUends[i]) ) {
        cat("\n WARNING! attempting to fix overrun, but new SUends is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
      }
      SUlengths[i] <- (SUends[i] - SUstarts[i]) + 1
      if ( !is.numeric(SUlengths[i]) || is.na(SUlengths[i]) ) {
        cat("\n WARNING! attempting to fix overrun, but new SUlengths is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
      }
      overrun_count <- overrun_count+1
      if ( oldend > SUends[i] ) {
        SUreaches[(SUends[i]+1):oldend] <- NA
      }
    }
  }
  if (noisy_print) cat(", ORs: ", overrun_count) 
  psv <- c( psv, overrun_count )
  
  # Find and remove short reaches in SU data.
  short_reaches <- NA
  if ( length(short_reaches) != 1 ) {
    cat("\n WARNING! expected short_reach length of 1 at this point, but found otherwise, participant:", ppp,"\n")
  }
  for ( i in 1:length(SUlengths) ) {
    if ( SUlengths[i] < reach_min_length ) {
      short_reaches <- c( short_reaches, i )
      SUreaches[SUstarts[i]:SUends[i]] <- NA
      SUstarts[i] <- NA
      SUends[i] <- NA
      SUlengths[i] <- NA
    }
  }
  num_of_short_reaches <- 1
  if ( length(short_reaches) == 1 ) {
    num_of_short_reaches <- 0 
  } else {
    short_reaches <- short_reaches[2:length(short_reaches)] 
    num_of_short_reaches <- length(short_reaches)
  }
  if (noisy_print) cat(", Ss: ", num_of_short_reaches) 
  psv <- c( psv, num_of_short_reaches )
  
  # Find reach endpoints in OT data (initial guess).
  OTtimes <- motionOT$time[which(motionOT$participant==ppp)]
  if ( any(is.na(OTtimes)) ) {
    cat("\n WARNING! Unexpected NA in OTtimes, participant:", ppp,"\n")
  }
  OTends <- rep(NA,SUnumofreaches)
  for ( i in 1:last_no_feedback_trial_num ) {
    if ( !(i %in% short_reaches) ) {
      tSU <- SUtimes[SUstarts[i]] # the reach ended at the time the print out started
      if ( !is.numeric(tSU) || is.na(tSU) ) {
        cat("\n WARNING! attempting to find OTends, but tSU is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
      }
      SEARCHING <- TRUE
      for ( j in 1:length(OTtimes) ) {
        if ( SEARCHING == TRUE && OTtimes[j] > tSU ) {
          OTends[i] <- (j-1) + OTsampleslide
          if ( !is.numeric(OTends[i]) || is.na(OTends[i]) ) {
            cat("\n WARNING! attempting to set OTends, but OTends is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
          }
          SEARCHING <- FALSE
        }
      }
    }
  }
  for ( i in (last_no_feedback_trial_num+1):SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      tSU <- ( SUtimes[SUstarts[i]] - ( 0.5 + (SUlengths[model]/1000) ) ) # the reach ended at the time the print out started, minus 0.5 seconds and the time of the model
      if ( !is.numeric(tSU) || is.na(tSU) ) {
        cat("\n WARNING! attempting to find OTends, but tSU is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
      }
      SEARCHING <- TRUE
      for ( j in 1:length(OTtimes) ) {
        if ( SEARCHING == TRUE && OTtimes[j] > tSU ) {
          OTends[i] <- (j-1) + OTsampleslide
          if ( !is.numeric(OTends[i]) || is.na(OTends[i]) ) {
            cat("\n WARNING! attempting to set OTends, but OTends is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
          }
          SEARCHING <- FALSE
        }
      }
    }
  }
  
  # Find reach start points in OT data (initial guess).
  OTstarts <- rep(NA,SUnumofreaches)
  for ( i in 1:SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      starttime <- OTtimes[OTends[i]] - ( SUlengths[i] / 1000 )
      if ( !is.numeric(starttime) || is.na(starttime) ) {
        cat("\n WARNING! attempting to find OTstarts, but starttime is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
      }
      if (length(starttime) != 1 ) {
        cat("\n WARNING! attempting to find OTstarts, but starttime is not of length 1, participant:", ppp, "reach:", i,"\n")
      }
      SEARCHING <- TRUE
      for ( j in 1:length(OTtimes) ) {
        if ( SEARCHING == TRUE && OTtimes[j] > starttime ) {
          OTstarts[i] <- j-1
          if ( !is.numeric(OTstarts[i]) || is.na(OTstarts[i]) ) {
            cat("\n WARNING! attempting to set OTstarts, but OTstarts is not numeric or is NA, participant:", ppp, "reach:", i,"\n")
          }
          SEARCHING <- FALSE
        }
      }
    }
  }
  
  # Refine start points (and so endpoints too, length is fixed) in OT data 
  #  by moving them back until doing so more no longer gets us closer to the true initial position.
  tx <- motionOT$x[which(motionOT$participant==ppp)]
  ty <- motionOT$y[which(motionOT$participant==ppp)]
  tz <- motionOT$z[which(motionOT$participant==ppp)]
  avg_tx <- mean(tx[1:IPrun],na.rm = TRUE) # na.rm b/c OT sometimes blinks out and leaves blank rows
  avg_ty <- mean(ty[1:IPrun],na.rm = TRUE)
  avg_tz <- mean(tz[1:IPrun],na.rm = TRUE)
  IP <- c( avg_tx, avg_ty, avg_tz )
  #  The random reaches are handled separately, as they don't have the model playback affecting SU printout time
  k_vec <- 1
  for ( i in 1:last_no_feedback_trial_num ) {
    if ( !(i %in% short_reaches) ) {
      no_count <- 0
      k <- OTstarts[i]
      SEARCHINGforK <- TRUE
      for ( j in k:(k-100) ) {
        if ( SEARCHINGforK ) {
          if ( no_count < 4 ) {
            rIP <- c( tx[j], ty[j], tz[j] ) 
            rIPback <- c( tx[j-1], ty[j-1], tz[j-1] ) 
            no_closer <- vdist(IP,rIPback) >= vdist(IP,rIP) * 0.995
            if ( isTRUE(no_closer) ) {
              no_count <- no_count+1
            }
          } else {
            k <- j+2
            SEARCHINGforK <- FALSE
          }
        }
      }
      k_vec <- c( k_vec, OTstarts[i] - k )
    }
  }
  k_vec <- k_vec[2:length(k_vec)]
  sync_tune <- as.integer(median(k_vec))
  # There might be NA in OTstarts and OTends, from removed shorts, but that's okay,
  #     since NA - sync_tune is still NA. 
  OTstarts[1:last_no_feedback_trial_num] <- OTstarts[1:last_no_feedback_trial_num] - sync_tune
  OTends[1:last_no_feedback_trial_num] <- OTends[1:last_no_feedback_trial_num] - sync_tune
  if ( is.na(sync_tune) ) {
    cat("\n WARNING! sync_tune for Random is NA, participant:", ppp,"\n")
  }
  if (noisy_print) cat(", S-R: ", sync_tune) 
  psv <- c( psv, sync_tune )
  # Now do reaches w/ model playback: 
  k_vec <- 1
  for ( i in (last_no_feedback_trial_num+1):SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      no_count <- 0
      k <- OTstarts[i]
      SEARCHINGforK <- TRUE
      for ( j in k:(k-100) ) {
        if ( SEARCHINGforK ) {
          if ( no_count < 4 ) {
            rIP <- c( tx[j], ty[j], tz[j] ) 
            rIPback <- c( tx[j-1], ty[j-1], tz[j-1] ) 
            no_closer <- vdist(IP,rIPback) >= vdist(IP,rIP) * 0.995
            if ( isTRUE(no_closer) ) {
              no_count <- no_count+1
            }
          } else {
            k <- j+2
            SEARCHINGforK <- FALSE
          }
        }
      }
      k_vec <- c( k_vec, OTstarts[i] - k )
    }
  }
  k_vec <- k_vec[2:length(k_vec)]
  sync_tune <- as.integer(median(k_vec))
  # There might be NA in OTstarts and OTends, from removed shorts, but that's okay,
  #     since NA - sync_tune is still NA. 
  OTstarts[(last_no_feedback_trial_num+1):SUnumofreaches] <- OTstarts[(last_no_feedback_trial_num+1):SUnumofreaches] - sync_tune
  OTends[(last_no_feedback_trial_num+1):SUnumofreaches] <- OTends[(last_no_feedback_trial_num+1):SUnumofreaches] - sync_tune
  if ( is.na(sync_tune) ) {
    cat("\n WARNING! sync_tune for Feedback is NA, participant:", ppp,"\n")
  }
  if (noisy_print) cat(", S-F: ", sync_tune) 
  psv <- c( psv, sync_tune )
  
  # Find and save velocities in OT data.
  OTvelocity <- rep(NA,length(OTtimes))
  for ( i in 1:SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      OTvelocity[OTstarts[i]] <- 0
      for ( j in (OTstarts[i]+1):OTends[i] ) {
        p2 <- c( tx[j], ty[j], tz[j] )
        p1 <- c( tx[j-1], ty[j-1], tz[j-1] )
        if ( !any(is.na(p1)) && !any(is.na(p2)) ) {
          OTvelocity[j] <- vdist(p1,p2) # this is naturally in cm/ms, which is the same as m/s
          OTvelocity[j] <- round(OTvelocity[j],significant_digitsOT)
          if ( !is.numeric(OTvelocity[j]) || is.na(OTvelocity[j]) || length(OTvelocity[j]) != 1 ) {
            cat("\n WARNING! OTvelocity is not numeric, is NA, or isn't length 1, participant:", ppp, "reach:", i,"sample:", j,"\n")
          }
        }
      }
      firstvel <- OTvelocity[OTstarts[i]+1] - abs(OTvelocity[OTstarts[i]+2] - OTvelocity[OTstarts[i]+1])
      if ( firstvel > 0 ) {
        OTvelocity[OTstarts[i]] <- round(firstvel,significant_digitsOT)
      } else {
        OTvelocity[OTstarts[i]] <- 0
      }
    }
  }
  
  # First, smooth OT velocity data w/ 50ms rolling average
  OTvelocity <- rolling_mean(OTvelocity,5,significant_digitsOT)
  # OT velocity is in m/s
  motionOT$SpatialVelocity[which(motionOT$participant==ppp)] <<- OTvelocity
  
  # One more sync check using velocity and adjusting the end position
  end_point_slide <- 0.25 # don't slide below this percentage from the end sample when trying to estimate true reach end point.
  # Start w/ first 25 reaches
  k_vec <- 1
  for ( i in 1:last_no_feedback_trial_num ) {
    if ( !(i %in% short_reaches) ) {
      rlength <- OTends[i] - OTstarts[i]
      end_velocities <- OTvelocity[(OTends[i]-(end_point_slide*rlength)):OTends[i]]
      k <- OTends[i]
      if ( any(!is.na(end_velocities)) ) {
        min_Evel <- min(OTvelocity[(OTends[i]-(end_point_slide*rlength)):OTends[i]],na.rm = TRUE)
        Evel <- OTvelocity[OTends[i]]
        ii <- OTends[i]
        SEARCHINGforII <- TRUE
        while ( is.na(Evel) && SEARCHINGforII ) {
          ii <- ii-1
          Evel <- OTvelocity[ii]
          if ( ii < (OTends[i] - (end_point_slide*rlength)) ) SEARCHINGforII <- FALSE
        }
        if ( !is.na(Evel) && (Evel > (min_Evel*1.02)) ) {
          k <- ii
          SEARCHINGforK <- TRUE
          while(SEARCHINGforK) {
            k <- k-1
            if ( !is.na(OTvelocity[k]) && (OTvelocity[k] <= (min_Evel*1.02)) ) SEARCHINGforK <- FALSE
            if ( k < (OTends[i] - (end_point_slide*rlength)) ) SEARCHINGforK <- FALSE
          }
        } else {
          k <- OTends[i]
        }
      } 
      k_vec <- c( k_vec, OTends[i] - k )
    }
  }
  k_vec <- k_vec[2:length(k_vec)]
  sync_tune <- as.integer(median(k_vec))
  # There might be NA in OTstarts and OTends, from removed shorts, but that's okay,
  #     since NA - sync_tune is still NA. 
  OTstarts[1:last_no_feedback_trial_num] <- OTstarts[1:last_no_feedback_trial_num] - sync_tune
  OTends[1:last_no_feedback_trial_num] <- OTends[1:last_no_feedback_trial_num] - sync_tune
  if ( is.na(sync_tune) ) {
    cat("\n WARNING! end sync_tune for Random is NA, participant:", ppp,"\n")
  }
  if (noisy_print) cat(", ES-R: ", sync_tune) 
  psv <- c( psv, sync_tune )
  # Now the reaches w/ model playback
  k_vec <- 1
  for ( i in (last_no_feedback_trial_num+1):SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      rlength <- OTends[i] - OTstarts[i]
      end_velocities <- OTvelocity[(OTends[i]-(end_point_slide*rlength)):OTends[i]]
      k <- OTends[i]
      if ( any(!is.na(end_velocities)) ) {
        min_Evel <- min(OTvelocity[(OTends[i]-(end_point_slide*rlength)):OTends[i]],na.rm = TRUE)
        Evel <- OTvelocity[OTends[i]]
        ii <- OTends[i]
        SEARCHINGforII <- TRUE
        while ( is.na(Evel) && SEARCHINGforII ) {
          ii <- ii-1
          Evel <- OTvelocity[ii]
          if ( ii < (OTends[i] - (end_point_slide*rlength)) ) SEARCHINGforII <- FALSE
        }
        if ( !is.na(Evel) && (Evel > (min_Evel*1.02)) ) {
          k <- ii
          SEARCHINGforK <- TRUE
          while(SEARCHINGforK) {
            k <- k-1
            if ( !is.na(OTvelocity[k]) && (OTvelocity[k] <= (min_Evel*1.02)) ) SEARCHINGforK <- FALSE
            if ( k < (OTends[i] - (end_point_slide*rlength)) ) SEARCHINGforK <- FALSE
          }
        } else {
          k <- OTends[i]
        }
      } 
      k_vec <- c( k_vec, OTends[i] - k )
    }
  }
  k_vec <- k_vec[2:length(k_vec)]
  sync_tune <- as.integer(median(k_vec))
  # There might be NA in OTstarts and OTends, from removed shorts, but that's okay,
  #     since NA - sync_tune is still NA. 
  OTstarts[(last_no_feedback_trial_num+1):SUnumofreaches] <- OTstarts[(last_no_feedback_trial_num+1):SUnumofreaches] - sync_tune
  OTends[(last_no_feedback_trial_num+1):SUnumofreaches] <- OTends[(last_no_feedback_trial_num+1):SUnumofreaches] - sync_tune
  if ( is.na(sync_tune) ) {
    cat("\n WARNING! end sync_tune for Feedback is NA, participant:", ppp,"\n")
  }
  if (noisy_print) cat(", ES-F: ", sync_tune) 
  psv <- c( psv, sync_tune )
  
  # Build the OT reach col
  OTreachcol <- rep(NA,length(OTtimes))
  OTtrialcol <- rep(NA,length(OTtimes))
  for ( i in 1:SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      OTreachcol[OTstarts[i]:OTends[i]] <- i
    }
  }
  OTtrialcol <- OTreachcol
  
  # Check for floating reaches
  num_of_floats <- 0
  floating_reaches <- NA
  for ( i in 1:SUnumofreaches ) {
    if ( !(i %in% short_reaches) ) {
      rStartPoint <- c( tx[OTstarts[i]], ty[OTstarts[i]], tz[OTstarts[i]] )
      iii <- OTstarts[i]
      while ( any(is.na(rStartPoint)) ) {
        iii <- iii+1
        rStartPoint <- c( tx[iii], ty[iii], tz[iii] )
      }
      if ( !is.numeric(vdist(IP,rStartPoint)) || is.na(vdist(IP,rStartPoint)) || length(vdist(IP,rStartPoint)) != 1 ) {
        cat("\n WARNING! float check vdist is not numeric, is NA, or isn't length 1, participant:", ppp, "reach:", i,"\n")
      }
      if ( vdist(IP,rStartPoint) > float_cutoff ) {
        SUreaches[SUstarts[i]:SUends[i]] <- NA
        SUstarts[i] <- NA
        SUends[i] <- NA
        SUlengths[i] <- NA
        OTreachcol[OTstarts[i]:OTends[i]] <- NA
        OTstarts[i] <- NA
        OTends[i] <- NA
        num_of_floats <- num_of_floats + 1
        floating_reaches <- c( floating_reaches, i ) 
      }
    }
  }
  floating_reaches <- floating_reaches[2:length(floating_reaches)]
  if (noisy_print) cat(", Fs: ", num_of_floats) 
  psv <- c( psv, num_of_floats )
  
  #if (noisy_print) plot(ty,type="p",pch=19)
  #for ( i in 1:SUnumofreaches ) {
  #  if (noisy_print) abline(v=OTends[i])
  #  if (noisy_print) abline(v=OTstarts[i])
  #}
  
  # Count how many reaches we have left
  OTreachcolNONA <- OTreachcol[!is.na(OTreachcol)]
  SUreachesNONA <- SUreaches[!is.na(SUreaches)]
  OTreachcolNONA <- unique(OTreachcolNONA)
  SUreachesNONA <- unique(SUreachesNONA)
  if ( length(OTreachcolNONA) != length(SUreachesNONA) ) {
    cat("\n WARNING! OT and SU reach columns don't contain the same reaches, participant:",ppp,"\n")
  }
  if (noisy_print) cat(", R (final): ", length(OTreachcolNONA),"\n") 
  psv <- c( psv, length(OTreachcolNONA) )
  psv <- c( psv, length(SUreachesNONA[which(SUreachesNONA<=last_no_feedback_trial_num)]) - 1 )
  psv <- c( psv, length(SUreachesNONA[which(SUreachesNONA>last_no_feedback_trial_num)]) )
  psv <- c( psv, length(SUreachesNONA[which(SUreachesNONA>=first_postlearning_feedback_trial_num)]) )
  
  # Now find the average length of reaches, without and with feedback
  tally <- NA
  for ( i in SUreachesNONA ) {
    if ( i <= last_no_feedback_trial_num ) {
      tally <- c( tally, length(SUreaches[which(SUreaches==i)]) )
    }
  }
  tally <- tally[2:length(tally)]
  psv <- c( psv, mean(tally) )
  tally <- NA
  for ( i in SUreachesNONA ) {
    if ( i > last_no_feedback_trial_num ) {
      tally <- c( tally, length(SUreaches[which(SUreaches==i)]) )
    }
  }
  tally <- tally[2:length(tally)]
  psv <- c( psv, mean(tally) )
  
  # Save reach columns
  motionOT$reach[which(motionOT$participant==ppp)] <<- OTreachcol
  motionOT$trials[which(motionOT$participant==ppp)] <<- OTtrialcol
  motionSU$reach[which(motionSU$participant==ppp)] <<- SUreaches
  
  # clean velocity columns: 
  OTindex <- which(motionOT$participant==ppp & 
                     is.na(motionOT$reach))
  motionOT$SpatialVelocity[OTindex] <<- NA
  SUindex <- which(motionSU$participant==ppp & 
                     is.na(motionSU$reach))
  motionSU$RotVelocity[SUindex] <<- NA

  return(psv)
  
}
model_check <- function(noisyprint) {
  if (noisyprint) cat("Model check started\n") 
  found <- FALSE
  for ( p in pTable$number ) {
    m <- pTable$model[which(pTable$number==p)]
    if (!(m %in% motionSU$reach[which(motionSU$participant==p)])) {
      if (noisyprint) cat("WARNING! model missing from participant:",p,"\n")
      found <- TRUE
    }
  }
  if (noisyprint) cat("Model check complete")
  if (found) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Used in improvement_stats and distance_stats
mean_plot <- function( xaxis, yaxis, ci, ttt, lll, yl, pvsig, pvsigFS, pvsigRS) {
  
  dsize <- 2
  axissize <- dsize
  if ( lll[1] > 0 ) lll[1] <- 0
  plot.default( xaxis, yaxis, 
                ylim = lll,
                xlim = c(0.5,2.5),
                ylab = yl,
                xlab = "condition",
                main = ttt,
                cex.main = dsize,
                cex.lab = dsize,
                axes = FALSE,
                pch = 19,
                cex = dsize*2
  )
  axis(side = 1, 
       at = as.factor(xaxis), 
       labels = xaxis,
       cex.axis=axissize
  )
  axis(side=2,cex.axis=axissize)
  abline(h=0,col = "gray", lty = "dashed", lwd=3)
  #arrows(1, yaxis[1] + ci[1], 
  #       1, yaxis[1] - ci[1], 
  #       code=3, col="black", angle=90, lwd=2)
  #arrows(2, yaxis[2] + ci[2], 
  #       2, yaxis[2] - ci[2], 
  #       code=3, col="black", angle=90, lwd=2)
  arrows(1, yaxis[1], 
         1, ci[1], 
         code=2, col="black", angle=90, lwd=2)
  arrows(2, yaxis[2], 
         2, ci[2], 
         code=2, col="black", angle=90, lwd=2)
  #pbarh1 <- max(yaxis[1] + ci[1],yaxis[2] + ci[2]) * 1.15
  #pbarh2 <- max(yaxis[1] + ci[1],yaxis[2] + ci[2]) * 1.25
  pbarh1 <- max(yaxis[1]+(lll[2]*0.25),yaxis[2]+(lll[2]*0.25)) * 1.15 
  pbarh2 <- max(yaxis[1]+(lll[2]*0.25),yaxis[2]+(lll[2]*0.25)) * 1.25
  segments(1,pbarh1,
           1,pbarh2,
           col="black")
  segments(2,pbarh1,
           2,pbarh2,
           col="black")
  segments(1,pbarh2,
           2,pbarh2,
           col="black")
  text(1.5,pbarh2*1.1,pvsig,cex=dsize)
  points( xaxis, yaxis, 
          pch = 19,
          cex = dsize*2,
          col = c( "green3", "red3" )
  )
  roundto <- 1
  if ( yl == "distance (unit quats)" ) {
    roundto <- significant_digitsSU
    show <- significant_digitsSU
  } 
  if ( yl == "distance (cm)" ) {
    roundto <- significant_digitsOT
    show <- significant_digitsOT
  } 

  text(1.1,yaxis[1]+(lll[2]*0.025),format(round(yaxis[1],roundto),nsmall=show),cex=dsize,adj = c(0, 0.5))
  text(2.1,yaxis[2]+(lll[2]*0.025),format(round(yaxis[2],roundto),nsmall=show),cex=dsize,adj = c(0, 0.5))
  text(1.25,yaxis[1]+(lll[2]*0.15),format(pvsigRS,nsmall=show),cex=dsize,adj = c(0, 0.5))
  text(2.25,yaxis[2]+(lll[2]*0.15),format(pvsigFS,nsmall=show),cex=dsize,adj = c(0, 0.5))
  
}
mean_plot_fb <- function( xaxis, yaxisF, yaxisS, ttt, lll, yl) {
  
  dsize <- 2
  axissize <- dsize
  if ( lll[1] > 0 ) lll[1] <- 0
  plot.default( xaxis, yaxisF, 
                ylim = lll,
                xlim = c(0.5,2.5),
                ylab = yl,
                xlab = "feedback",
                main = ttt,
                cex.main = dsize,
                cex.lab = dsize,
                axes = FALSE,
                pch = 19,
                cex = dsize*2
  )
  axis(side = 1, 
       at = as.factor(xaxis), 
       labels = xaxis,
       cex.axis=axissize
  )
  axis(side=2,cex.axis=axissize)
  abline(h=0,col = "gray", lty = "dashed", lwd=3)
  points( xaxis, yaxisF, 
          pch = 19,
          cex = dsize*2,
          col = c( "red3", "red3" )
  )
  points( xaxis, yaxisS, 
          pch = 19,
          cex = dsize*2,
          col = c( "green3", "green3" )
  )
  roundto <- 1
  if ( yl == "distance (unit quats)" ) {
    roundto <- significant_digitsSU
    show <- significant_digitsSU
  } 
  if ( yl == "distance (cm)" ) {
    roundto <- significant_digitsOT
    show <- significant_digitsOT
  } 
  NF_son <- 1
  NF_fau <- 1
  F_son <- 1
  F_fau <- 1
  labelscaler <- 0.1
  if ( yaxisF[1] > yaxisS[1] ) {
    NF_fau <- yaxisF[1]+(lll[2]*labelscaler)
    NF_son <- yaxisS[1]-(lll[2]*labelscaler)
  } else {
    NF_fau <- yaxisF[1]-(lll[2]*labelscaler)
    NF_son <- yaxisS[1]+(lll[2]*labelscaler)
  }
  if ( yaxisF[2] > yaxisS[2] ) {
    F_fau <- yaxisF[2]+(lll[2]*labelscaler)
    F_son <- yaxisS[2]-(lll[2]*labelscaler)
  } else {
    F_fau <- yaxisF[2]-(lll[2]*labelscaler)
    F_son <- yaxisS[2]+(lll[2]*labelscaler)
  }
  text(1,NF_fau,format(round(yaxisF[1],roundto),nsmall=show),cex=dsize,adj = 0)
  text(1,NF_son,format(round(yaxisS[1],roundto),nsmall=show),cex=dsize,adj = 0)
  text(2,F_fau,format(round(yaxisF[2],roundto),nsmall=show),cex=dsize,adj = 0)
  text(2,F_son,format(round(yaxisS[2],roundto),nsmall=show),cex=dsize,adj = 0)
  text(0.975,NF_fau,"Faux =",cex=dsize,adj = 1)
  text(0.975,NF_son,"Son =",cex=dsize,adj = 1)
  text(1.975,F_fau,"Faux =",cex=dsize,adj = 1)
  text(1.975,F_son,"Son =",cex=dsize,adj = 1)
  
}
boxplot_jitter <- function(n,values,ttt,lll,yl) {
  
  default_size <- 2
  if ( lll[1] > 0 ) lll[1] <- 0
  # Create data
  axissize <- default_size
  names <- n
  value <- values
  data <- data.frame(names,value)
  
  myColors <- ifelse(levels(names)=="Sonification" , "green3" , 
                     ifelse(levels(names)=="Faux Sonification", "red3", 
                            "grey" ) )
  
  # Basic boxplot
  boxplot(data$value ~ data$names,
          ylim = lll,
          ylab = yl,
          xlab = "condition",
          main = ttt,
          cex.main = default_size,
          cex.lab = default_size,
          col = myColors, 
          axes = FALSE
  )
  abline(h=0,col = "gray", lty = "dashed", lwd=3)
  axis(side = 1, 
       at = as.factor(names), 
       labels = names,
       cex.axis=axissize
  )
  axis(side=2,cex.axis=axissize)
  
  # Add data points
  # mylevels <- levels(data$names)
  # levelProportions <- summary(data$names)/nrow(data)
  # for(i in 1:length(mylevels)){
  #   thislevel <- mylevels[i]
  #   thisvalues <- data[data$names==thislevel, "value"]
  #   # take the x-axis indices and add a jitter, proportional to the N in each level
  #   myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  #   points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
  # }
}
pvsignf <- function(pvalue) {
  stars <- "*"
  if ( pvalue > 0.05 ) stars <- "ns"
  if ( pvalue <= 0.05 ) stars <- "*"
  if ( pvalue <= 0.01 ) stars <- "**"
  if ( pvalue <= 0.001 ) stars <- "***"
  if ( pvalue <= 0.0001 ) stars <- "****"
  return(stars)
}
# Used in fill_pTable
checkifnumber <- function(x) {
  if ( is.na(x) || !is.numeric(x) || length(x) != 1 ) {
    cat("\n WARNING! expected number, but got NA, nonnumeric, or length", length(x),"\n")
  }
}
checkifnumericvector <- function(x,l) {
  if ( any(is.na(x)) || any(!is.numeric(x)) || length(x) != l ) {
    cat("\n WARNING! expected numeric vector of length", l,"but got NAs, nonnumeric items, or length", length(x),"\n")
  }
}
# Used in fill_pTable
find_spatial_path_error <- function(printplots,ppp,final_only,useDTW) {
  
  # For all reaches R of participant ppp, this function finds the distance between each point of R
  #   and ppp's model, defined as the distance between that point and a matching point of R, where
  #   "matching" is done either by simple rescaling, or by DTW of velocity curves. At the end, finds
  #   the mean of these distances for each reach, called "path error". Note that, for OT/SU analysis,
  #   this function also normalizes these values by dividing them by the length of ppp's model. 
  
  # normalized error for the OT/SU correlation analysis doesn't use DTW.
  
  model <- pTable$model[which(pTable$number==ppp)]
  MRD <- rep(NA,max(motionOT$reach,na.rm=TRUE)) # mean reach distance
  
  # Grab data
  tx <- motionOT$x[which(motionOT$participant==ppp)]
  ty <- motionOT$y[which(motionOT$participant==ppp)]
  tz <- motionOT$z[which(motionOT$participant==ppp)]
  reach <- motionOT$reach[which(motionOT$participant==ppp)]
  mx <- tx[which(reach==model)] 
  my <- ty[which(reach==model)] 
  mz <- tz[which(reach==model)] 
  
  avg_distances <- rep(NA,max(reach,na.rm = TRUE))
  model_length <- length(reach[which(reach==model)])
  checkifnumber(model_length)
  
  # Find length of the model for normalization
  model_path_length <- 0 
  for ( k in 2:model_length ) {
    model_path_length <- c( model_path_length, 
                            vdist(
                              c( mx[k],my[k],mz[k] ),
                              c( mx[k-1],my[k-1],mz[k-1] )
                            )
    )
  }
  model_path_length <- sum(model_path_length,na.rm=TRUE)
  checkifnumber(model_path_length)
  
  # Now find distances
  missed_points <- 0 
  found_points <- 0
  nr <- max(reach,na.rm = TRUE) # Number of reaches
  for ( i in 1:nr ) {
    if ( i %in% reach ) {
      r_start <- min(which(motionOT$reach==i & motionOT$participant==ppp),na.rm = TRUE)
      reach_length <- length(reach[which(reach==i)])
      checkifnumber(reach_length)
      distances <- rep(NA,reach_length)
      rtx <- tx[which(reach==i)]
      rty <- ty[which(reach==i)]
      rtz <- tz[which(reach==i)]
      for ( j in 1:reach_length ) {
        if ( j < 2 ) {
          model_point <- j
        } else {
          model_point <- as.integer( model_length * ( j / reach_length ) )
          checkifnumber(model_point)
        }
        if ( model_point == 0 ) model_point <- 1
        p <- c(rtx[j],rty[j],rtz[j])
        m <- c(mx[model_point],my[model_point],mz[model_point])
        if ( !any(is.na(p)) && !any(is.na(m)) ) {
          checkifnumericvector(p,3)
          checkifnumericvector(m,3)
          distances[j] <- round(vdist(p,m),significant_digitsOT)
          checkifnumber(distances[j])
          found_points <- found_points+1
        } else {
          missed_points <- missed_points+1
        }
      }
      avg_distances[i] <- round(mean(distances,na.rm=TRUE),significant_digitsOT)
      normalized_distances <- distances / model_path_length
      motionOT$Rerror[r_start:((r_start+reach_length)-1)] <<- distances
      motionOT$Nerror[r_start:((r_start+reach_length)-1)] <<- normalized_distances
      if (useDTW) {
        d <- DTW_model_indexesOT_Vel(ppp,i)
        mi <- d$index1
        ri <- d$index2
        distances <- rep(NA,length(ri))
        if ( length(mi) != length(ri) ) {
          cat("Something went wrong w/ DTW")
          return()
        }
        for ( j in 1:length(ri) ) {
          pj <- ri[j]
          mj <- mi[j]
          p <- c(rtx[pj],rty[pj],rtz[pj])
          m <- c(mx[mj],my[mj],mz[mj])
          if ( !any(is.na(p)) && !any(is.na(m)) ) {
            checkifnumericvector(p,3)
            checkifnumericvector(m,3)
            distances[j] <- round(vdist(p,m),significant_digitsOT)
            checkifnumber(distances[j])
          } 
        }
        avg_distances[i] <- round(mean(distances,na.rm=TRUE),significant_digitsOT)
      }
      checkifnumber(avg_distances[i])
      MRD[i] <- avg_distances[i]
    } 
  }
  
  # Print plots, if desired
  if ( printplots ) {
    #for coloring plots
    cv <- 1
    for ( i in 1:(model-1) ) {
      cv <- c( cv, "blue" )
    }
    cv <- c( cv, 1 ) 
    for ( i in (model+1):last_no_feedback_trial_num ) {
      cv <- c( cv, "blue" )
    }
    if ( pTable$Condition[which(pTable$number==ppp)] == "S" ) {
      for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
        cv <- c( cv, "green" ) 
      }
    } else if ( pTable$Condition[which(pTable$number==ppp)] == "C" ) {
      for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
        cv <- c( cv, "red" ) 
      }
    }
    cv <- cv[2:length(cv)]
    #plots
    plot(1:max(reach,na.rm = TRUE),avg_distances,pch=16,col=cv,
         main="Mean Distance from Model by Reach (XZY)", xlab="reach number",ylab="distance (meters)")
  }
  
  # Find mean path error for w/ and w/o Feedback, and improvement, and save to pTable
  rnum <- max(reach,na.rm = TRUE)
  if ( model == 1 ) {
    random_r <- avg_distances[2:last_no_feedback_trial_num]
  } else {
    random_r <- c( avg_distances[1:(model-1)], avg_distances[(model+1):last_no_feedback_trial_num] )
  }
  if (final_only) {
    son_r <- avg_distances[first_postlearning_feedback_trial_num:rnum]
  } else {
    son_r <- avg_distances[(last_no_feedback_trial_num+1):rnum]
  }
  pTable$SpatialPathErrorNF[which(pTable$number==ppp)] <<- round(mean(random_r,na.rm=TRUE),significant_digitsOT)
  pTable$SpatialPathErrorF[which(pTable$number==ppp)] <<- round(mean(son_r,na.rm=TRUE),significant_digitsOT)
  pTable$SpatialPathErrorI[which(pTable$number==ppp)] <<- round(pTable$SpatialPathErrorNF[which(pTable$number==ppp)] - pTable$SpatialPathErrorF[which(pTable$number==ppp)],significant_digitsOT)
  
  cat("Missed point percentage (OT):",(missed_points/(found_points+missed_points))*100,"\n")
  
  return(MRD)
  
}
find_spatial_target_error <- function(printplots,ppp,final_only) {
  
  # For all reaches R of participant ppp, this function finds the distance between the last point of R
  #   and the last point of ppp's model, or between the nearest valid points to these points. 
  
  model <- pTable$model[which(pTable$number==ppp)]
  MRD <- rep(NA,max(motionOT$reach,na.rm=TRUE))
  
  # load data
  tx <- motionOT$x[which(motionOT$participant==ppp)]
  ty <- motionOT$y[which(motionOT$participant==ppp)]
  tz <- motionOT$z[which(motionOT$participant==ppp)]
  reach <- motionOT$reach[which(motionOT$participant==ppp)]
  mx <- tx[which(reach==model)] 
  my <- ty[which(reach==model)] 
  mz <- tz[which(reach==model)] 
  
  # Find valid model point nearest to its end
  model_end <- c(mx[length(mx)],my[length(my)],mz[length(mz)])
  if ( any(is.na(model_end)) || length(model_end) != 3 ) {
    NEEDMODELEND <- TRUE
    check <- length(mx)
    while (NEEDMODELEND) {
      check <- check-1
      model_end <- c(mx[check],my[check],mz[check])
      if ( !any(is.na(model_end)) && length(model_end) == 3 ) {
        NEEDMODELEND <- FALSE
      }
    }
    if ( check < length(mx) * 0.5 ) {
      cat("WARNING! model end (OT) found more 50% from last model sample\n")
    }
    cat("Model end (OT) found at %:",(check/length(mx))*100,"\n")
  }
  checkifnumericvector(model_end,3)
  
  # Find target distances
  missed_reach <- 0 
  found_reach <- 0
  missed_reach_list <- NA
  distances <- rep(NA,max(reach,na.rm = TRUE))
  check_avg <- NA
  for ( i in 1:max(reach,na.rm = TRUE) ) {
    if ( i %in% reach ) {
      rx <- tx[which(reach==i)] 
      ry <- ty[which(reach==i)] 
      rz <- tz[which(reach==i)] 
      r_end <- c(rx[length(rx)],ry[length(ry)],rz[length(rz)])
      SEARCHINGTOOLONG <- FALSE
      if ( any(is.na(r_end)) || length(r_end) != 3 ) {
        NEEDREND <- TRUE
        check <- length(rx)
        checkifnumber(check)
        while (NEEDREND) {
          check <- check-1
          r_end <- c(rx[check],ry[check],rz[check])
          if ( !any(is.na(r_end)) && length(r_end) == 3 ) {
            NEEDREND <- FALSE
          }
          if ( check < length(rx) * 0.35 ) SEARCHINGTOOLONG <- TRUE
          if ( SEARCHINGTOOLONG ) NEEDEND <- FALSE
        }
        check_avg <- c( check_avg, (check/length(rx))*100 )
      }
      check_avg <- c( check_avg, 100 )
      if ( SEARCHINGTOOLONG ) {
        missed_reach_list <- c( missed_reach_list, i ) 
        missed_reach <- missed_reach+1
      } else {
        checkifnumericvector(r_end,3)
        distances[i] <- round(vdist(model_end,r_end),significant_digitsOT)
        checkifnumber(distances[i])
        MRD[i] <- distances[i]
        found_reach <- found_reach+1
      }
    } 
  }
  if ( any(!is.na(check_avg)) ) {
    check_avg <- mean(check_avg,na.rm=TRUE)
    cat("Reach ends found on avg at %:",check_avg,"\n")
  }
  
  # Print plot of results, if desired
  if ( printplots ) {
    cv <- 1
    for ( i in 1:(model-1) ) {
      cv <- c( cv, "blue" )
    }
    cv <- c( cv, 1 ) 
    for ( i in (model+1):last_no_feedback_trial_num ) {
      cv <- c( cv, "blue" )
    }
    for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
      cv <- c( cv, "green" ) 
    }
    cv <- cv[2:length(cv)]
    plot(1:max(reach,na.rm = TRUE),distances,pch=16,col=cv,
         main="Distance to Model End by Reach (XZY space)", xlab="reach number",ylab="distance (meters)")
  }
  
  #Find mean target error for w/ and w/o Feedback, and improvement, and save to pTable
  rnum <- max(reach,na.rm = TRUE)
  if ( model == 1 ) {
    random_r <- distances[2:last_no_feedback_trial_num]
  } else {
    random_r <- c( distances[1:(model-1)], distances[(model+1):last_no_feedback_trial_num] )
  }
  if (final_only) {
    son_r <- distances[first_postlearning_feedback_trial_num:rnum]
  } else {
    son_r <- distances[(last_no_feedback_trial_num+1):rnum]
  }
  pTable$SpatialTargetErrorNF[which(pTable$number==ppp)] <<- round(mean(random_r,na.rm=TRUE),significant_digitsOT)
  pTable$SpatialTargetErrorF[which(pTable$number==ppp)] <<- round(mean(son_r,na.rm=TRUE),significant_digitsOT)
  pTable$SpatialTargetErrorI[which(pTable$number==ppp)] <<- round(pTable$SpatialTargetErrorNF[which(pTable$number==ppp)] - pTable$SpatialTargetErrorF[which(pTable$number==ppp)],significant_digitsOT)
  
  if ( length(missed_reach_list) > 1 ) {
    missed_reach_list <- missed_reach_list[2:length(missed_reach_list)]
    cat("Missed reach percentage (OT):",(missed_reach/(found_reach+missed_reach))*100,"Reaches:",missed_reach_list,"\n")
  } else {
    cat("Missed reach percentage (OT):",(missed_reach/(found_reach+missed_reach))*100,"\n")
  }
  
  return(MRD)
  
}
find_rotation_path_error <- function(printplots,ppp,final_only,useDTW) {
  
  # For all reaches R of participant ppp, this function finds the distance between each point of R
  #   and ppp's model, defined as the distance between that point and a matching point of R, where
  #   "matching" is done either by simple rescaling, or by DTW of velocity curves. At the end, finds
  #   the mean of these distances for each reach, called "path error". Note that, for OT/SU analysis,
  #   this function also normalizes these values by dividing them by the length of ppp's model. 
  
  model <- pTable$model[which(pTable$number==ppp)]
  MRD <- rep(NA,max(motionOT$reach,na.rm=TRUE))
  
  # Grab data
  atx <- motionSU$qax[which(motionSU$participant==ppp)]
  aty <- motionSU$qay[which(motionSU$participant==ppp)]
  atz <- motionSU$qaz[which(motionSU$participant==ppp)]
  atr <- motionSU$qar[which(motionSU$participant==ppp)]
  btx <- motionSU$qbx[which(motionSU$participant==ppp)]
  bty <- motionSU$qby[which(motionSU$participant==ppp)]
  btz <- motionSU$qbz[which(motionSU$participant==ppp)]
  btr <- motionSU$qbr[which(motionSU$participant==ppp)]
  reach <- motionSU$reach[which(motionSU$participant==ppp)]
  amx <- atx[which(reach==model)] 
  amy <- aty[which(reach==model)] 
  amz <- atz[which(reach==model)] 
  amr <- atr[which(reach==model)] 
  bmx <- btx[which(reach==model)] 
  bmy <- bty[which(reach==model)] 
  bmz <- btz[which(reach==model)] 
  bmr <- btr[which(reach==model)] 
  
  avg_distances <- rep(NA,max(reach,na.rm = TRUE))
  model_length <- length(reach[which(reach==model)])
  checkifnumber(model_length)
  
  # Find length of the model for normalization
  model_path_length <- 0 
  for ( k in 2:model_length ) {
    model_path_length <- c( model_path_length, 
                            vdist(
                              c( amx[k],amy[k],amz[k],amr[k] ),
                              c( amx[k-1],amy[k-1],amz[k-1],amr[k-1] )
                            ) + 
                              vdist(
                                c( bmx[k],bmy[k],bmz[k],bmr[k] ),
                                c( bmx[k-1],bmy[k-1],bmz[k-1],bmr[k-1] )
                              )
    )
  }
  model_path_length <- sum(model_path_length,na.rm=TRUE)
  checkifnumber(model_path_length)
  
  # Now find distances
  missed_points <- 0 
  found_points <- 
  nr <- max(reach,na.rm = TRUE) # number of reaches
  for ( i in 1:nr ) {
    if ( i %in% reach ) {
      r_start <- min(which(motionSU$reach==i & motionSU$participant==ppp),na.rm = TRUE)
      reach_length <- length(reach[which(reach==i)])
      checkifnumber(reach_length)
      distances <- rep(NA,reach_length)
      artx <- atx[which(reach==i)]
      arty <- aty[which(reach==i)]
      artz <- atz[which(reach==i)]
      artr <- atr[which(reach==i)]
      brtx <- btx[which(reach==i)]
      brty <- bty[which(reach==i)]
      brtz <- btz[which(reach==i)]
      brtr <- btr[which(reach==i)]
      
      for ( j in 1:reach_length ) {
        if ( j < 2 ) {
          model_point <- j
        } else {
          model_point <- as.integer( model_length * ( j / reach_length ) )
          checkifnumber(model_point)
        }
        if ( model_point == 0 ) model_point <- 1
        p1 <- c(artx[j],arty[j],artz[j],artr[j])
        p2 <- c(brtx[j],brty[j],brtz[j],brtr[j])
        m1 <- c(amx[model_point],amy[model_point],amz[model_point],amr[model_point])
        m2 <- c(bmx[model_point],bmy[model_point],bmz[model_point],bmr[model_point])
        if ( !any(is.na(p1)) && !any(is.na(p2)) && !any(is.na(m1)) && !any(is.na(m2)) ) {
          checkifnumericvector(p1,4)
          checkifnumericvector(p2,4)
          checkifnumericvector(m1,4)
          checkifnumericvector(m2,4)
          distances[j] <- round(vdist(p1,m1) + vdist(p2,m2),significant_digitsSU)
          checkifnumber(distances[j])
          found_points <- found_points+1
        } else {
          missed_points <- missed_points+1
        }
      }
      avg_distances[i] <- round(mean(distances,na.rm=TRUE),significant_digitsSU)
      normalized_distances <- distances / model_path_length
      motionSU$Rerror[r_start:((r_start+reach_length)-1)] <<- distances
      motionSU$Nerror[r_start:((r_start+reach_length)-1)] <<- normalized_distances
      if (useDTW) {
        d <- DTW_model_indexesSU_Vel(ppp,i)
        mi <- d$index1
        ri <- d$index2
        distances <- rep(NA,length(ri))
        if ( length(mi) != length(ri) ) {
          cat("Something went wrong w/ DTW")
          return()
        }
        for ( j in 1:length(ri) ) {
          pj <- ri[j]
          mj <- mi[j]
          p1 <- c(artx[pj],arty[pj],artz[pj],artr[pj])
          p2 <- c(brtx[pj],brty[pj],brtz[pj],brtr[pj])
          m1 <- c(amx[mj],amy[mj],amz[mj],amr[mj])
          m2 <- c(bmx[mj],bmy[mj],bmz[mj],bmr[mj])
          if ( !any(is.na(p1)) && !any(is.na(p2)) && !any(is.na(m1)) && !any(is.na(m2)) ) {
            checkifnumericvector(p1,4)
            checkifnumericvector(p2,4)
            checkifnumericvector(m1,4)
            checkifnumericvector(m2,4)
            distances[j] <- round(vdist(p1,m1) + vdist(p2,m2),significant_digitsSU)
            checkifnumber(distances[j])
          } 
        }
        avg_distances[i] <- round(mean(distances,na.rm=TRUE),significant_digitsSU)
      }
      checkifnumber(avg_distances[i])
      MRD[i] <- avg_distances[i]
      
    } 
    
  }
  
  # Print plots, if desired
  if ( printplots ) {
    #for coloring plots
    cv <- 1
    for ( i in 1:(model-1) ) {
      cv <- c( cv, "blue" )
    }
    cv <- c( cv, 1 ) 
    for ( i in (model+1):last_no_feedback_trial_num ) {
      cv <- c( cv, "blue" )
    }
    if ( pTable$Condition[which(pTable$number==ppp)] == "S" ) {
      for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
        cv <- c( cv, "green" ) 
      }
    } else if ( pTable$Condition[which(pTable$number==ppp)] == "C" ) {
      for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
        cv <- c( cv, "red" ) 
      }
    }
    cv <- cv[2:length(cv)]
    
    #plots
    plot(1:max(reach,na.rm = TRUE),avg_distances,pch=16,col=cv,
         main="Distance to Model End by Reach (QUAT space)", 
         xlab="reach number",ylab="distance (quaternion units)")
    
  }
  
  # Find mean path error for w/ and w/o Feedback, and improvement, and save to pTable
  rnum <- max(reach,na.rm = TRUE)
  if ( model == 1 ) {
    random_r <- avg_distances[2:last_no_feedback_trial_num]
  } else {
    random_r <- c( avg_distances[1:(model-1)], avg_distances[(model+1):last_no_feedback_trial_num] )
  }
  if (final_only) {
    son_r <- avg_distances[first_postlearning_feedback_trial_num:rnum]
  } else {
    son_r <- avg_distances[(last_no_feedback_trial_num+1):rnum]
  }
  pTable$RotationPathErrorNF[which(pTable$number==ppp)] <<- round(mean(random_r,na.rm=TRUE),significant_digitsSU)
  pTable$RotationPathErrorF[which(pTable$number==ppp)] <<- round(mean(son_r,na.rm=TRUE),significant_digitsSU)
  pTable$RotationPathErrorI[which(pTable$number==ppp)] <<- round(pTable$RotationPathErrorNF[which(pTable$number==ppp)] - pTable$RotationPathErrorF[which(pTable$number==ppp)],significant_digitsSU)
  
  cat("Missed point percentage (SU):",(missed_points/(found_points+missed_points))*100,"\n")
  
  return(MRD)
  
}
find_rotation_target_error <- function(printplots,ppp,final_only) {
  
  # For all reaches R of participant ppp, this function finds the distance between the last point of R
  #   and the last point of ppp's model, or between the nearest valid points to these points. 
  
  model <- pTable$model[which(pTable$number==ppp)]
  MRD <- rep(NA,max(motionOT$reach,na.rm=TRUE))
  
  # load data
  atx <- motionSU$qax[which(motionSU$participant==ppp)]
  aty <- motionSU$qay[which(motionSU$participant==ppp)]
  atz <- motionSU$qaz[which(motionSU$participant==ppp)]
  atr <- motionSU$qar[which(motionSU$participant==ppp)]
  btx <- motionSU$qbx[which(motionSU$participant==ppp)]
  bty <- motionSU$qby[which(motionSU$participant==ppp)]
  btz <- motionSU$qbz[which(motionSU$participant==ppp)]
  btr <- motionSU$qbr[which(motionSU$participant==ppp)]
  reach <- motionSU$reach[which(motionSU$participant==ppp)]
  amx <- atx[which(reach==model)] 
  amy <- aty[which(reach==model)] 
  amz <- atz[which(reach==model)] 
  amr <- atr[which(reach==model)] 
  bmx <- btx[which(reach==model)] 
  bmy <- bty[which(reach==model)] 
  bmz <- btz[which(reach==model)] 
  bmr <- btr[which(reach==model)] 
  
  # Find valid model point nearest to its end
  model_end1 <- c(amx[length(amx)],amy[length(amy)],amz[length(amz)],amr[length(amr)])
  model_end2 <- c(bmx[length(bmx)],bmy[length(bmy)],bmz[length(bmz)],bmr[length(bmr)])
  if ( any(is.na(model_end1)) || length(model_end1) != 4 ) {
    NEEDMODELEND <- TRUE
    check <- length(amx)
    while (NEEDMODELEND) {
      check <- check-1
      model_end1 <- c(amx[check],amy[check],amz[check],amr[check])
      if ( !any(is.na(model_end1)) && length(model_end1) == 4 ) {
        NEEDMODELEND <- FALSE
      }
    }
    if ( check < length(amx) * 0.5 ) {
      cat("WARNING! model end (SUa) found more 50% from last model sample\n")
    }
    cat("Model end (SUa) found at %:",(check/length(mx))*100,"\n")
  }
  if ( any(is.na(model_end2)) || length(model_end2) != 4 ) {
    NEEDMODELEND <- TRUE
    check <- length(bmx)
    while (NEEDMODELEND) {
      check <- check-1
      model_end2 <- c(bmx[check],bmy[check],bmz[check],bmr[check])
      if ( !any(is.na(model_end2)) && length(model_end2) == 4 ) {
        NEEDMODELEND <- FALSE
      }
    }
    if ( check < length(bmx) * 0.5 ) {
      cat("WARNING! model end (SUb) found more 50% from last model sample\n")
    }
    cat("Model end (SUb) found at %:",(check/length(mx))*100,"\n")
  }
  checkifnumericvector(model_end1,4)
  checkifnumericvector(model_end2,4)
  
  # Find target distances
  missed_reach <- 0 
  found_reach <- 0
  missed_reach_list <- NA
  distances <- rep(NA,max(reach,na.rm = TRUE))
  check_avgA <- NA
  check_avgB <- NA
  for ( i in 1:max(reach,na.rm = TRUE) ) {
    if ( i %in% reach ) {
      arx <- atx[which(reach==i)] 
      ary <- aty[which(reach==i)] 
      arz <- atz[which(reach==i)] 
      arr <- atr[which(reach==i)]
      brx <- btx[which(reach==i)] 
      bry <- bty[which(reach==i)] 
      brz <- btz[which(reach==i)] 
      brr <- btr[which(reach==i)]
      r_end1 <- c(arx[length(arx)],ary[length(ary)],arz[length(arz)],arr[length(arr)])
      r_end2 <- c(brx[length(brx)],bry[length(bry)],brz[length(brz)],brr[length(brr)])
      SEARCHINGTOOLONG <- FALSE
      if ( any(is.na(r_end1)) || length(r_end1) != 4 ) {
        NEEDREND <- TRUE
        check <- length(arx)
        checkifnumber(check)
        while (NEEDREND) {
          check <- check-1
          r_end1 <- c(arx[check],ary[check],arz[check],arr[check])
          if ( !any(is.na(r_end1)) && length(r_end1) == 4 ) {
            NEEDREND <- FALSE
          }
          if ( check < length(arx) * 0.35 ) SEARCHINGTOOLONG <- TRUE
          if ( SEARCHINGTOOLONG ) NEEDEND <- FALSE
        }
        check_avgA <- c( check_avgA, ((check/length(arx))*100) ) 
      }
      check_avgA <- c( check_avgA, 100 )
      if ( any(is.na(r_end2)) || length(r_end2) != 4 ) {
        NEEDREND <- TRUE
        check <- length(brx)
        checkifnumber(check)
        if ( is.na(length(brx)) || !is.numeric(length(brx)) || !(length(brx)>0) ) {
          cat("WARNING! length of brx isn't a number:",length(brx),"\n")
        }
        while (NEEDREND) {
          check <- check-1
          r_end2 <- c(brx[check],bry[check],brz[check],brr[check])
          if ( !any(is.na(r_end2)) && length(r_end2) == 4 ) {
            NEEDREND <- FALSE
          }
          if ( check < length(brx) * 0.35 ) SEARCHINGTOOLONG <- TRUE
          if ( SEARCHINGTOOLONG ) NEEDEND <- FALSE
        }
        check_avgB <- c( check_avgB, ((check/length(brx))*100) )
      }
      check_avgB <- c( check_avgB, 100 )
      if ( SEARCHINGTOOLONG ) {
        missed_reach_list <- c( missed_reach_list, i ) 
        missed_reach <- missed_reach+1
      } else {
        checkifnumericvector(r_end1,4)
        checkifnumericvector(r_end2,4)
        distances[i] <- round(vdist(model_end1,r_end1) + vdist(model_end2,r_end2),significant_digitsSU)
        checkifnumber(distances[i])
        MRD[i] <- distances[i]
        found_reach <- found_reach+1
      }
    } 
  }
  if ( any(!is.na(check_avgA)) ) {
    check_avgA <- mean(check_avgA,na.rm=TRUE)
    cat("Reach ends (SUa) found on avg at %:",check_avgA,"\n")
  }
  if ( any(!is.na(check_avgB)) ) {
    check_avgB <- mean(check_avgB,na.rm=TRUE)
    cat("Reach ends (SUb) found on avg at %:",check_avgB,"\n")
  }
  
  # Print plot of results, if desired
  if ( printplots ) {
    #for coloring plots
    cv <- 1
    for ( i in 1:(model-1) ) {
      cv <- c( cv, "blue" )
    }
    cv <- c( cv, 1 ) 
    for ( i in (model+1):last_no_feedback_trial_num ) {
      cv <- c( cv, "blue" )
    }
    if ( pTable$Condition[which(pTable$number==ppp)] == "S" ) {
      for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
        cv <- c( cv, "green" ) 
      }
    } else if ( pTable$Condition[which(pTable$number==ppp)] == "C" ) {
      for ( i in (last_no_feedback_trial_num+1):max(reach,na.rm = TRUE) ) {
        cv <- c( cv, "red" ) 
      }
    }
    cv <- cv[2:length(cv)]
    
    #plots
    plot(1:max(reach,na.rm = TRUE),distances,pch=16,col=cv,
         main="Distance to Model End by Reach (QUAT space)", 
         xlab="reach number",ylab="distance (quaternion units)")
    
  }
  
  #Find mean target error for w/ and w/o Feedback, and improvement, and save to pTable
  rnum <- max(reach,na.rm = TRUE)
  if ( model == 1 ) {
    random_r <- distances[2:last_no_feedback_trial_num]
  } else {
    random_r <- c( distances[1:(model-1)], distances[(model+1):last_no_feedback_trial_num] )
  }
  if (final_only) {
    son_r <- son_r <- distances[first_postlearning_feedback_trial_num:rnum]
  } else {
    son_r <- distances[(last_no_feedback_trial_num+1):rnum]
  }
  pTable$RotationTargetErrorNF[which(pTable$number==ppp)] <<- round(mean(random_r,na.rm=TRUE),significant_digitsSU)
  pTable$RotationTargetErrorF[which(pTable$number==ppp)] <<- round(mean(son_r,na.rm=TRUE),significant_digitsSU)
  pTable$RotationTargetErrorI[which(pTable$number==ppp)] <<- round(pTable$RotationTargetErrorNF[which(pTable$number==ppp)] - pTable$RotationTargetErrorF[which(pTable$number==ppp)],significant_digitsSU)
  
  if ( length(missed_reach_list) > 1 ) {
    missed_reach_list <- missed_reach_list[2:length(missed_reach_list)]
    cat("Missed reach percentage (SU):",(missed_reach/(found_reach+missed_reach))*100,"Reaches:",missed_reach_list,"\n")
  } else {
    cat("Missed reach percentage (SU):",(missed_reach/(found_reach+missed_reach))*100,"\n")
  }
  
  return(MRD)
  
}
