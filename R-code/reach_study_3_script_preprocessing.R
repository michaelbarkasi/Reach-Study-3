
# Preprocessing & Data Cleaning:
# Functions to process data into dataframes we can use for the analysis

# Run this to chop all OptiTrack data and check all reaches for all participants as a batch
chop_data <- function(print,
                      float_cutoff # if reach starts further than this (cm) from IP in OT data, discard as float
) {
  
  # Raw OT data is in meters; put in cm
  motionOT$x <<- motionOT$x * 100
  motionOT$y <<- motionOT$y * 100
  motionOT$z <<- motionOT$z * 100
  
  # Copy SU reach column into a trial column; won't be cleaned out with NAs once bad reaches removed
  motionSU$trials <<- motionSU$reach
  motionOT$trials <<- rep(NA,length(motionOT$reach))
  
  PSV <- NA # this is for tracking adjustments made during the chopping process
  
  cat("Chopping and checking reaches\n")
  
  participant_vector <- pTable$number
  
  cat("\n")
  
  cat("Number of participants: ", length(participant_vector), "\n")
  cat("Participants: ", participant_vector, "\n") 
  
  cat("\n")
  
  # This is the part of the function that chops the data.
  # Runs ParticipantChopOT for all participants
  for ( i in 1:length(participant_vector) ) {
    
    cat("Participant: ", participant_vector[i]) 
    
    PSV <- ParticipantChopOT(print,
                             pTable$model[i],
                             -20, # just a guess at reaction time (200ms) between the Son Unit's sync prompt and actual OT button press
                             pTable$SUStartTime[i],
                             participant_vector[i],
                             150,
                             float_cutoff,
                             PSV
                            )
    
  }
  
  if ( model_check(FALSE) ) cat("\n WARNING! At least one model lost in chopping.\n")
  
  PSV <- PSV[2:length(PSV)] # throw out the NA used to initialize the vector
  
  if ( length(PSV)/length(participant_vector) != 16 ) {
    cat("\n WARNING! something went wrong constructing PSV; 
        PSV length:",length(PSV),"participant_vector length:",length(participant_vector),"\n")
  }
  
  cat("\npart_chop_summary column names:\n
        RR = Raw Reaches, BSUS = Bad SU Samples, ORs = Overruns, Ss = Shorts, 
        S-R = Random Sync, S-F = Feedback Sync, ES-R = Random Sync (end), 
        ES-F = Feedback Sync (end), Fs = Floats, R(f) = Final Reaches, 
        R(r) = Random Reaches, R(fb) = Reaches w/ Feedback, R(L25) = Final 25 Reaches,
        MRLr(ms) = Mean Reach Length, Random (ms), MRLfb(ms) = Mean Reach Length, Feedback (ms)\n" )
  
  part_chop_summary <- matrix( data=PSV, 
                               nrow=length(participant_vector), 
                               ncol=16, 
                               byrow=TRUE )
  colnames(part_chop_summary) <- c( "Participant Number", 
                                    "RR", 
                                    "BSUS", 
                                    "ORs", 
                                    "Ss", 
                                    "S-R", 
                                    "S-F", 
                                    "ES-R", 
                                    "ES-F",
                                    "Fs", 
                                    "R(f)", 
                                    "R(r)", 
                                    "R(fb)", 
                                    "R(L25)", 
                                    "MRLr(ms)", 
                                    "MRLfb(ms)" )
  part_chop_summary <<- data.frame(part_chop_summary)
  
} 
chop_data(TRUE,15)

# Compute Error and put into pTable/rTable/motion tables
# model is pulled out when computing mean error for random reaches
fill_pTable <-function(final_only,useDTW) {
  
  participant_vector <- pTable$number
  
  Participants <- NA
  ReachNum <- NA
  SpatialPathError <- NA # = Mean Reach Spatial Error, i.e mean "path" error in physical space
  SpatialTargetError <- NA # = Mean Final Spatial Error, i.e. mean "target" error in physical space
  RotationPathError <- NA # = Mean Reach Rotation Error, i.e. mean "path" error in rotation space
  RotationTargetError <- NA # = MEan Final Rotation Error, i.e. mean "target" error in rotation space
  
  mr <- max(motionOT$reach,na.rm=TRUE) # Max reach number
  
  # For computing and saving "real" error and normalized error, 
  #   at each point in a reach, defined as distance to its scaled matching model point. 
  motionOT$Rerror <<- rep(NA,length(motionOT$reach))
  motionOT$Nerror <<- rep(NA,length(motionOT$reach))
  motionSU$Rerror <<- rep(NA,length(motionSU$reach))
  motionSU$Nerror <<- rep(NA,length(motionSU$reach))
  
  # Compute spatial and rotation, path and target errors
  for ( i in participant_vector ) {
    
    temp_rv <- motionOT$reach[which(motionOT$participant==i)]
    
    Participants <- c( Participants, rep(i,mr) )
    
    for ( j in 1:mr ) {
      if ( j %in% temp_rv ) {
        ReachNum <- c( ReachNum, j )
      } else {
        ReachNum <- c( ReachNum, NA ) 
      }
    }
    
    cat("Computing stats for participant: ", i,"\n")
    SpatialPathError <- c( SpatialPathError, find_spatial_path_error(FALSE,i,final_only,useDTW) )
    SpatialTargetError <- c( SpatialTargetError, find_spatial_target_error(FALSE,i,final_only) )
    RotationPathError <- c( RotationPathError, find_rotation_path_error(FALSE,i,final_only,useDTW) )
    RotationTargetError <- c( RotationTargetError, find_rotation_target_error(FALSE,i,final_only) )
    
  }
  
  # Built rTable (contains data on reaches)
  Participants <- Participants[2:length(Participants)]
  ReachNum <- ReachNum[2:length(ReachNum)]
  SpatialPathError <- SpatialPathError[2:length(SpatialPathError)]
  SpatialTargetError <- SpatialTargetError[2:length(SpatialTargetError)]
  RotationPathError <- RotationPathError[2:length(RotationPathError)]
  RotationTargetError <- RotationTargetError[2:length(RotationTargetError)]
  Condition <- rep(NA,length(Participants))
  Condition2 <- rep(NA,length(Participants))
  Feedback <- rep(NA,length(Participants))
  for ( i in 1:length(Participants) ) {
    if ( !is.na(ReachNum[i]) && 
         ReachNum[i] <= last_no_feedback_trial_num && 
         pTable$model[which(pTable$number==Participants[i])] != ReachNum[i] 
    ) {
      Condition2[i] <- "R"
      Feedback[i] <- "No"
    } else if ( !is.na(ReachNum[i]) && ReachNum[i] > last_no_feedback_trial_num ) {
      Condition2[i] <- pTable$Condition[which(pTable$number==Participants[i])] 
      Feedback[i] <- "Yes"
    }
  }
  for ( i in 1:length(Participants) ) {
    Condition[i] <- pTable$Condition[which(pTable$number==Participants[i])] 
  }
  rTable <<- data.frame(Participants,
                        Condition,
                        Condition2,
                        Feedback,
                        ReachNum,
                        SpatialPathError,
                        SpatialTargetError,
                        RotationPathError,
                        RotationTargetError)
  rTable$trials <<- rep(NA,length(rTable$Participants))
  for ( p in pTable$number ) {
    rTable$trials[which(rTable$Participants==p)] <<- 1:75
  }
  
} 
fill_pTable(TRUE,TRUE) 

# Need to run this to convert Condition and Condition2 columns to factor for plotting
convert_Condition <- function() {
  for ( i in 1:length(pTable$Condition) ) {
    if (pTable$Condition[i]=="S") {
      pTable$Condition[i] <<- "Sonification"
    } else if (pTable$Condition[i]=="C") {
      pTable$Condition[i] <<- "Faux Sonification"
    }
  }
  pTable$Condition <<- as.factor(pTable$Condition)
  for ( i in 1:length(rTable$Condition) ) {
    if (rTable$Condition[i]=="S") {
      rTable$Condition[i] <<- "Sonification"
    } else if (rTable$Condition[i]=="C") {
      rTable$Condition[i] <<- "Faux Sonification"
    }
  }
  rTable$Condition <<- as.factor(rTable$Condition)
  for ( i in 1:length(rTable$Condition2) ) {
    if (is.na(rTable$Condition2[i])) {
      rTable$Condition2[i] <<- NA
    } else if (rTable$Condition2[i]=="S") {
      rTable$Condition2[i] <<- "Sonification"
    } else if (rTable$Condition2[i]=="C") {
      rTable$Condition2[i] <<- "Faux Sonification"
    } else if (rTable$Condition2[i]=="R") {
      rTable$Condition2[i] <<- "Random Motion"
    }
  }
  rTable$Condition2 <<- as.factor(rTable$Condition2)
  rTable$Feedback <<- as.factor(rTable$Feedback)
}
convert_Condition()

# keep only trial rows in motionOT
motionOT <<- motionOT[!is.na(motionOT$trials),]

#count reaches left after shorts and floats are removed: 
cat("\nReaches left after shorts and floats are removed:", length(which(!is.na(rTable$ReachNum))) )
