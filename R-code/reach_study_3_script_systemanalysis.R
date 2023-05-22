
# Sonification System Analysis

cat("\nRunning analysis of sonification system.\n")

benchmark_data <- read_csv("reach-study-3-benchmarking_test_May18_2023.csv", 
                     col_types = cols(.default = col_double()))
cat("\nFor summary of benchmark data, run summary(benchmark_data) from within script.\n")
summary(benchmark_data)

cat("Analyzing sample rates:\n")
sample_rates <- read_csv("reach-study-3-unitsamplerates.csv", 
                           col_types = cols(.default = col_double()))
unit_sample_rates <- function() {
  
  r <- sum(!is.na(sample_rates))
  l <- sum(sample_rates <= 999,na.rm = TRUE)
  h <- sum(sample_rates >= 1001,na.rm = TRUE)
  m <- mean(as.matrix(sample_rates),na.rm=TRUE)
  mi <- min(as.matrix(sample_rates),na.rm=TRUE)
  ma <- max(as.matrix(sample_rates),na.rm=TRUE)
  
  cat("\nreaches (lost overruns):", r,"\n")
  cat("mean sample rate:", m,"\n")
  cat("min sample rate:", mi,"\n")
  cat("max sample rate:", ma,"\n")
  cat("number of reaches w/ rate <= 999:", l,"\n")
  cat("number of reaches w/ rate >= 1001:", h,"\n")
  cat("\n")
  
}
unit_sample_rates()

cat("\nRunning comparison of normalized error between systems.\n")
# OT vs SU analysis
# Must run this to match up SU and OT normalized errors
compare_normalized_errors <- function() {
  
  cat("Matching up each OT normalized error with corresponding SU normalized error.\n")
  
  motionOT$NerrorSU <<- rep(NA,length(motionOT$reach))
  
  for ( p in pTable$number ) {
    reaches <- rTable$ReachNum[which(rTable$Participants==p)]
    reaches <- reaches[which(!is.na(reaches))]
    for ( r in reaches ) {
      nerrorOT <- motionOT$Nerror[which(motionOT$participant==p & motionOT$reach==r)]
      nerrorSU <- motionSU$Nerror[which(motionSU$participant==p & motionSU$reach==r)]
      rlOT <- length(nerrorOT)
      rlSU <- length(nerrorSU)
      OTindex <- min(which(motionOT$participant==p & motionOT$reach==r),na.rm = TRUE)
      for ( e in 1:rlOT ) {
        if ( !is.na(nerrorOT[e]) ) {
          # find corresponding normalized error in SU
          index_ratio <- e/rlOT
          SUindex <- as.integer(index_ratio * rlSU)
          if ( SUindex == 0 ) SUindex <- 1
          if ( !is.na(nerrorSU[SUindex]) ) {
            motionOT$NerrorSU[OTindex+(e-1)] <<- nerrorSU[SUindex]
          }
        }
      }
    }
  }
  
}
compare_normalized_errors()

# Test for normality and outliers before running correlational analysis
error_correlation_assumption_check <- function() {
  
  # pull out NAs
  nas <- which(is.na(motionOT$Nerror) | is.na(motionOT$NerrorSU))
  Nerror <- motionOT$Nerror[-nas]
  NerrorSU <- motionOT$NerrorSU[-nas]
  
  cat("Number of OT data points (w/o NAs): ",length(Nerror),"\n")
  cat("Number of SU data points (w/o NAs): ",length(NerrorSU),"\n")
  
  # test for outliers
  cat("\nChecking for outliers (>0.3% at >3 sd from mean), based on the 68–95–99.7 rule.\n")
  
  passedOT <- "No"
  passedSU <- "No"
  outlier_indexOT <- index_outliers(Nerror,3)
  outlier_indexSU <- index_outliers(NerrorSU,3)
  if (length(outlier_indexOT) < length(Nerror) * 0.003 ) {
    passedOT <- "Yes"
  } else {
    passedOT <- "No"
  }
  if (length(outlier_indexSU) < length(Nerror) * 0.003 ) {
    passedSU <- "Yes"
  } else {
    passedSU <- "No"
  }
  cat("\nNumber of outliers in OT data:", length(outlier_indexOT), "Passed?", passedOT, "\n")
  cat("Number of outliers in SU data:", length(outlier_indexSU), "Passed?", passedSU, "\n")

  pass <- 0
  while ( passedOT == "No" || passedSU == "No" ) {
    
    pass <- pass+1
    cat("\nRemoving outliers, pass:", pass, "\n")
    outliers <- c(outlier_indexOT,outlier_indexSU)
    Nerror <- Nerror[-outliers]
    NerrorSU <- NerrorSU[-outliers]
    
    outlier_indexOT <- index_outliers(Nerror,3)
    outlier_indexSU <- index_outliers(NerrorSU,3)
    if (length(outlier_indexOT) < length(Nerror) * 0.003 ) {
      passedOT <- "Yes"
    } else {
      passedOT <- "No"
    }
    if (length(outlier_indexSU) < length(Nerror) * 0.003 ) {
      passedSU <- "Yes"
    } else {
      passedSU <- "No"
    }
    cat("\nNumber of outliers in OT data:", length(outlier_indexOT), "Passed?", passedOT, "\n")
    cat("Number of outliers in SU data:", length(outlier_indexSU), "Passed?", passedSU, "\n")
    cat("\n")
    cat("Number of samples after outliers removed:\n")
    cat("Number of OT data points (w/o NA): ",length(Nerror),"\n")
    cat("Number of SU data points (w/o NA): ",length(NerrorSU),"\n")
    
    # Save for use in the next steps: 
    
    Nerror <<- Nerror
    NerrorSU <<- NerrorSU
    
  }
  
  # test for normality (package rstatix)
  cat("\nChecking for normality w/ histograms:\n")
  hist(motionOT$Nerror)
  hist(motionOT$NerrorSU)
  cat("Normality clearly fails.\n") 
  cat("\n")
  
}
error_correlation_assumption_check()

# Now find correlation between SU and OT error, either all together or by participant
# Note: The normalized errors are computed using linear rescaling, not DTW
corr_test_spearman <- function() {
  
  c <- cor.test(Nerror, NerrorSU, 
                method = "spearman",
                exact = FALSE,
                na.rm = TRUE )
  cat("Finding correlation between SU and OT normalized error values for each reach point using Spearman.\n")
  cat("\nError E of sample S from reach R by participant P was normalized by dividing E by the length of P's model reach.\n")
  cat("\nNote if S is sample n and R has length L, E is the linear distance between S and the as.integer(n/L) model point; DTW was not used to match reach and model points.\n")
  cat("\nCorrelation (rho): ", c$estimate, "\n")
  cat("Deg of Freedom: ", c$parameter, "\n")
  cat("p value: ", c$p.value, "\n")
  cat("\nFor full correlation results, run corr_test separately in R script.\n")
  return(c)
}
corr_test_pearson <- function() {

  c <- cor.test(Nerror, NerrorSU, 
                method = "pearson",
                na.rm = TRUE )
  cat("\nFinding correlation between SU and OT normalized error values for each reach point using Pearson.\n")
  cat("\nError E of sample S from reach R by participant P was normalized by dividing E by the length of P's model reach.\n")
  cat("\nNote if S is sample n and R has length L, E is the linear distance between S and the as.integer(n/L) model point; DTW was not used to match reach and model points.\n")
  cat("\nCorrelation (r): ", c$estimate, "\n")
  cat("Deg of Freedom: ", c$parameter, "\n")
  cat("p value: ", c$p.value, "\n")
  par(mar = c(5.1, 5.0, 4.1, 2.1)) # default: c(5.1, 4.1, 4.1, 2.1)
  plot.default(Nerror,NerrorSU,
       col=rgb(red = 0, green = 0, blue = 0, alpha = 0.05),
       pch=16,
       main="Comparison of Normalized Error Between Systems",
       xlab="OptiTrack",
       ylab="Sonification System",
       axes = FALSE,
       cex.lab=1.75,cex.main=2)
  #abline(a=0, b=1,col="blue",lwd=2)
  axis(side=1,cex.axis=1.75)
  axis(side=2,cex.axis=1.75)
  par(mar = c(5.1, 4.1, 4.1, 2.1)) # default: c(5.1, 4.1, 4.1, 2.1)
  cat("\nFor full correlation results, run corr_test separately in R script.\n")
  return(c)
}
corr_test_spearman()
corr_test_pearson()
dev.print(png, "plot_Nerror_comparison.png", width=1000, height=700)

cat("\nRunning analysis of time warping.\n")
# Compare no time-warping to system's real-time time-warping to DTW
# Looks at only sonification reaches
DTW_MOT_analysis <- function() {
  
  # uses all and only reaches with real sonification feedback
  
  #dev.off()
  
  par(mfrow=c(1,3),
      mar = c(5.1, 5.0, 4.1, 2.1)) # default: c(5.1, 4.1, 4.1, 2.1)
  
  MOT <- motionSU$MOTsample
  MOTN <- motionSU$MOTsample
  part <- motionSU$participant
  reach <- motionSU$reach
  samp <- motionSU$sample
  sampN <- motionSU$sample
  sampMN <- motionSU$sample
  
  MOTL <- part
  
  cat("\nNormalizing MOT estimate\n")
  for ( i in 1:length(pTable$number) ) {
    pp <- pTable$number[i]
    mm <- pTable$model[i]
    ml <- length(which(motionSU$reach==mm & motionSU$participant==pp))
    ii <- which(part == pp)
    MOTL[ii] <- ml
  }
  MOTN <- MOT / MOTL # each estimated model point is encoded as a fraction of the model length
  sampMN <- samp / MOTL # each reach sample point is encoded as a fraction of its model's length, representing no time-warping
  sampMN[which(sampMN > 1)] <- 1 
  
  # Only looking at sonification participants
  son_part <- pTable$number[which(pTable$Condition=="Sonification")]
  
  indexes <- which(!is.na(reach) & reach > last_no_feedback_trial_num & part %in% son_part)

  cat("Normalizing sample numbers")
  cat("\nParticipant: ")
  lg <- samp 
  for ( PP in pTable$number ) {
    if ( PP %in% son_part ) {
      cat(PP,", ")
      for ( rr in rTable$ReachNum[which(rTable$Participants==PP)] ) {
        if ( !is.na(rr) && rr > last_no_feedback_trial_num ) {
          ii <- which(part==PP & reach==rr)
          llg <- length(ii)
          lg[ii] <- llg
        }
      }
    }
  }
  sampN <- samp / lg # each reach sample point is encoded as a fraction of reach length
  
  cat("\nBuilding DTW series")
  cat("\nParticipant: ")
  sampRB <- 1
  MOTRB <- 1
  for ( PP in pTable$number ) {
    if ( PP %in% son_part ) {
      cat(PP,", ")
      for ( rr in rTable$ReachNum[which(rTable$Participants==PP)] ) {
        if ( !is.na(rr) && rr > last_no_feedback_trial_num ) {
        
          m_num <- pTable$model[which(pTable$number==PP)]
          r1i <- which(motionSU$participant==PP & motionSU$reach==rr)
          rv <- motionSU$RotVelocity[r1i]
          mi <- which(motionSU$participant==PP & motionSU$reach==m_num)
          mv <- motionSU$RotVelocity[mi]
          
          rv <- na.omit(rv)
          mv <- na.omit(mv)
          
          d <- dtw(mv,rv)
          
          # the reach velocity series is x, the model velocity series is y, 
          #   so d$index2 gives the *model* indexes that minimize "cost" when 
          #   paired w/ unwarped reach indexes
          
          ri <- d$index2 /length(rv) #d$index1/length(d$index1)
          miTW <- d$index1 /length(mi) 
          
          sampRB <- c( sampRB, ri )
          MOTRB <- c( MOTRB, miTW ) 
          
        }
      }
    }
  }
  sampRB <- sampRB[2:length(sampRB)]
  MOTRB <- MOTRB[2:length(MOTRB)]
  
  cat("\nPrinting")
  plot(sampN[indexes],sampMN[indexes],
       col=rgb(red = 0, green = 0, blue = 0, alpha = 0.05),
       xlab = "normalized sample number",ylab = "normalized model number",pch=16,
       main="Model Estimates without Time-Warping",
       cex.lab=2.5,cex.main=2.75,
       axes=FALSE)
  axis(side=1,cex.axis=2.5)
  axis(side=2,cex.axis=2.5)
  plot(sampN[indexes],MOTN[indexes],
       col=rgb(red = 0, green = 0, blue = 0, alpha = 0.05),
       xlab = "normalized sample number",ylab = "normalized time-warped model number",pch=16,
       main="Real-Time Model Time-Warping Estimates",
       cex.lab=2.5,cex.main=2.75,
       axes=FALSE)
  axis(side=1,cex.axis=2.5)
  axis(side=2,cex.axis=2.5)
  plot(sampRB,MOTRB,
       col=rgb(red = 0, green = 0, blue = 0, alpha = 0.05),
       xlab = "normalized sample number",ylab = "normalized time-warped model number",pch=16,
       main="Model Dynamic Time-Warping by Velocity",
       cex.lab=2.5,cex.main=2.75,
       axes=FALSE)
  axis(side=1,cex.axis=2.5)
  axis(side=2,cex.axis=2.5)
  
}
DTW_MOT_analysis()
dev.print(png, "plot_DTW_comparison.png", width=2100, height=700)
