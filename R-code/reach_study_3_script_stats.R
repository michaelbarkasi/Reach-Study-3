# Stat plots and analysis

improvement_stats <- function(errorimprovement,alt,name,dunits,plotname) {
  
  pTable$Condition <- relevel(pTable$Condition, "Sonification")
  
  cat("\n")
  cat("Stat Error Improvement Analysis for: ", name, "\n")
  cat("\n")
  
  # Step 1: Check assumptions for both one-sample and Welch two-sample t-tests (outliers and normality)
  cat("Checking assumptions for both one-sample and Welch two-sample t-tests.\n")
  cat("\n")
  
  # test for outliers
  cat("Checking for outliers (>3 sd from mean).\n")
  passedSon <- "No"
  passedFau <- "No"
  outlier_indexSon <- index_outliers(errorimprovement[which(pTable$Condition=="Sonification")],3)
  outlier_indexFau <- index_outliers(errorimprovement[which(pTable$Condition=="Faux Sonification")],3)
  if (length(outlier_indexSon) == 0 ) {
    passedSon <- "Yes"
  } else {
    passedSon <- "No"
  }
  if (length(outlier_indexFau) == 0 ) {
    passedFau <- "Yes"
  } else {
    passedFau <- "No"
  }
  cat("Number of outliers in Son:", length(outlier_indexSon), "Passed?", passedSon, "\n")
  cat("Number of outliers in Fau:", length(outlier_indexFau), "Passed?", passedFau, "\n")
  cat("\n")
  
  # test for normality (package rstatix)
  cat("Checking for normality w/ Shapiro-Wilk Test (p>0.05):\n")
  strSon <- shapiro_test(errorimprovement[which(pTable$Condition=="Sonification")])
  strFau <- shapiro_test(errorimprovement[which(pTable$Condition=="Faux Sonification")])
  if (strSon$p.value>0.05) {
    passedSon <- "Yes"
  } else {
    passedSon <- "No"
  }
  if (strFau$p.value>0.05) {
    passedFau <- "Yes"
  } else {
    passedFau <- "No"
  }
  cat("p in Shapiro-Wilk for Son:", strSon$p.value, "Passed?", passedSon, "\n")
  cat("p in Shapiro-Wilk for Fau:", strFau$p.value, "Passed?", passedFau, "\n")
  cat("\n")
  
  # Step 2: Run a Welch two-sample t-test w/ alt hyp that Son improvement is greater than Faux Son improvement
  cat("Running Welch Two-sample t-test w/ alt hypothesis that Son improvement is", alt_type, "than Faux Son improvement.\n")
  cat("\n")
  
  ttresult <- t.test(
                errorimprovement ~ Condition,
                data = pTable,
                alternative = alt,
                paired = FALSE,
                var.equal = FALSE,
                conf.level = 0.95,
                mu = 0
              )
  
  cat("Results of Welch two-sample t-test for: ", name, "\n")
  cat("Son Mean: ", ttresult$estimate[1],
      "Faux Mean: ", ttresult$estimate[2],"\n",
      "t-stat: ", ttresult$statistic,
      "Deg of Freedom: ", ttresult$parameter,
      "p-value: ", ttresult$p.value,"\n",
      "95% Conf Int: ", ttresult$conf.int,"\n",
      "Alt Hyp: ", "true difference in means between group Sonification and group Faux Sonification is", ttresult$alternative,"than 0.\n",
      "Method: ", ttresult$method,"\n")
  
  cat("\n")
  cat("Standard Deviation:\n")
  cat("Son: ", sd(errorimprovement[which(pTable$Condition=="Sonification")]), "\n")
  cat("Faux: ", sd(errorimprovement[which(pTable$Condition=="Faux Sonification")]), "\n")
  cat("\n")
  
  # Step 2: Run a one-sample t-test on both Son and Fau improvement w/ alt hyp that improvement is greater than 0. 
  cat("Running one-sample t-tests for both Son and Faux w/ alt hypothesis that improvement is", alt_type, "than 0.\n")
  cat("\n")
  
  ttresultS <- t.test(
                  errorimprovement[which(pTable$Condition=="Sonification")],
                  alternative = alt,
                  conf.level = 0.95,
                  mu = 0
                )
  
  ttresultF <- t.test(
                  errorimprovement[which(pTable$Condition=="Faux Sonification")],
                  alternative = alt,
                  conf.level = 0.95,
                  mu = 0
                )
  
  cat("Results of one-sample t-test for Son: ", name, "\n")
  cat("Son Mean: ", ttresultS$estimate,"\n",
      "t-stat: ", ttresultS$statistic,
      "Deg of Freedom: ", ttresultS$parameter,
      "p-value: ", ttresultS$p.value,"\n",
      "95% Conf Int: ", ttresultS$conf.int,"\n",
      "Alt Hyp: ", "true mean is", ttresultS$alternative,"than 0.\n",
      "Method: ", ttresultS$method,"\n")
  cat("\n")
  
  cat("Results of one-sample t-test for Faux: ", name, "\n")
  cat("Faux Mean: ", ttresultF$estimate,"\n",
      "t-stat: ", ttresultF$statistic,
      "Deg of Freedom: ", ttresultF$parameter,
      "p-value: ", ttresultF$p.value,"\n",
      "95% Conf Int: ", ttresultF$conf.int,"\n",
      "Alt Hyp: ", "true mean is", ttresultF$alternative,"than 0.\n",
      "Method: ", ttresultF$method)
  
  # Now plot results, along with a box plot showing quartiles and distribution
  
  par(mfrow=c(1,2),
      mar = c(5.1, 5.0, 4.1, 0.5)) # default: c(5.1, 4.1, 4.1, 2.1)
  
  improvement <- c(ttresult$estimate[1],ttresult$estimate[2])
  condition <- c("Sonification", "Faux Sonification")
  df <- data.frame(improvement,
                   condition)
  df$condition <- as.factor(df$condition)
  df$condition <- relevel(df$condition, "Sonification")
  
  ymin <- min(errorimprovement) - 0.05
  ymax <- max(errorimprovement) + 0.05
  
  mean_plot(df$condition,
            df$improvement,
            c(ttresultS$conf.int[1],ttresultF$conf.int[1]),
            "means",
            c(ymin,ymax),
            dunits,
            pvsignf(ttresult$p.value),
            pvsignf(ttresultF$p.value),
            pvsignf(ttresultS$p.value))
  boxplot_jitter(pTable$Condition,
                 errorimprovement,
                 "quartiles",
                 c(ymin,ymax),
                 dunits)
  
  cat("\n")
  cat("Saving plots.\n")
  dev.print(png, plotname, width=1000, height=500)
  
}
cat("\nRunning Improvement Stats:\n")
cat("Run w/ two-sided t-tests first to check for effects in the opposite direction:\n")
alt_type <- "two.sided"
improvement_stats(pTable$SpatialTargetErrorI,
                  alt_type,
                  "Spatial Target Error",
                  "distance (cm)",
                  "plot_spatialtargeterror_improvement.png")
improvement_stats(pTable$SpatialPathErrorI,
                  alt_type,
                  "Spatial Path Error",
                  "distance (cm)",
                  "plot_spatialpatherror_improvement.png")
improvement_stats(pTable$RotationTargetErrorI,
                  alt_type,
                  "Rotation Target Error",
                  "distance (unit quats)",
                  "plot_rotationtargeterror_improvement.png")
improvement_stats(pTable$RotationPathErrorI,
                  alt_type,
                  "Rotation Path Error",
                  "distance (unit quats)",
                  "plot_rotationpatherror_improvement.png")

cat("\nRun w/ one-sided t-tests as explained in paper:\n")
alt_type <- "greater"
improvement_stats(pTable$SpatialTargetErrorI,
                  alt_type,
                  "Spatial Target Error",
                  "distance (cm)",
                  "plot_spatialtargeterror_improvement.png")
improvement_stats(pTable$SpatialPathErrorI,
                  alt_type,
                  "Spatial Path Error",
                  "distance (cm)",
                  "plot_spatialpatherror_improvement.png")
improvement_stats(pTable$RotationTargetErrorI,
                  alt_type,
                  "Rotation Target Error",
                  "distance (unit quats)",
                  "plot_rotationtargeterror_improvement.png")
improvement_stats(pTable$RotationPathErrorI,
                  alt_type,
                  "Rotation Path Error",
                  "distance (unit quats)",
                  "plot_rotationpatherror_improvement.png")

error_means_and_plots <- function(errorNF,errorFB,name,dunits,plotname) {
  
  # Construct temporary data frames (for Son and Faux Son):
  error <- c(errorNF[which(pTable$Condition=="Faux Sonification")],
             errorNF[which(pTable$Condition=="Sonification")],
             errorFB[which(pTable$Condition=="Faux Sonification")],
             errorFB[which(pTable$Condition=="Sonification")])
  lNFS <- length(errorNF[which(pTable$Condition=="Sonification")])
  lFBS <- length(errorFB[which(pTable$Condition=="Sonification")])
  lNFF <- length(errorNF[which(pTable$Condition=="Faux Sonification")])
  lFBF <- length(errorFB[which(pTable$Condition=="Faux Sonification")])
  if ( lNFS != lFBS ) {
    cat("Warning! Something went wrong.")
    return()
  }
  if ( lNFF != lFBF ) {
    cat("Warning! Something went wrong.")
    return()
  }
  feedback <- c( rep("No",lNFF+lNFS), rep("Yes",lFBF+lFBS) )
  condition <- c( rep("Faux Sonification",lNFF),
                  rep("Sonification",lNFS),
                  rep("Faux Sonification",lFBF),
                  rep("Sonification",lFBS))
  df <- data.frame(feedback,
                   condition,
                   error)
  df$feedback <- as.factor(df$feedback)
  df$feedback <- relevel(df$feedback, "No")
  df$condition <- as.factor(df$condition)
  df$condition <- relevel(df$condition, "Faux Sonification")
  
  cat("\n")
  cat("Finding Mean Values for", name, "\n")
  
  par(mfrow=c(1,2),
      mar = c(5.1, 5.0, 4.1, 0.5)) # default: c(5.1, 4.1, 4.1, 2.1)
  
  mean_errorFau <- c(mean(df$error[which(df$feedback=="No" & df$condition=="Faux Sonification")]),
                     mean(df$error[which(df$feedback=="Yes" & df$condition=="Faux Sonification")]))
  mean_errorSon <- c(mean(df$error[which(df$feedback=="No" & df$condition=="Sonification")]),
                     mean(df$error[which(df$feedback=="Yes" & df$condition=="Sonification")]))
  
  cat("Mean Error Faux Son No Feedback:", mean_errorFau[1],"\n")
  cat("Mean Error Faux Son Feedback:", mean_errorFau[2],"\n")
  cat("Mean Error Son No Feedback:", mean_errorSon[1],"\n")
  cat("Mean Error Son Feedback:", mean_errorSon[2],"\n")
  
  sd_errorFau <- c(sd(df$error[which(df$feedback=="No" & df$condition=="Faux Sonification")]),
                     sd(df$error[which(df$feedback=="Yes" & df$condition=="Faux Sonification")]))
  sd_errorSon <- c(sd(df$error[which(df$feedback=="No" & df$condition=="Sonification")]),
                     sd(df$error[which(df$feedback=="Yes" & df$condition=="Sonification")]))
  
  cat("SD Error Faux Son No Feedback:", sd_errorFau[1],"\n")
  cat("SD Error Faux Son Feedback:", sd_errorFau[2],"\n")
  cat("SD Error Son No Feedback:", sd_errorSon[1],"\n")
  cat("SD Error Son Feedback:", sd_errorSon[2],"\n")
  
  feedback <- c("No", "Yes")
  feedback <- as.factor(feedback)
  feedback <- relevel(feedback, "No")
  
  cat("\n")
  cat("Making plots for", name, "\n")
  
  ymin <- 0
  ymax <- max(df$error) * 1.2
  
  mean_plot_fb(feedback, 
               mean_errorFau, 
               mean_errorSon, 
               "means", 
               c(ymin,ymax),
               dunits
              )
  
  default_size <- 2
  axissize <- default_size
  boxplot(error~condition*feedback,
          data=df,
          ylim = c(ymin,ymax),
          ylab = dunits,
          xlab = "feedback",
          main = "quartiles",
          cex.main = default_size,
          cex.lab = default_size,
          col = c("red3","green3"),
          at = c(1.25,1.75,3.25,3.75),
          axes=FALSE)
  abline(h=0,col = "gray", lty = "dashed", lwd=3)
  xpoints <- seq(median(1:2),4,2)
  axis(side = 1, 
       labels = c("No", "Yes"),
       cex.axis = axissize,
       at = xpoints
  )
  axis(side=2,cex.axis=axissize)
  text(1.25,ymax*0.05,"Faux",cex=default_size*0.5)
  text(1.75,ymax*0.05,"Son",cex=default_size*0.5)
  text(3.25,ymax*0.05,"Faux",cex=default_size*0.5)
  text(3.75,ymax*0.05,"Son",cex=default_size*0.5)
  
  cat("\n")
  cat("Saving plots.\n")
  dev.print(png, plotname, width=1000, height=500)

}
error_means_and_plots(pTable$SpatialTargetErrorNF,
                      pTable$SpatialTargetErrorF,
                      "Spatial Target Error",
                      "distance (cm)",
                      "plot_spatialtargeterror.png")
error_means_and_plots(pTable$SpatialPathErrorNF,
                      pTable$SpatialPathErrorF,
                      "Spatial Path Error",
                      "distance (cm)",
                      "plot_spatialpatherror.png")
error_means_and_plots(pTable$RotationTargetErrorNF,
                      pTable$RotationTargetErrorF,
                      "Rotation Target Error",
                      "distance (unit quats)",
                      "plot_rotationtargeterror.png")
error_means_and_plots(pTable$RotationPathErrorNF,
                      pTable$RotationPathErrorF,
                      "Rotation Path Error",
                      "distance (unit quats)",
                      "plot_rotationpatherror.png")

# Misc: 
cat("\nBox formed by min/max values on each OT axis:\n")
cat("x:", min(motionOT$x,na.rm=TRUE), ",", max(motionOT$x,na.rm=TRUE), "\n")
cat("y:", min(motionOT$y,na.rm=TRUE), ",", max(motionOT$y,na.rm=TRUE), "\n")
cat("z:", min(motionOT$z,na.rm=TRUE), ",", max(motionOT$z,na.rm=TRUE), "\n")

# Find sd in spatial target for each participant in Faux Son condition
FauxSonResponses <- function() {
  dev.off()
  cat("\nStandard deviation in spatial target error for each participant in Faux Son\n")
  
  FSsd <- 1
  
  for ( i in 1:length(pTable$number) ) {
    p <- pTable$number[i]
    if ( pTable$Condition[i] == "Faux Sonification" ) {
      index <- which(rTable$Participants==p & 
                       rTable$ReachNum >= first_postlearning_feedback_trial_num)
      SD <- sd(rTable$SpatialTargetError[index],na.rm = TRUE)
      FSsd <- c( FSsd, SD )
      cat("P: ", p, "sd: ", SD, "\n")
    }
  }
  
  FSsd <- FSsd[2:length(FSsd)]
  
  hist(FSsd,breaks=10,main="Histogram of Spatial Target Error SD in Faux Sonification",xlab="standard deviation", ylab="number of participants")
  
}
FauxSonResponses()
dev.print(png, "plot_fauxson_STEsd.png", width=500, height=500)
