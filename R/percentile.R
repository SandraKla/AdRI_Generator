###################################################################################################
############ Script for the generation of given reference intervals ###############################
###################################################################################################

#' Generator for age-dependent values with given 95% reference intervals
#' Needed: Data with reference intervals (smaller age steps are better) from a normally 
#' distrubted analyte, because the function generate normally distributed data from this reference intervals!
#'  
#' @param data_percentile Data with the reference intervals in .csv format
#' @param n_ Number of observations
#' @param text_name Name of the analyte
#' @param text_unit Unit of the analyte

percentile_function <- function(data_percentile, n_, text_name, text_unit){
  
  # Read the data
  reference_data_orig <- read.csv2(data_percentile, header = TRUE, sep = ";", dec = ",", na.strings = "", stringsAsFactors = FALSE)
  
  # Change the colnames and type of the columns
  colnames(reference_data_orig) <- c("age", "down", "up")
  
  #' Dataset (reference_data_orig):
  #'        age down up
  #' 1      365    2  6
  #' 2      730    2  6
  #' 3     1095    3  6
  #' 4     1460    4  6
  #' 5     1825    4  6
  #' 6     2190    5  6
  
  reference_data <- data.frame(age = as.integer(reference_data_orig$age), 
                               down = as.numeric(reference_data_orig$down), 
                               up = as.numeric(reference_data_orig$up))

  # Get sigma and mu from normally distrubted data
  sigma <- (reference_data$up-reference_data$down)/(1.96--1.96) 
  # (Q97.5 - Q2.5)/sigma =  (pnorm(97.5)-pnorm(2.5))/1 = (1.96--1.96)/1
  mu  <- reference_data$up - 1.96*sigma
  # mu <- reference_data$down + 1.96*sigma
  
  # Save the mu and sigma for each datapoint
  reference_para <- data.frame(age = reference_data$age, mu = mu, sigma = sigma) 
  
  #    age  mu     sigma
  # 1  365 4.0 1.0204082
  # 2  730 4.0 1.0204082
  # 3 1095 4.5 0.7653061
  # 4 1460 5.0 0.5102041
  # 5 1825 5.0 0.5102041
  # 6 2190 5.5 0.2551020
  
  # Fix seed
  set.seed(13)

  # Generates the data with the mu and sigma values
  table_data_reference <- data.frame()
  
  for (i in 1:nrow(reference_para)){
    # Make normally distributed data
    percentile_data <- c(rnorm(n=n_,reference_para$mu[i],reference_para$sigma[i]))
    save_table <- data.frame(age = reference_para$age[i], value = percentile_data)
    table_data_reference <- rbind(table_data_reference, save_table)
  }
  
  plot(table_data_reference$age, table_data_reference$value, 
       xlab = "Age [Days]", ylab =  paste0(text_name," [",text_unit,"]"), 
       pch = 20, cex = 0.75, col = "grey", ylim = c(0, max(reference_data$up) + max(reference_data$up)/10))

  #lines(smooth.spline(table_data_reference$age,table_data_reference$value))
  lines(reference_data$age, reference_data$down, col = "indianred")
  lines(reference_data$age, reference_data$up, col = "cornflowerblue")
 
  ##################################### Preprocessing #############################################
  
  rows_table_data_reference <- nrow(table_data_reference) # Save nrow from table
  table_data_reference <- table_data_reference[table_data_reference$value > 0,] # Delete negative values
  
  if(!(rows_table_data_reference == nrow(table_data_reference))){
    print(paste("Warning!", rows_table_data_reference - nrow(table_data_reference) ,"values were negative and are deleted"))}

  ##################################### Save the data #############################################
  
  table_percentile <- data.frame(ALTER = round_df(table_data_reference$age/365,3), 
                                 ALTERTAG = table_data_reference$age, 
                                 ERGEBNIST1 = round_df(table_data_reference$value,3))
  table_percentile["PATISTAMMNR"] <- seq(1, nrow(table_percentile))
  table_percentile["SEX"] <- "NA"
  table_percentile["EINSCODE"] <- "Generator"
  table_percentile["CODE1"] <- text_name

  return(table_percentile)
}

#' Generator for age dependent values for hemoglobin from the publication:
#' Zierk J, Hirschmann J, Toddenroth D, et al. Next-generation reference
#' intervals for pediatric hematology. Clin Chem Lab Med. 2019;57(10):1595‐1607. doi:10.1515/cclm-2018-123
#' 
#' @param reference_ Dataset with hemoglobin data (women and men)
#' @param n_percentile Number of observations

percentile_hemoglobin <- function(reference_, n_percentile = 10){
  
  par(mfrow = c(2,1)) 
  
  # Fix seed
  set.seed(13)
  
  colnames(reference_) <- c("age", "down", "up")
  reference_data <- data.frame(age = as.integer(reference_$age), 
                               down = as.numeric(reference_$down), 
                               up = as.numeric(reference_$up))

  sigma <- (reference_data$up-reference_data$down)/(1.96--1.96)
  # (Q97.5 - Q2.5)/sigma =  (pnorm(97.5)-pnorm(2.5))/1 = (1.96--1.96)/1
  mu <- reference_data$up - 1.96*sigma

  ##################################### Save data from the hemoglobin #############################  
  
  reference_para <- data.frame(age = reference_data$age, mu = mu, sigma = sigma)

  table_reference_data <- data.frame()
  
  for (i in 1:nrow(reference_para)){
    percentile_data <- c(rnorm(n=n_percentile, reference_para$mu[i], reference_para$sigma[i]))
    save_table <- data.frame(age = reference_para$age[i]-1, value = percentile_data)
    table_reference_data <- rbind(table_reference_data, save_table)
  }
  
  table_save <- table_reference_data
  
  ##################################### Data from 0 - 180 Days ####################################

  reference_data_newborn <- subset(reference_data, age <= 180, select = c(age, down, up)) 
  
  sigma <- (reference_data_newborn$up-reference_data_newborn$down)/(1.96--1.96)
  mu <- reference_data_newborn$up - 1.96*sigma
  
  reference_para_newborn <- data.frame(age = reference_data_newborn$age, mu = mu, sigma = sigma)
  table_newborn <- data.frame()

  for (i in 1:nrow(reference_para_newborn)){
    norm_data <- c(rnorm(n=n_percentile,reference_para_newborn$mu[i],reference_para_newborn$sigma[i]))
    save_table <- data.frame(age = reference_para_newborn$age[i]-1, value = norm_data)
    table_newborn <- rbind(table_newborn, save_table)
  }
  
  plot(table_newborn$age, table_newborn$value, pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]", ylab = "Hemoglobin [g/dl]")
  lines(reference_para_newborn$age, reference_para_newborn$mu, col = "indianred", lwd = 2)
  lines(reference_data_newborn$age, reference_data_newborn$down, col = "cornflowerblue", lwd = 2)
  lines(reference_data_newborn$age, reference_data_newborn$up, col = "cornflowerblue", lwd = 2)
  
  ##################################### Data from 1 - 18 Years ####################################
  
  reference_data_children <- subset(reference_data, age >= 365, select = c(age, down, up)) 
  
  sigma <- (reference_data_children$up-reference_data_children$down)/(1.96--1.96)
  mu <- reference_data_children$up - 1.96*sigma
  
  reference_para_children <- data.frame(age = reference_data_children$age, mu = mu, sigma = sigma)
  table_children <- data.frame()

  for (i in 1:nrow(reference_para_children)){
    norm_data <- c(rnorm(n=n_percentile/10,reference_para_children$mu[i],reference_para_children$sigma[i]))
    save_table <- data.frame(age = reference_para_children$age[i]-1, value = norm_data)
    table_children <- rbind(table_children, save_table)
  }
  
  plot(table_children$age, table_children$value, pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]", ylab = "Hemoglobin [g/dl]")
  lines(reference_para_children$age, reference_para_children$mu, col = "indianred", lwd = 2)
  lines(reference_data_children$age, reference_data_children$down, col = "cornflowerblue", lwd = 2)
  lines(reference_data_children$age, reference_data_children$up, col = "cornflowerblue", lwd = 2)
  
  return(table_save)
}