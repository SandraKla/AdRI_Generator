###################################################################################################
############ Script for the generation of data with a linear or exponential trend #################
###################################################################################################

#' Round numeric values from a dataframe
#' 
#' @param x Expects dataframe
#' @param digits Digits to round
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

#' Generator for age dependent changing analytes, can be used in the Shiny App AdRI
#'
#' @param age Age range for the simulation
#' @param age_steps Steps for data generation
#' @param distribution Distribution for the data
#' @param n_ Observations for every age step
#' @param name_value Name of the analyte
#' @param formula_mu Trend of mu
#' @param formula_sigma Trend of sigma
#' @param formula_nu Trend of nu
#' @param formula_tau Trend of tau
#' @param ill_factor Value for ill patients (percent)
#' @param mu_factor_ill Value that is added to mu and simulate ill patients (only by Normaldistribution)

make_data <- function(age, age_steps, distribution, 
                      n_, name_value, 
                      formula_mu, formula_sigma, formula_nu, formula_tau, 
                      ill_factor, mu_factor_ill){

  # Linear function with y = slope*x (a) + intercept (b)
  linear <- function(x, a, b){
    x <- x*a+b
    return(x)
  }
  
  # Exponentially function 
  expo <- function(x, a, b){
    x <- a*exp(x*b)
    return(x)
  }
  
  formula_mu    <- parse(text = formula_mu)
  formula_sigma <- parse(text = formula_sigma)
  formula_nu    <- parse(text = formula_nu)
  formula_tau   <- parse(text = formula_tau)
  
  # Fix seed
  set.seed(13)

  # Change Days in Year
  age_steps <- age_steps/365

  # Generates data.frame with all changing parameters (mu, sigma, nu, tau)
  data_para <- data.frame()
  generated_data <- data.frame()
  
  ##################################### Normaldistribution ########################################
  if(distribution == "NO"){
    for(i in seq(0, age, by = age_steps)){
      save_data_para <- data.frame(age = i, mu = eval(formula_mu), sigma = eval(formula_sigma))
      data_para <- rbind(data_para,save_data_para)
      
      # Generation of a table with the age and the associated mu and sigma
      #   age mu sigma
      # 1 0.0  0     1
      # 2 0.5  0     1
      # 3 1.0  0     1
      # 4 1.5  0     1
      # 5 2.0  0     1
      # 6 2.5  0     1
    }
    
    for (i in 1:nrow(data_para)){
      
      # Generate to each row from data_para normally distributed data special also pathologic cases they have a shifted mu value
      # rnorm() for generation for the generation of normally distributed data
      norm_data <- c(rnorm(n=n_,data_para$mu[i],data_para$sigma[i]), 
                     rnorm(n=n_*ill_factor,data_para$mu[i]+mu_factor_ill,data_para$sigma[i]))
      save_table <- data.frame(age = data_para$age[i], value = norm_data)
      generated_data <- rbind(generated_data, save_table)
    }
  }

  ##################################### Log-Normaldistribution ####################################
  if(distribution == "LOGNO"){
    for(i in seq(0, age, by = age_steps)){
      save_data_para <- data.frame(age = i, mu = eval(formula_mu), sigma = eval(formula_sigma))
      data_para <- rbind(data_para,save_data_para)
    }
    
    for (i in 1:nrow(data_para)){
      norm_data <- c(rlnorm(n=n_,data_para$mu[i],data_para$sigma[i]))
      save_table <- data.frame(age = data_para$age[i], value = norm_data)
      generated_data <- rbind(generated_data, save_table)
    }
  }

  ##################################### BCCG Distribution #########################################
  if(distribution == "BCCG"){
    for(i in seq(0, age, by = age_steps)){
      save_data_para <- data.frame(age = i, mu = eval(formula_mu), sigma = eval(formula_sigma), 
                                   nu = eval(formula_nu))
      data_para <- rbind(data_para,save_data_para)
    }
  
    for (i in 1:nrow(data_para)){
      norm_data <- c(rBCCG(n=n_,data_para$mu[i],data_para$sigma[i],data_para$nu[i]))
      save_table <- data.frame(age = data_para$age[i], value = norm_data)
      generated_data <- rbind(generated_data, save_table)
    }
  }

  ##################################### BCPE Distribution #########################################
  if(distribution == "BCPE"){
    for(i in seq(0, age, by = age_steps)){
      save_data_para <- data.frame(age = i, mu = eval(formula_mu), sigma = eval(formula_sigma), 
                                   nu = eval(formula_nu), tau = eval(formula_tau))
      data_para <- rbind(data_para,save_data_para)
    }
  
    for (i in 1:nrow(data_para)){
      norm_data <- c(rBCPE(n=n_,mu=data_para$mu[i],sigma=data_para$sigma[i],nu=data_para$nu[i],tau=data_para$tau[i]))
      save_table <- data.frame(age = data_para$age[i], value = norm_data)
      generated_data <- rbind(generated_data, save_table)
    }
  }

  ##################################### BCT Distribution ##########################################
  if(distribution == "BCT"){
    for(i in seq(0, age, by = age_steps)){
      save_data_para <- data.frame(age = i, mu = eval(formula_mu), sigma = eval(formula_sigma), 
                                   nu = eval(formula_nu), tau = eval(formula_tau))
      data_para <- rbind(data_para,save_data_para)
    }
  
    for (i in 1:nrow(data_para)){
      norm_data <- c(rBCT(n=n_,data_para$mu[i],data_para$sigma[i],data_para$nu[i],data_para$tau[i]))
      save_table <- data.frame(age = data_para$age[i], value = norm_data)
      generated_data <- rbind(generated_data, save_table)
    }
  }

  ##################################### Preprocessing #############################################

  rows_table_ <- nrow(generated_data) # Save nrow from table to get the number of negative values
  generated_data <- generated_data[generated_data$value > 0,] # Eliminate negative values
  
  if(!(rows_table_ == nrow(generated_data))){
    print(paste("Warning!", rows_table_ - nrow(generated_data) ,"values were negative and are deleted."))}

  ##################################### Save the data #############################################
  
  table_generator <- data.frame(ALTER = round_df(generated_data$age,3), 
                           ALTERTAG = generated_data$age * 365,
                           ERGEBNIST1 = round_df(generated_data$value,3))
  table_generator["PATISTAMMNR"] <- seq(1, nrow(table_generator)) # All values are unique
  table_generator["SEX"] <- "NA" # Sex is NA 
  table_generator["EINSCODE"] <- "Generator" 
  table_generator["CODE1"] <- name_value

  return(table_generator)
}