# Package load ----
library(snowfall)
library(anadrofish)
library(tidyverse)

# Progress and benchmarking ----
# Record system time for total_time calculation at end
start <- Sys.time()
start

# Number of times to run the parallel simulation
n_iter <- 100 

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      
                     max = n_iter, 
                     style = 3,    
                     width = 50,   
                     char = "=")   

for(i in 1:n_iter) {
  
  # Parallel settings ----
  # Get number of cores
  ncpus <- 10
    
  # Initialize snowfall
  suppressMessages( sfInit(parallel = TRUE, cpus = ncpus, type = "SOCK") )
    
  # Wrapper function ----
  sim <- function(x){
  
    # Define passage scenarios (ASFMC 2024) 
    passages <- c(
      sample(seq(0, 1, .1), 1, replace = FALSE),
      sample(seq(0, 1, .1), 1, replace = FALSE),
      sample(seq(0, 1, .1), 1, replace = FALSE))
    
    passage <- passages
    
    species <- "BBH"
  
  # . Call simulation ----
    res <- sim_pop(
      species = species,
      river = as.character(get_rivers(species)[sample(1:length(get_rivers(species)), 1)]),
      nyears = 50,    
      n_init = MASS::rnegbin(1, 1e6, 1),
      sr = rbeta(1, 100, 100),
      b = 0.05,
      upstream = passage[1],
      downstream = passage[2],
      downstream_j = passage[3],
      output_years = 'last',
      age_structured_output = TRUE,
      sex_specific = TRUE)
  
  # . Define the output lists ----
    retlist <- list(
      res = res)      
    
    return(retlist)    
  }  
    
  # Parallel execution ----
  # . Load libraries on workers -----
  # Suppress messages gets rid of the sfLibrary() message, 
  # capture.output captures the actual library() message that still isn't caught
  capture.output(suppressMessages(sfLibrary(anadrofish)))
  capture.output(suppressMessages(sfLibrary(tidyverse)))
  
  # . Distribute to workers -----
  # Number of iterations per parallel simulation
  niterations <- 1e3
  
  # Run the simulation
  result <- sfLapply(1:niterations, sim) 
  
  # . Stop snowfall ----
  suppressMessages( sfStop() )
  
  # Results ----
  # . Save file ----
  # 'result' is a list of lists. Save this:
  save(result, file = paste0("results/bbh_habitat_variable_", i, ".rda"))
  
  setTxtProgressBar(pb, i)

}

# Progress and timing ----
close(pb)

total_time <- Sys.time()-start
total_time