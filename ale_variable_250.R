# Package load ----
# devtools::install_github("danStich/anadrofish@v2.1.0")
library(snowfall)
library(parallel)
library(anadrofish)
library(tidyverse)

# Progress and benchmarking ----
# Record system time for total_time calculation at end
start <- Sys.time()
start

# Parallel settings ----
# Get number of cores for simulation using parallel package
ncpus <- parallel::detectCores() - 1

# Initialize snowfall socket cluster
sfInit(parallel = TRUE, cpus = ncpus, type = "SOCK")

# Wrapper function ----
sim <- function(x){

  # Define passage scenarios (ASFMC 2024) 
  passages <- c(
    sample(seq(0, 1, .1), 1, replace = FALSE),
    sample(seq(0, 1, .1), 1, replace = FALSE),
    sample(seq(0, 1, .1), 1, replace = FALSE))
  
  passage <- passages
  
  species <- "ALE"

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
sfLibrary(anadrofish)
sfLibrary(tidyverse)

# . Distribute to workers -----
# Number of iterations per parallel simulation
niterations <- 250000

# Run the simulation
result <- sfLapply(1:niterations, sim) 

# Stop the cluster
sfStop()

# Progress and timing ----
total_time <- Sys.time()-start
total_time

save(result, file = "ale_habitat_variable_250000_04.rda")