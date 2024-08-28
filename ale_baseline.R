# Package load ----
  library(snowfall)
  library(anadrofish)
  library(tidyverse)

# Parallel settings ----
# Get number of cores
ncpus <- 10

# Initialize snowfall
sfInit(parallel = TRUE, cpus = ncpus, type = "SOCK")

# Wrapper function ----
sim <- function(x){

  # Define passage scenarios (ASFMC 2024) 
  passages <- list(
    c(0, 1, 1),
    c(1, 1, 1),
    c(0.31, 0.8, 0.90))
  
  scenarios <- c("No passage", "No dams", "Current")
  scenario_num <- sample(1:3, 1, replace = TRUE)
  
  scenario <- scenarios[scenario_num]
  passage <- passages[[scenario_num]]
  
  species <- "ALE"

# . Call simulation ----
  res <- sim_pop(
    species = species,
    river = as.character(get_rivers(species)[sample(1:length(get_rivers(species)), 1)]),
    nyears = 100,    
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
  res$scenario <- scenario

  retlist <- list(
    res = res)      
  
  return(retlist)    
}  
  
# Parallel execution ----
# . Load libraries on workers -----
sfLibrary(anadrofish)
sfLibrary(tidyverse)

# . Distribute to workers -----
# Number of simulations to run
niterations <- 1e3

# Run the simulation ----
start <- Sys.time()

result <- sfLapply(1:niterations, sim) 

total_time <- Sys.time()-start
total_time

# . Stop snowfall ----
sfStop()

# Results ----
# . Save file ----
# 'result' is a list of lists. Save this:
save(result, file = "results/ale_habitat_baseline.rda")
