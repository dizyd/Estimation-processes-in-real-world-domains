# Load necessary libraries
library(tidyverse)
library(MCMCvis)
library(runjags)

# Load the data
df           <- read_csv2("Materials/design_data_mammals.csv") |> rename(ID_item = ID) |> select(-item, -crit,-img)
test_IDs     <- df |> filter(training == 0) |> pull(ID_item)
training_IDs <- df |> filter(training == 1) |> pull(ID_item)

est          <- read_csv("Data/data_analysis_mammals.csv") |>
                  pivot_longer(cols = chp46vl70814:u13d9hi208ge,
                               names_to = "ID", values_to = "est") |> 
                  left_join(df |> select(-training),by = "ID_item")

n <- est$ID |> n_distinct()

exemplars <- est |> filter(ID_item %in% training_IDs) |> select(contains("dim"),"crit") |> distinct() |> as.matrix()


data      <- est |>
                filter(ID_item %in% test_IDs) |>
                select(ID, ID_item, est, contains("dim")) |> 
                mutate(ID = rep(1:n,times = length(test_IDs))) |> 
                arrange(ID,ID_item)
                  

# Helper Functions --------------------------------------------------------

data_4_jags       <- function(data,exemplars,
                              cue_labels       = c("c1","c2","c3","c4"),
                              ID_label         = "ID",
                              response_label   = "judgment",
                              group_label      = "group"){
  
  # === === === Transform into JAGS data  === === ===
  
  
  # Extract criterion values of all exemplars
  ncol_ex     <- ncol(exemplars)
  criterion   <- exemplars[,ncol_ex]
  
  # Nr. of Exemplars
  n_exemplars  <-  nrow(exemplars)
  
  # Nr. of Cues
  n_cues   = ncol_ex-1 # number of cues
  
  # Nr. of Persons
  n_persons  <-  length(unique(data[,ID_label] %>% unlist()))
  
  ## Count number of trials
  n_trials <-  nrow(data)
  
  # Create matrix with the cues(objects) each person has seen on corresponding trials
  Cues   <- data    %>% # Extract  the simulated data
    .[,c(ID_label,cue_labels)] %>% # extract columns
    .[,-1] %>% # Delete the ID Column within each list entry
    as.matrix() # Convert to Matrix
  
  # Compute the similarity Matrix (S) with rows as objects(trials) and column as  exemplars
  # Continuous Cues:
  d <- array(0,dim = c(n_trials, n_exemplars, n_cues)) # Create empty error
  
  for (i in 1:n_trials){ # For each trial
    for (j in 1:n_exemplars){ # For each exemplar
      d[i,j,] <- (Cues[i,]-exemplars[j,-ncol_ex])*0.1 # Calculate the similarity of the object and the exemplar
    }
  }
  
  
  
  ## Extract the response (i.e criterion judgments)
  Y   <- data[,response_label] %>% unlist() %>% as.vector()
  
  ## Convert ID variable to continuous ID Variable from 1 to x
  IDs <- as.numeric(as.factor(data[,ID_label] %>% unlist()))
  
  # === === === Save data  === === ===
  
  data_list <- list( "criterion"   = criterion,
                     "exemplars"   = exemplars,
                     "n_exemplars" = n_exemplars,
                     "n_persons"   = n_persons,
                     "n_cues"      = n_cues,
                     "n_trials"    = n_trials,
                     "Cues"        = Cues,
                     "d"           = d,
                     "Y"           = Y,
                     "IDs"         = IDs
  )
  return(data_list)
}



# Prepare JAGS data -------------------------------------------------------

jags_data <- data_4_jags(data,
                         exemplars,
                         cue_labels       = paste0("dim_",1:10),
                         ID_label         = "ID",
                         response_label   = "est")


# RUN MCMC ----------------------------------------------------------------


results <- run.jags(model     = "Scripts/Parameter Estimation/Testing/bayes_GCM_wWeights",
                    monitor   = c("c","w","sigma"),
                    data      = jags_data,
                    n.chains  = 4,
                    adapt     = 3e3,
                    burnin    = 3e3,
                    sample    = 5e3,
                    thin      = 10,
                    method    = "parallel",
                    summarise = F)


save(results,file=paste0("Scripts/Parameter Estimation/Testing/results_mcmc_GCM_mammals_with_weights.Rdata"))

# ==========  Diagnostics   Checks        ============


load("Scripts/Parameter Estimation/Testing/results_mcmc_GCM_mammals_with_weights.Rdata")

MCMCsummary(results$mcmc, params   = "c")


MCMCtrace(results$mcmc,
          params   = "c",
          Rhat     = TRUE,
          n.eff    = TRUE,
          ind      = FALSE)

