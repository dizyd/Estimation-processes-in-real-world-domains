# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)

source("Scripts/plot_settings.R")


# Helper function        ---------------------------------------------------------

# Read all pmp_<DOMAIN>_<run>.csv files of one domain and stack them into one
# data.frame. The run number (0-49) is taken from the file name and stored in `run`.
read_pmp_domain <- function(folder, domain) {
  files <- list.files(file.path("Results/Model Comparison", folder),
                      pattern   = paste0("^pmp_", domain, "_\\d+\\.csv$"),
                      full.names = TRUE)
  
  map_dfr(files, function(f) {
    run <- as.integer(str_extract(basename(f), "\\d+(?=\\.csv$)"))
    read_csv(f, show_col_types = FALSE) |>
      rename(ID = ...1) |>
      mutate(run = run, .before = 1)}) |>
    arrange(run, ID)
}


# Load Data              ---------------------------------------------------------------

pmp_food      <- read_pmp_domain("Food",      "FOOD")
pmp_countries <- read_pmp_domain("Countries", "COUNTRIES")
pmp_mammals   <- read_pmp_domain("Mammals",   "MAMMALS")

ID_dict       <- read_csv2("Data/ID_dictionaries.csv") |> rename(ID = IDs)

# Make Tidy DF (50 runs) -----------------------------------------------------------------

# Classification is based on the PMPs averaged across the 50 network runs.
# `agreement` is the proportion of runs whose individual best model matches
# the best model of the averaged PMPs (stability of the classification).

model_names <- c("RULEXJ","CAM","GCM","MAPP","QEST","RGUESS")

pmp_all <- bind_rows(Food      = pmp_food,
                     Countries = pmp_countries,
                     Mammals   = pmp_mammals,
                     .id = "domain") |> 
           mutate(ID_n = ID + 1) |>                       # python 0-index -> ID_n
           pivot_longer(RULEXJ:RGuess, 
                        names_to  = "model", 
                        values_to = "pmp") |> 
           mutate(model = toupper(model))

# (a) average PMPs across runs
pmp_avg <- pmp_all |> 
             group_by(domain, ID_n, model) |> 
             summarise(pmp_mean = mean(pmp),
                       pmp_sd   = sd(pmp),
                       .groups  = "drop")

# (b) best model per participant based on the averaged PMPs
best_avg <- pmp_avg |> 
              group_by(domain, ID_n) |> 
              slice_max(pmp_mean, n = 1, with_ties = FALSE) |> 
              ungroup() |> 
              transmute(domain, ID_n,
                        best_mod     = model,
                        best_mod_ind = match(model, model_names),
                        best_mod_pmp = pmp_mean,
                        best_mod_sd  = pmp_sd)

# (c) agreement: share of runs whose own best model equals the averaged best model
agreement <- pmp_all |> 
               group_by(domain, ID_n, run) |> 
               slice_max(pmp, n = 1, with_ties = FALSE) |> 
               ungroup() |> 
               select(domain, ID_n, run, best_mod_run = model) |> 
               left_join(best_avg |> select(domain, ID_n, best_mod), 
                         by = c("domain","ID_n")) |> 
               group_by(domain, ID_n) |> 
               summarise(agreement = mean(best_mod_run == best_mod),
                         n_runs    = n(),
                         .groups   = "drop")

df_runs <- best_avg |> 
             left_join(agreement, by = c("domain","ID_n")) |> 
             left_join(ID_dict,   by = c("domain","ID_n")) |> 
             arrange(domain, ID_n)

# Save DF
write_csv2(df_runs, "Results/Model Comparison/best_mods.csv")
