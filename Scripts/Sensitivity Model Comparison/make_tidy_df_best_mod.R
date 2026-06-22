# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)

source("Scripts/plot_settings.R")


# Load Data              ---------------------------------------------------------------

pmp_food      <- read_csv("Results/Model Comparison/pmp_FOOD.csv")      |> rename(ID = ...1)
pmp_countries <- read_csv("Results/Model Comparison/pmp_COUNTRIES.csv") |> rename(ID = ...1)
pmp_mammals   <- read_csv("Results/Model Comparison/pmp_MAMMALS.csv")   |> rename(ID = ...1)

ID_dict       <- read_csv2("Data/ID_dictionaries.csv") |> rename(ID = IDs)

# Make Tidy DF           ---------------------------------------------------------------

best_mod_f <- apply(pmp_food[,-1],1,which.max) 
best_mod_c <- apply(pmp_countries[,-1],1,which.max)
best_mod_m <- apply(pmp_mammals[,-1],1,which.max)


best_mod_f_pmp <- apply(pmp_food[,-1],1,max) 
best_mod_c_pmp <- apply(pmp_countries[,-1],1,max)
best_mod_m_pmp <- apply(pmp_mammals[,-1],1,max)


df <- data.frame(domain       = c(rep("Food",length(best_mod_f)), 
                                  rep("Countries",length(best_mod_c)),
                                  rep("Mammals",length(best_mod_m))),
                 ID_n         = c(1:length(best_mod_f),1:length(best_mod_c),1:length(best_mod_m)),
                 best_mod_ind = c(best_mod_f,best_mod_c,best_mod_m),
                 best_mod_pmp = c(best_mod_f_pmp,best_mod_c_pmp,best_mod_m_pmp)) |> 
            mutate(best_mod     = case_when(
                         best_mod_ind == 1 ~ "RULEXJ",
                         best_mod_ind == 2 ~ "CAM",
                         best_mod_ind == 3 ~ "GCM",
                         best_mod_ind == 4 ~ "MAPP",
                         best_mod_ind == 5 ~ "QEST",
                         best_mod_ind == 6 ~ "RGUESS")) |> 
            left_join(ID_dict,by = c("domain","ID_n"))

# Save DF
write_csv2(df,"Results/Model Comparison/best_mods.csv")
