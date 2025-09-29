# Load Packages                  -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)

source("Scripts/plot_settings.R")


# Load Data                      ---------------------------------------------------------------

best_mod       <- read_csv2("Results/Model Comparison/best_mods.csv")

pars_FOOD      <- read_csv("Results/Parameter Estimates/par_ests_FOOD_21082025.csv")      |> select(-...1) 
pars_MAMMALS   <- read_csv("Results/Parameter Estimates/par_ests_MAMMALS_21082025.csv")   |> select(-...1) 
pars_COUNTRIES <- read_csv("Results/Parameter Estimates/par_ests_COUNTRIES_21082025.csv") |> select(-...1)

# Bind
pars <- bind_rows(pars_FOOD,pars_MAMMALS,pars_COUNTRIES) |> mutate(ID_n = ID_ind + 1)

# Join
pars <- left_join(pars,best_mod,by=c("ID_n","domain"))

# Prepare DF for  tables         ---------------------------------------------------------------

# with Standard point estimates (mean) and 
# credibility intervals (95%-CI between the 2.5% and 97.5% quantiles of the corresponding posterior)



temp <- pars |>  
          filter(model == best_mod) |> 
          group_by(domain, best_mod, paramter) |> 
          summarize(n       = n(),
                    mean    = mean(mean, na.rm=T),
                    sd      = mean(sd, na.rm=T),
                    hdi_025 = mean(hdi_025, na.rm=T),
                    hdi_975 = mean(hdi_975, na.rm=T)) |> 
          filter(n > 5) |> 
          select(domain, best_mod, parameter=paramter, mean:hdi_975, n) |> 
          ungroup() 

# Make Tables  D1-D3 (Food)      ---------------------------------------------------------------

temp |> 
  filter(domain == "Food") |> 
  select(-domain) |> 
  mutate(parameter = factor(parameter, levels = c("alpha","w_CAM[0]","w_CAM[1]","w_CAM[2]","w_CAM[3]","w_CAM[4]"
                                                  ,"w_CAM[5]","w_CAM[6]","w_CAM[7]","w_CAM[8]","w_CAM[9]","w_CAM[10]",
                                                  "w_CAM[11]","w_CAM[12]","w_CAM[13]","w_CAM[14]","c",
                                                  "w[0]", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "w[6]",
                                                  "w[7]", "w[8]", "w[9]", "w[10]", "w[11]", "w[12]", "w[13]", "w[14]",
                                                  "w_GCM[1]", "w_GCM[2]", "w_GCM[3]", "w_GCM[4]", "w_GCM[5]", "w_GCM[6]",
                                                  "w_GCM[7]", "w_GCM[8]", "w_GCM[9]", "w_GCM[10]",
                                                  "w_GCM[11]","w_GCM[12]","w_GCM[13]","w_GCM[14]", "sigma"))) |> 
  arrange(best_mod, parameter) |> 
  mutate(parameter = case_when(
            parameter == "sigma" ~ "$\\sigma$",
            parameter == "c"     ~ "$c$",
            parameter == "w[0]" ~ "$w_0$",
            parameter == "w[1]" ~ "$w_1$",
            parameter == "w[2]" ~ "$w_2$",
            parameter == "w[3]" ~ "$w_3$",
            parameter == "w[4]" ~ "$w_4$",
            parameter == "w[5]" ~ "$w_5$",
            parameter == "w[6]" ~ "$w_6$",
            parameter == "w[7]" ~ "$w_7$",
            parameter == "w[8]" ~ "$w_8$",
            parameter == "w[9]" ~ "$w_9$",
            parameter == "w[10]" ~ "$w_{10}$",
            parameter == "w[11]" ~ "$w_{11}$",
            parameter == "w[12]" ~ "$w_{12}$",
            parameter == "w[13]" ~ "$w_{13}$",
            parameter == "w[14]" ~ "$w_{14}$",
            parameter == "w_CAM[0]" ~ "$w_{0 \\text{ CAM}}$",
            parameter == "w_CAM[1]" ~ "$w_{1  \\text{ CAM}}$",
            parameter == "w_CAM[2]" ~ "$w_{2  \\text{ CAM}}$",
            parameter == "w_CAM[3]" ~ "$w_{3  \\text{ CAM}}$",
            parameter == "w_CAM[4]" ~ "$w_{4  \\text{ CAM}}$",
            parameter == "w_CAM[5]" ~ "$w_{5  \\text{ CAM}}$",
            parameter == "w_CAM[6]" ~ "$w_{6  \\text{ CAM}}$",
            parameter == "w_CAM[7]" ~ "$w_{7  \\text{ CAM}}$",
            parameter == "w_CAM[8]" ~ "$w_{8  \\text{ CAM}}$",
            parameter == "w_CAM[9]" ~ "$w_{9  \\text{ CAM}}$",
            parameter == "w_CAM[10]" ~ "$w_{10  \\text{ CAM}}$",
            parameter == "w_CAM[11]" ~ "$w_{11  \\text{ CAM}}$",
            parameter == "w_CAM[12]" ~ "$w_{12  \\text{ CAM}}$",
            parameter == "w_CAM[13]" ~ "$w_{13  \\text{ CAM}}$",
            parameter == "w_CAM[14]" ~ "$w_{14  \\text{ CAM}}$",
            parameter == "w_GCM[0]" ~ "$w_{0 \\text{ GCM}}$",
            parameter == "w_GCM[1]" ~ "$w_{1 \\text{ GCM}}$",
            parameter == "w_GCM[2]" ~ "$w_{2 \\text{ GCM}}$",
            parameter == "w_GCM[3]" ~ "$w_{3 \\text{ GCM}}$",
            parameter == "w_GCM[4]" ~ "$w_{4 \\text{ GCM}}$",
            parameter == "w_GCM[5]" ~ "$w_{5 \\text{ GCM}}$",
            parameter == "w_GCM[6]" ~ "$w_{6 \\text{ GCM}}$",
            parameter == "w_GCM[7]" ~ "$w_{7 \\text{ GCM}}$",
            parameter == "w_GCM[8]" ~ "$w_{8 \\text{ GCM}}$",
            parameter == "w_GCM[9]" ~ "$w_{9 \\text{ GCM}}$",
            parameter == "w_GCM[10]" ~ "$w_{10 \\text{ GCM}}$", 
            parameter == "w_GCM[11]" ~ "$w_{11 \\text{ GCM}}$", 
            parameter == "w_GCM[12]" ~ "$w_{12 \\text{ GCM}}$", 
            parameter == "w_GCM[13]" ~ "$w_{13 \\text{ GCM}}$", 
            parameter == "w_GCM[14]" ~ "$w_{14 \\text{ GCM}}$", 
            TRUE ~ parameter)) -> t_pars_food
  
table(t_pars_food$best_mod)

t_pars_food |> 
  filter(best_mod == "CAM") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{CAM} model for $N$ = participants",escape = FALSE) 

t_pars_food |> 
  filter(best_mod == "GCM") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{GCM} model for $N$ = participants.",escape = FALSE)
  
t_pars_food |> 
  filter(best_mod == "RULEXJ") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{RULEX-J} model for $N$ = participants.",escape = FALSE) 



# Make Tables  D4-D5 (Countries) ---------------------------------------------------------------


temp |> 
  filter(domain == "Countries") |> 
  select(-domain) |> 
  mutate(parameter = factor(parameter, levels = c("alpha","w_CAM[0]","w_CAM[1]","w_CAM[2]","w_CAM[3]","w_CAM[4]"
                                                  ,"w_CAM[5]","w_CAM[6]","w_CAM[7]","w_CAM[8]","w_CAM[9]","w_CAM[10]","c",
                                                  "w[0]", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "w[6]",
                                                  "w[7]", "w[8]", "w[9]", "w[10]",
                                                  "w_GCM[1]", "w_GCM[2]", "w_GCM[3]", "w_GCM[4]", "w_GCM[5]", "w_GCM[6]",
                                                  "w_GCM[7]", "w_GCM[8]", "w_GCM[9]", "w_GCM[10]", "sigma"))) |> 
  arrange(best_mod, parameter) |> 
  mutate(parameter = case_when(
    parameter == "alpha" ~ "$\\alpha$",
    parameter == "sigma" ~ "$\\sigma$",
    parameter == "c"     ~ "$c$",
    parameter == "w[0]" ~ "$w_0$",
    parameter == "w[1]" ~ "$w_1$",
    parameter == "w[2]" ~ "$w_2$",
    parameter == "w[3]" ~ "$w_3$",
    parameter == "w[4]" ~ "$w_4$",
    parameter == "w[5]" ~ "$w_5$",
    parameter == "w[6]" ~ "$w_6$",
    parameter == "w[7]" ~ "$w_7$",
    parameter == "w[8]" ~ "$w_8$",
    parameter == "w[9]" ~ "$w_9$",
    parameter == "w[10]" ~ "$w_{10}$",
    parameter == "w_CAM[0]" ~ "$w_{0 \\text{ CAM}}$",
    parameter == "w_CAM[1]" ~ "$w_{1  \\text{ CAM}}$",
    parameter == "w_CAM[2]" ~ "$w_{2  \\text{ CAM}}$",
    parameter == "w_CAM[3]" ~ "$w_{3  \\text{ CAM}}$",
    parameter == "w_CAM[4]" ~ "$w_{4  \\text{ CAM}}$",
    parameter == "w_CAM[5]" ~ "$w_{5  \\text{ CAM}}$",
    parameter == "w_CAM[6]" ~ "$w_{6  \\text{ CAM}}$",
    parameter == "w_CAM[7]" ~ "$w_{7  \\text{ CAM}}$",
    parameter == "w_CAM[8]" ~ "$w_{8  \\text{ CAM}}$",
    parameter == "w_CAM[9]" ~ "$w_{9  \\text{ CAM}}$",
    parameter == "w_CAM[10]" ~ "$w_{10  \\text{ CAM}}$",
    parameter == "w_GCM[0]" ~ "$w_{0 \\text{ GCM}}$",
    parameter == "w_GCM[1]" ~ "$w_{1 \\text{ GCM}}$",
    parameter == "w_GCM[2]" ~ "$w_{2 \\text{ GCM}}$",
    parameter == "w_GCM[3]" ~ "$w_{3 \\text{ GCM}}$",
    parameter == "w_GCM[4]" ~ "$w_{4 \\text{ GCM}}$",
    parameter == "w_GCM[5]" ~ "$w_{5 \\text{ GCM}}$",
    parameter == "w_GCM[6]" ~ "$w_{6 \\text{ GCM}}$",
    parameter == "w_GCM[7]" ~ "$w_{7 \\text{ GCM}}$",
    parameter == "w_GCM[8]" ~ "$w_{8 \\text{ GCM}}$",
    parameter == "w_GCM[9]" ~ "$w_{9 \\text{ GCM}}$",
    parameter == "w_GCM[10]" ~ "$w_{10 \\text{ GCM}}$",
    TRUE ~ parameter
  )) -> t_pars_countries



table(t_pars_countries$best_mod)

t_pars_countries |> 
  filter(best_mod == "GCM") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{GCM} model for $N$ = participants.",escape = FALSE)

t_pars_countries |> 
  filter(best_mod == "RULEXJ") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{RULEX-J} model for $N$ = participants.",escape = FALSE) 



# Make Tables  D6-D7 (Mammals)   ---------------------------------------------------------------


temp |> 
  filter(domain == "Mammals") |> 
  select(-domain) |> 
  mutate(parameter = factor(parameter, levels = c("c","w[0]", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "w[6]",
                                                  "w[7]", "w[8]", "w[9]", "w[10]","n_cats", "sigma"))) |> 
  arrange(best_mod, parameter) |> 
  mutate(parameter = case_when(
    parameter == "sigma" ~ "$\\sigma$",
    parameter == "c"     ~ "$c$",
    parameter == "n_cats"     ~ "$\\text{k}$",
    parameter == "w[0]" ~ "$w_0$",
    parameter == "w[1]" ~ "$w_1$",
    parameter == "w[2]" ~ "$w_2$",
    parameter == "w[3]" ~ "$w_3$",
    parameter == "w[4]" ~ "$w_4$",
    parameter == "w[5]" ~ "$w_5$",
    parameter == "w[6]" ~ "$w_6$",
    parameter == "w[7]" ~ "$w_7$",
    parameter == "w[8]" ~ "$w_8$",
    parameter == "w[9]" ~ "$w_9$",
    parameter == "w[10]" ~ "$w_{10}$",
    TRUE ~ parameter
  )) -> t_pars_mammals





table(t_pars_mammals$best_mod)

t_pars_mammals |> 
  filter(best_mod == "GCM") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{GCM} model for $N$ = participants.",escape = FALSE)

t_pars_mammals |> 
  filter(best_mod == "MAPP") |> 
  select(-best_mod, -n) |> 
  kable(digits  = 2, booktabs=TRUE, align="c", format = "latex",
        col.names = c("Parameter","$M$","$SD$", "HDI-2.5/%","HDI-97.5/%"),
        caption = "Average posterior parameter values of the \\textbf{MAPPING} model for $N$ = participants.",escape = FALSE) 




