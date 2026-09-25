library(tidyverse)
library(correlation)


best_mod <- read_csv2("Results/Model Comparison/best_mods.csv")
est      <- read_csv2("Data/data_tidy_combined.csv") 
r2       <- read.csv2("Results/Posterior Predictions/r2_per_person.csv")

temp <- best_mod |> 
         left_join(est |> select(ID,knowledge,strategy), by = "ID") |> 
         left_join(r2  |> select(ID_n=ID_ind,domain,r2), by = c("ID_n","domain")) |> 
         distinct()

temp |> 
  group_by(domain) |> 
  summarize(m = mean(knowledge),
            sd = sd(knowledge),
            n = n())

temp |> 
  group_by(domain,best_mod) |> 
  summarize(m = mean(knowledge),
            n = n())


temp |> 
  select(ID,domain,knowledge,r2) |> 
  group_by(domain) |> 
  correlation()


est |> 
  filter(phase == "training") |> 
  group_by(ID,block) |> 
  mutate(RMSE = sqrt(mean((true-est)^2))) |> 
  select(ID,domain,RMSE,knowledge,block) |>
  distinct() |> 
  filter(block == 1 | block == 5) |> 
  group_by(domain, block) |> 
  correlation()



est |> 
  filter(phase == "testing" & ID_item != "Basketball") |> 
  group_by(ID) |> 
  mutate(RMSE = sqrt(mean((true-est)^2))) |> 
  select(ID,domain,RMSE,knowledge) |>
  distinct() |> 
  group_by(domain) |> 
  correlation()
