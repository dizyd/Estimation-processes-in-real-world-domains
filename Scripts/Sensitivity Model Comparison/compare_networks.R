# Load Packages           -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)
library(readr)
library(purrr)

source("Scripts/plot_settings.R")


# Load Manuscript Data    ---------------------------------------------------------------

pmp_food_man      <- read_csv("Results/Model Comparison/pmp_FOOD.csv")      |> rename(ID = ...1) |> add_column(type = "Manuscript")
pmp_countries_man <- read_csv("Results/Model Comparison/pmp_COUNTRIES.csv") |> rename(ID = ...1) |> add_column(type = "Manuscript")
pmp_mammals_man   <- read_csv("Results/Model Comparison/pmp_MAMMALS.csv")   |> rename(ID = ...1) |> add_column(type = "Manuscript")



# Load and combine .csv's -------------------------------------------------

files_m <- list.files("Results/Sensitivity Model Comparison",
                      pattern    = "^pmp_MAMMALS_.*\\.csv$",
                      full.names = TRUE)

df_m <- read_csv(files_m, id = "source") |> 
          rename(ID = ...1) |> 
          mutate(type = paste0("Sensitivity #",parse_number(source)+1)) |> 
          select(-source) |> 
          bind_rows(pmp_mammals_ma) |> 
          pivot_longer(cols = RULEXJ:RGuess, values_to = "pmp", names_to = "models")


# Make Figure  (PMPs)   ---------------------------------------------------------------

mod_order <- c("RULEXJ", "CAM", "GCM", "MAPP",  "QEst", "RGuess")

# p_f <- pmp_food_l |> 
#         left_join(test_RMSE |> filter(domain == "Food"), by = "ID") |> 
#         ggplot(aes(x = models, y = reorder(ID,RMSE), fill = pmp)) +
#           geom_tile(show.legend = F, color="white") +
#           scale_fill_viridis(name = "Posterior\nModel\nProbability", limits = c(0, 1)) +
#           scale_y_discrete(labels = function(x) sprintf("", x)) + # P%s
#           labs(title = "Food",x = " ", y = "Participants") +
#           theme_nice() +
#           theme(axis.text.x = element_text(angle = 45, hjust = 1),
#                 panel.grid = element_blank(),
#                 plot.title = element_text(face="bold")) + 
#           scale_x_discrete(limits = mod_order) 
# 
# 
# p_c <- pmp_countries_l |> 
#         left_join(test_RMSE |> filter(domain == "Countries"), by = "ID") |> 
#         ggplot(aes(x = models, y = reorder(ID,RMSE), fill = pmp)) + 
#           geom_tile(show.legend = F, color="white") +
#           scale_fill_viridis(name = "Posterior Model\nProbability", limits = c(0, 1)) +
#           scale_y_discrete(labels = function(x) sprintf("", x)) +
#           labs(title = "Countries",x = "Model", y = " ") +
#           theme_nice() +
#           theme(axis.text.x = element_text(angle = 45, hjust = 1),
#                 panel.grid = element_blank(),
#                 plot.title = element_text(face="bold")) + 
#           scale_x_discrete(limits = mod_order)



p_m_man <- df_m |> 
            filter(type == "Manuscript") |> 
            ggplot(aes(x = models, y = ID, fill = pmp)) + 
              geom_tile(show.legend = T, color="white") +
              scale_fill_viridis(name = "Posterior\nModel\nProbability\n", limits = c(0, 1)) +
              scale_y_discrete(labels = function(x) sprintf("", x)) +
              labs(title = "",x = " ", y = " ") +
              theme_nice() +
              theme(legend.position = "right",
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.grid = element_blank(),
                    plot.title = element_text(face="bold")) + 
              scale_x_discrete(limits = mod_order) +
              facet_wrap(.~type)

p_m_sens <- df_m |> 
            filter(type != "Manuscript") |>
            ggplot(aes(x = models, y = ID, fill = pmp)) + 
              geom_tile(show.legend = T, color="white") +
              scale_fill_viridis(name = "Posterior\nModel\nProbability\n", limits = c(0, 1)) +
              scale_y_discrete(labels = function(x) sprintf("", x)) +
              labs(title = "",x = " ", y = " ") +
              theme_nice() +
              theme(legend.position = "right",
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.grid = element_blank(),
                    plot.title = element_text(face="bold")) + 
              scale_x_discrete(limits = mod_order) +
              facet_wrap(.~type, ncol = 3)


p_m_man + p_m_sens +
  plot_layout(guides='collect') +
  plot_annotation(title = 'Mammals') &
  theme(plot.title = element_text(face="bold",hjust = 0.5, size = 20))

# Make arrow
ggsave("Figures/pmp_mammals_sens.pdf",width=35,height=29,units = "cm",device = cairo_pdf)

