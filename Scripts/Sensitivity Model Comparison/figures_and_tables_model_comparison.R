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

# Make into long format

pmp_food_l <- pmp_food |> 
                rename(ID_ind = ID) |> 
                mutate(ID_ind = ID_ind + 1) |> 
                pivot_longer(cols = RULEXJ:RGuess, values_to = "pmp", names_to = "models") 

pmp_countries_l <- pmp_countries |> 
                      rename(ID_ind = ID) |> 
                      mutate(ID_ind = ID_ind + 1) |>  
                      pivot_longer(cols = RULEXJ:RGuess, values_to = "pmp", names_to = "models")

pmp_mammals_l <- pmp_mammals |> 
                    rename(ID_ind = ID) |> 
                    mutate(ID_ind = ID_ind + 1) |> 
                    pivot_longer(cols = RULEXJ:RGuess, values_to = "pmp", names_to = "models")


# Add actual IDs to the data.frames
IDs_food      <- read_csv("Data/data_analysis_food.csv")      |> select(-ID_item,-training,-crit,-item,-img) |> names()
IDs_countries <- read_csv("Data/data_analysis_countries.csv") |> select(-ID_item,-training,-crit,-item,-img) |> names()
IDs_mammals   <- read_csv("Data/data_analysis_mammals.csv")   |> select(-ID_item,-training,-crit,-item,-img) |> names()


pmp_food_l      <- pmp_food_l      |> add_column(ID = rep(IDs_food,each = 6))
pmp_countries_l <- pmp_countries_l |> add_column(ID = rep(IDs_countries,each = 6))
pmp_mammals_l   <- pmp_mammals_l   |> add_column(ID = rep(IDs_mammals,each = 6))


# Load estimation data
est     <- read_csv2("Data/data_tidy_combined.csv")
testing <- est |>
            filter(phase == "testing", ID_item != "Basketball") |> 
            mutate(est = case_when(domain == "Mammals"   & est > 10000 ~ NA,
                                   domain == "Food"      & est > 100   ~ NA,
                                   domain == "Countries" & est > 100   ~ NA,
                                   TRUE                                ~ est))


# Compute MAE between true and estimated value for each person
test_RMSE <- testing |> 
                filter(training == 0) |> 
                group_by(ID,domain) |> 
                summarize(RMSE = sqrt(mean((est-true)^2,na.rm=T)))
              


# Make Figure 4 (PMPs)   ---------------------------------------------------------------


mod_order <- c("RULEXJ", "CAM", "GCM", "MAPP",  "QEst", "RGuess")


  
p_f <- pmp_food_l |> 
        left_join(test_RMSE |> filter(domain == "Food"), by = "ID") |> 
        ggplot(aes(x = models, y = reorder(ID,RMSE), fill = pmp)) +
          geom_tile(show.legend = F, color="white") +
          scale_fill_viridis(name = "Posterior\nModel\nProbability", limits = c(0, 1)) +
          scale_y_discrete(labels = function(x) sprintf("", x)) + # P%s
          labs(title = "Food",x = " ", y = "Participants") +
          theme_nice() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = element_blank(),
                plot.title = element_text(face="bold")) + 
          scale_x_discrete(limits = mod_order) 


p_c <- pmp_countries_l |> 
        left_join(test_RMSE |> filter(domain == "Countries"), by = "ID") |> 
        ggplot(aes(x = models, y = reorder(ID,RMSE), fill = pmp)) + 
          geom_tile(show.legend = F, color="white") +
          scale_fill_viridis(name = "Posterior Model\nProbability", limits = c(0, 1)) +
          scale_y_discrete(labels = function(x) sprintf("", x)) +
          labs(title = "Countries",x = "Model", y = " ") +
          theme_nice() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = element_blank(),
                plot.title = element_text(face="bold")) + 
          scale_x_discrete(limits = mod_order)


p_m <- pmp_mammals_l |> 
        left_join(test_RMSE |> filter(domain == "Mammals"), by = "ID") |> 
        ggplot(aes(x = models, y = reorder(ID,RMSE), fill = pmp)) + 
          geom_tile(show.legend = T, color="white") +
          scale_fill_viridis(name = "Posterior\nModel\nProbability\n", limits = c(0, 1)) +
          scale_y_discrete(labels = function(x) sprintf("", x)) +
          labs(title = "Mammals",x = " ", y = " ") +
          theme_nice() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = element_blank(),
                plot.title = element_text(face="bold")) + 
          scale_x_discrete(limits = mod_order)


# Make arrow

arrw <- ggplot(iris) +
          geom_segment(
            x = 1, y = 10,
            xend = 1, yend = 1,
            lineend = "round", # See available arrow types in example above
            linejoin = "round",
            size = 2, 
            arrow = arrow(length = unit(0.3, "inches")),
          ) + 
          annotate("text", x = 1, y = 0,  label = "lowest", size = 6) +
          annotate("text", x = 1, y = 11, label = "highest", size = 6) +
          annotate("text", x = 0.7, y = 5.5, label = "RMSE between true and \n estimated criterion values", size = 6, angle = 90) +
          scale_x_continuous(limits = c(.5, 1.5)) +
          scale_y_continuous(limits = c(0, 11)) +
          theme_void()


arrw + p_f + p_c + p_m  +   plot_layout(ncol = 4)

ggsave("Figures/pmp.pdf",width=30,height=25,units = "cm",device = cairo_pdf)


# Make Tables            ---------------------------------------------------------------



best_mod_f <- apply(pmp_food[,-1],1,which.max) 
best_mod_c <- apply(pmp_countries[,-1],1,which.max)
best_mod_m <- apply(pmp_mammals[,-1],1,which.max)


best_mod <- data.frame(domain = c(rep("Food",length(best_mod_f)),
                                  rep("Countries",length(best_mod_c)),
                                  rep("Mammals",length(best_mod_m))),
                       ID     = c(1:length(best_mod_f),1:length(best_mod_c),1:length(best_mod_m)),
                       best_mod_ind = c(best_mod_f,best_mod_c,best_mod_m))


best_mod |> 
  group_by(domain) |> 
  summarize(RULEXJ = sum(best_mod_ind == 1),
            CAM    = sum(best_mod_ind == 2),
            GCM    = sum(best_mod_ind == 3),
            MAPP   = sum(best_mod_ind == 4),
            QEST   = sum(best_mod_ind == 5),
            RGUESS = sum(best_mod_ind == 6)) |> 
  kable(format    = "latex", digits=2, booktabs=TRUE, align="c",
        col.names = c("Domain",mod_order),
        label     = "best_models",
        caption   = "Counts of best fitting model in each domain",
        escape    = FALSE)

# Make Confusion Matrix  ---------------------------------------------------------------

# Copy values from .ipynbs for now, do it better later

mods    <- c("RulEx-J","CAM","GCM","MAPP","QEst","RGuess")
df_mods <- expand_grid("true"=mods,"est"=mods)


cf_food <- df_mods |>
                add_column(p = c(.59,.26,.15,.00,.00,.00,
                                 .06,.92,.00,.00,.00,.01,
                                 .03,.00,.96,.00,.00,.01,
                                 .00,.00,.00,1.00,.00,.00,
                                 .00,.01,.00,.00,.99,.00,
                                 .00,.01,.00,.00,.00,.99))



cf_countries <- df_mods |>
                    add_column(p = c(.53,.27,.13,.00,.00,.06,
                                       .08,.89,.01,.01,.00,.01,
                                       .03,.00,.93,.01,.00,.03,
                                       .00,.00,.01,.98,.00,.01,
                                       .00,.00,.00,.00,1.00,.00,
                                       .02,.01,.02,.00,.00,.95))

cf_mammals <- df_mods |>
                    add_column(p = c(.66,.19,.15,.00,.00,.00,
                                     .05,.94,.00,.00,.00,.00,
                                     .06,.00,.94,.00,.00,.00,
                                     .00,.00,.00,1.00,.00,.00,
                                     .00,.00,.00,.00,1.00,.00,
                                     .00,.00,.00,.00,.00,1.00))

pcf_f <- ggplot(cf_food, aes(x = true, y = est, fill = p)) +
            geom_tile(show.legend = F) +
            scale_fill_viridis(name = "", limits = c(0, 1)) +
            geom_text(aes(label = papaja::printnum(p)),color = ifelse(cf_countries$p > .3, "black","white"),size=4.5) +
            scale_y_discrete(limits = rev(mods)) +
            labs(title = "Food",x = "", y = "Predicted Model") +
            theme_nice() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid = element_blank(),
                  plot.title = element_text(face="bold")) + 
            scale_x_discrete(limits = mods)


pcf_c <- ggplot(cf_countries, aes(x = true, y = est, fill = p)) +
            geom_tile(show.legend = F) +
            scale_fill_viridis(name = "Proportion of Classified Model", limits = c(0, 1)) +
            geom_text(aes(label = papaja::printnum(p)),color = ifelse(cf_countries$p > .3, "black","white"),size=4.5) +
            scale_y_discrete(limits = rev(mods)) +
            labs(title = "Countries",x = "True Model", y = "") +
            theme_nice() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid = element_blank(),
                  plot.title = element_text(face="bold")) + 
            scale_x_discrete(limits = mods)

pcf_m <- ggplot(cf_mammals, aes(x = true, y = est, fill = p)) +
            geom_tile() +
            scale_fill_viridis(name = "Proportion of \npredicted model \ngiven true model\n", limits = c(0, 1)) +
            geom_text(aes(label = papaja::printnum(p)),color = ifelse(cf_countries$p > .3, "black","white"),size=4.5) +
            scale_y_discrete(limits = rev(mods)) +
            labs(title = "Mammals",x = "", y = "") +
            theme_nice() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid = element_blank(),
                  legend.position = "right",
                  plot.title = element_text(face="bold")) + 
            scale_x_discrete(limits = mods)



pcf_f + pcf_c + pcf_m 

ggsave("Figures/cm.pdf",width=47,height=15,units = "cm",device = cairo_pdf)
