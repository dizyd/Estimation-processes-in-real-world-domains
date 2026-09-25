# Load Packages                    -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)

source("Scripts/plot_settings.R")


# Helper function                  ---------------------------------------------------------

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


# Load & tidy Data                 ---------------------------------------------------------------

pmp_food      <- read_pmp_domain("Food",      "FOOD")
pmp_countries <- read_pmp_domain("Countries", "COUNTRIES")
pmp_mammals   <- read_pmp_domain("Mammals",   "MAMMALS")

# Make into long format
pmp_all_l <- bind_rows(Food      = pmp_food,
                     Countries = pmp_countries,
                     Mammals   = pmp_mammals,
                     .id = "domain") |> 
            mutate(ID_n = ID + 1) |> # python 0-index -> ID_n
            pivot_longer(RULEXJ:RGuess, 
                         names_to  = "model", 
                         values_to = "pmp") |> 
            mutate(model = toupper(model))

# (a) average PMPs across runs
pmp_avg_l <- pmp_all_l |> 
              group_by(domain, ID_n, model) |> 
              summarise(pmp = mean(pmp), .groups  = "drop")


# Add actual IDs to the data.frames
ID_dict   <- read_csv2("Data/ID_dictionaries.csv") |> rename(ID = IDs)
pmp_avg_l <- pmp_avg_l |> left_join(ID_dict, by = c("domain","ID_n")) 


# Load estimation data
est     <- read_csv2("Data/data_tidy_combined.csv")
testing <- est |>
            filter(phase == "testing", ID_item != "Basketball") |> 
            mutate(est = case_when(domain == "Mammals"   & est > 10000 ~ NA,
                                   domain == "Food"      & est > 100   ~ NA,
                                   domain == "Countries" & est > 100   ~ NA,
                                   TRUE                                ~ est))


# Compute RMSE between true and estimated value for each person
test_RMSE <- testing |> 
                filter(training == 0) |> 
                group_by(ID,domain) |> 
                summarize(RMSE = sqrt(mean((est-true)^2,na.rm=T)), .groups  = "drop")
              


# Make Figure 7 (PMPs)             ---------------------------------------------------------------

mod_order <- c("RULEXJ", "CAM", "GCM", "MAPP",  "QEST", "RGUESS")

  
p_f <- pmp_avg_l |>
        filter(domain == "Food") |> 
        left_join(test_RMSE |> filter(domain == "Food"), by = "ID") |> 
        ggplot(aes(x = model, y = reorder(ID,RMSE), fill = pmp)) +
          geom_tile(show.legend = F, color="white") +
          scale_fill_viridis(name = "Posterior\nModel\nProbability", limits = c(0, 1)) +
          scale_y_discrete(labels = function(x) sprintf("", x)) + # P%s
          labs(title = "Food",x = " ", y = "Participants") +
          theme_nice() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = element_blank(),
                plot.title = element_text(face="bold")) + 
          scale_x_discrete(limits = mod_order) 


p_c <-  pmp_avg_l |>
        filter(domain == "Countries") |> 
        left_join(test_RMSE |> filter(domain == "Countries"), by = "ID") |> 
        ggplot(aes(x = model, y = reorder(ID,RMSE), fill = pmp)) + 
          geom_tile(show.legend = F, color="white") +
          scale_fill_viridis(name = "Posterior Model\nProbability", limits = c(0, 1)) +
          scale_y_discrete(labels = function(x) sprintf("", x)) +
          labs(title = "Countries",x = "Model", y = " ") +
          theme_nice() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = element_blank(),
                plot.title = element_text(face="bold")) + 
          scale_x_discrete(limits = mod_order)


p_m <-  pmp_avg_l |>
        filter(domain == "Mammals") |> 
        left_join(test_RMSE |> filter(domain == "Mammals"), by = "ID") |> 
        ggplot(aes(x = model, y = reorder(ID,RMSE), fill = pmp)) + 
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


# Make Table 2                     ---------------------------------------------------------------

best_mod <- read_csv2("Results/Model Comparison/best_mods.csv")


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

# Make Figure 6 (Confusion Matrix) ---------------------------------------------------------------
# Based on single runs Scripts\Model Comparison\Single Runs
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
