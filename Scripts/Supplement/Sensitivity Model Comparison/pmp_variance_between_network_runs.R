# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)
library(ggdist)

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

# Load & tidy Data       ---------------------------------------------------------------

pmp_food      <- read_pmp_domain("Food",      "FOOD")
pmp_countries <- read_pmp_domain("Countries", "COUNTRIES")
pmp_mammals   <- read_pmp_domain("Mammals",   "MAMMALS")

mod_order <- c("RULEXJ", "CAM", "GCM", "MAPP",  "QEST", "RGUESS")

# Make into long format: one row per domain x participant x run x model
pmp_all_l <- bind_rows(Food      = pmp_food,
                       Countries = pmp_countries,
                       Mammals   = pmp_mammals,
                       .id = "domain") |> 
             mutate(ID_n = ID + 1) |> # python 0-index -> ID_n
             pivot_longer(RULEXJ:RGuess, 
                          names_to  = "model", 
                          values_to = "pmp") |> 
             mutate(model = factor(toupper(model), levels = mod_order),
                    ID_n  = factor(ID_n, labels = paste0("P", sort(unique(ID_n)))))


# Plot function          ---------------------------------------------------------------

# For each participant (facet): distribution of the PMPs over the 50 network runs
# per model as a violin, plus a large black dot for the mean PMP. With
# points = TRUE the 50 individual runs are added as jittered points.
# Note: the default bandwidth of geom_violin is far too wide for PMPs that are
# often squeezed against 0 or 1, hence the fixed `bw`. Smaller values of `bw`
# make the violins follow the 50 values more closely (but look bumpy), larger
# values make them smoother.
plot_pmp_variance <- function(data, domain, ncol = 8, points = FALSE, bw = .05){
  
  p <- data |> 
        filter(domain == {{domain}}) |> 
        ggplot(aes(x = model, y = pmp, fill = model)) +
          geom_violin(color = NA, alpha = if (points) .6 else .85, 
                      width = .9, bw = bw, scale = "width", show.legend = FALSE)
  
  if (points) {
    p <- p + geom_point(position = position_jitter(width = .1, height = 0),
                        size = .35, color = "grey15", alpha = .6, show.legend = FALSE)
  }
  
  p +
    stat_summary(fun = mean, geom = "point", 
                 color = "black", size = 2.5, show.legend = FALSE) +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .5, 1)) +
    facet_wrap(~ ID_n, ncol = ncol) +
    labs(title = domain, x = "Model", y = "Posterior Model Probability") +
    theme_nice() +
    theme(axis.text.x   = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y   = element_text(size = 10),
          strip.text    = element_text(size = 12),
          panel.grid    = element_blank(),
          panel.spacing = unit(2, "mm"),
          plot.title    = element_text(face = "bold"))
}


# Make Figures           ---------------------------------------------------------------

p_f <- plot_pmp_variance(pmp_all_l, "Food")
p_c <- plot_pmp_variance(pmp_all_l, "Countries")
p_m <- plot_pmp_variance(pmp_all_l, "Mammals")

ggsave("Figures/pmp_variance_food.pdf",      p_f, width = 40, height = 30, units = "cm", device = cairo_pdf)
ggsave("Figures/pmp_variance_countries.pdf", p_c, width = 40, height = 30, units = "cm", device = cairo_pdf)
ggsave("Figures/pmp_variance_mammals.pdf",   p_m, width = 40, height = 30, units = "cm", device = cairo_pdf)


# Agreement Table        ---------------------------------------------------------------

# For each participant: how often (out of the 50 networks) does the winning model
# of a single network coincide with the winning model based on the averaged PMPs?

# winning model per participant based on the averaged PMPs
best_avg <- pmp_all_l |> 
              group_by(domain, ID_n, model) |> 
              summarise(pmp_mean = mean(pmp), .groups = "drop") |> 
              group_by(domain, ID_n) |> 
              slice_max(pmp_mean, n = 1, with_ties = FALSE) |> 
              ungroup() |> 
              select(domain, ID_n, best_mod = model)

# winning model of each single network run
best_run <- pmp_all_l |> 
              group_by(domain, ID_n, run) |> 
              slice_max(pmp, n = 1, with_ties = FALSE) |> 
              ungroup() |> 
              select(domain, ID_n, run, best_mod_run = model)

agreement <- best_run |> 
              left_join(best_avg, by = c("domain","ID_n")) |> 
              group_by(domain, ID_n) |> 
              summarise(agreement = mean(best_mod_run == best_mod), .groups = "drop")

# Summary table: agreement per domain
tab_agreement <- agreement |> 
                  group_by(domain) |> 
                  summarise(n        = n(),
                            M        = mean(agreement)   * 100,
                            Mdn      = median(agreement) * 100,
                            Min      = min(agreement)    * 100,
                            perfect  = mean(agreement == 1) * 100,
                            above_90 = mean(agreement >= .9) * 100,
                            below_80 = mean(agreement <  .8) * 100) |> 
                  arrange(match(domain, c("Food","Countries","Mammals")))

tab_agreement

tab_agreement |> 
  kable(format    = "latex", digits = 1, booktabs = TRUE, align = "c",
        col.names = c("Domain", "$N$", "$M$", "$Mdn$", "Min",
                      "100\\%", "$\\geq$ 90\\%", "< 80\\%"),
        label     = "agreement",
        caption   = "Agreement of the individual networks with the classification based on the averaged posterior model probabilities",
        escape    = FALSE)
