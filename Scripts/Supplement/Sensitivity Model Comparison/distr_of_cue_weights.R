# What this script does: 
# Regress each participant’s 68 judgements on the MDS coordinates of the same
# items, and compare the resulting slope coefficients with the prior the
# networks were trained on.

# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)
library(ggdist)

source("Scripts/plot_settings.R")


# Helper functions       --------------------------------------------------------

# One OLS per participant per domain: est ~ V1 + ... + Vk (MDS dimensions).
# Returns one row per participant: ID, domain, intercept, V1, ..., Vk.

slopes_per_participant <- function(domain, data = est) {

  mds <- read_csv2(paste0("Data/Multidimensional Scaling/MDS_config_",
                          str_to_lower(domain), ".csv"),
                   show_col_types = FALSE) |>
    mutate(item_nr = row_number())

  data |>
    filter(domain ==  {{domain}}) |>
    mutate(item_nr = parse_number(img)) |>
    select(ID, domain, item_nr, est) |>
    inner_join(mds, by = "item_nr") |>
    nest(.by = c(ID, domain)) |>
    mutate(coefs = map(data, function(d) coef(lm(est ~ ., data = select(d, -item_nr))))) |>
    unnest_wider(coefs) |>
    rename(intercept = `(Intercept)`) |>
    select(-data)
}


# Load & tidy Data       ---------------------------------------------------------------

est <- read_csv2("Data/data_tidy_combined.csv") |> 
        filter(phase == "testing", training == 0, ID_item != "Basketball")

# Plot function          ---------------------------------------------------------------

# Histogram of all MDS weights of one domain (V1, ..., Vk pooled over dimensions
# and participants, intercept excluded) on the density scale, with the
# N(0, prior_sd) prior the networks were trained on as a line on top.
# `coefs` is the output of slopes_per_participant(). `label_side` puts the
# prior label right or left of the curve (e.g. when the data end close to it).
plot_cue_weights <- function(coefs, domain, prior_sd, bins = 30,
                             label_side = c("right", "left")) {

  side <- if (match.arg(label_side) == "right") 1 else -1

  weights <- coefs |>
    filter(domain == {{domain}}) |>
    pivot_longer(starts_with("V"), names_to = "dimension", values_to = "weight",
                 values_drop_na = TRUE)

  ggplot(weights, aes(x = weight)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = clrs[1], color = "white", alpha = .85) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = prior_sd), n = 500,
                  color = "black", linewidth = 1) +
    # Math label (plotmath) beside the curve at +- 1 SD
    annotate("text", x = side * prior_sd, y = dnorm(prior_sd, 0, prior_sd),
             hjust = if (side > 0) -.2 else 1.2,
             label = paste0("italic(N)*'(0, ", prior_sd, ")'"), parse = TRUE,
             family = "Jost", size = 4) +
    labs(title = domain, x = "Regression weights of MDS dimension", y = "Density") +
    theme_nice() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold")) 
}



# Make & save plot        --------------------------------------------------------------------

coefs <- map_dfr(c("Food", "Mammals", "Countries"), slopes_per_participant)

p_food      <- plot_cue_weights(coefs, "Food",      prior_sd = 25, label_side = "left")
p_mammals   <- plot_cue_weights(coefs, "Mammals",   prior_sd = 750) +
  scale_x_continuous(breaks = c(-2000, 0, 2000))
p_countries <- plot_cue_weights(coefs, "Countries", prior_sd = 15)

p_food + p_countries + p_mammals + plot_layout(axis_titles = "collect")


ggsave("Figures/Supplement/distr_cue_weights.pdf", width = 35, height = 11, units = "cm", device = cairo_pdf)
