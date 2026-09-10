# Load Packages           -----------------------------------------------------------

library(tidyverse)
library(kableExtra)
library(readr)
library(purrr)


mod_order <- c("RULEXJ", "CAM", "GCM", "MAPP",  "QEst", "RGuess")


# Load Data               -----------------------------------------------------------

# Manuscript PMPs (single run) and the PMPs of the sensitivity runs (one per re-trained
# network). `domain` is the upper case domain name used in the file names,
# i.e. COUNTRIES, FOOD or MAMMALS.

load_domain <- function(domain){

  pmp_man <- read_csv(paste0("Results/Model Comparison/pmp_", domain, ".csv"),
                      show_col_types = FALSE) |>
                rename(ID = ...1) |>
                pivot_longer(cols      = all_of(mod_order),
                             names_to  = "models",
                             values_to = "pmp_man")

  files   <- list.files(file.path("Results/Sensitivity Model Comparison",
                                  str_to_title(domain)),
                        full.names = TRUE)

  files   <- files[startsWith(basename(files), paste0("pmp_", domain, "_")) &
                   endsWith(files, ".csv")]

  pmp_sens <- read_csv(files, id = "source", show_col_types = FALSE) |>
                rename(ID = ...1) |>
                mutate(run = parse_number(basename(source))) |>
                select(-source) |>
                pivot_longer(cols      = all_of(mod_order),
                             names_to  = "models",
                             values_to = "pmp")

  list(man = pmp_man, sens = pmp_sens)

}


# Agreement with the manuscript run  ------------------------------------------------

# For every participant: in how many of the re-trained networks was the same model the
# best model (i.e. the one with the highest PMP) as in the manuscript run?

agreement <- function(domain){

  d <- load_domain(domain)

  # best model per participant according to the manuscript PMPs
  best <- d$man |>
            group_by(ID) |>
            slice_max(pmp_man, n = 1, with_ties = FALSE) |>
            ungroup() |>
            transmute(ID, best_mod = models)

  # best model per participant in each of the re-trained networks
  best_sens <- d$sens |>
                 group_by(run, ID) |>
                 slice_max(pmp, n = 1, with_ties = FALSE) |>
                 ungroup() |>
                 select(run, ID, sens_mod = models) |>
                 left_join(best, by = "ID")

  # most frequently selected model in those runs that disagree with the manuscript
  alt <- best_sens |>
           filter(sens_mod != best_mod) |>
           count(ID, sens_mod) |>
           group_by(ID) |>
           slice_max(n, n = 1, with_ties = FALSE) |>
           ungroup() |>
           transmute(ID, alt_mod = sens_mod, n_alt = n)

  best_sens |>
    group_by(ID, best_mod) |>
    summarise(n_runs  = n(),
              n_agree = sum(sens_mod == best_mod),
              .groups = "drop") |>
    left_join(alt, by = "ID") |>
    mutate(perc   = n_agree / n_runs * 100,
           domain = str_to_title(domain),
           person = paste0("P", ID + 1))

}


# the 50 files of a domain are only read once, no matter how often `agreement()` is used
agreement <- local({
  cache <- list()
  fun   <- agreement
  function(domain){
    if (is.null(cache[[domain]])) cache[[domain]] <<- fun(domain)
    cache[[domain]]
  }
})


# Table: one row per domain  --------------------------------------------------------

# Agreement = percentage of the re-trained networks in which the same model had the
# highest PMP as in the manuscript run. Summarised over the participants of a domain.

tab_domains <- function(domains){

  map_dfr(domains, agreement) |>
    group_by(Domain = domain) |>
    summarise(n_par  = n(),
              n_nets = max(n_runs),
              M      = mean(perc),
              Mdn    = median(perc),
              SD     = sd(perc),
              Min    = min(perc),
              Max    = max(perc),
              n_all  = sum(n_agree == n_runs),
              .groups = "drop") |>
    mutate(Range = paste0(sprintf("%.0f", Min), "--", sprintf("%.0f", Max)),
           across(c(M, Mdn, SD), ~ sprintf("%.1f", .x))) |>
    select(Domain, n_par, M, Mdn, SD, Range, n_all)

}


kbl_domains <- function(domains, n_nets = 50){

  kbl(tab_domains(domains),
      booktabs  = TRUE,
      align     = c("l", rep("r", 4), "c", "r"),
      col.names = c("Domain", "$n$", "$M$", "$Mdn$", "$SD$", "Range", "$n$"),
      caption   = paste0("Agreement between the model comparison reported in the ",
                         "manuscript and the sensitivity analysis.")) |>
    add_header_above(c(" " = 1, "Participants" = 1,
                       "Agreement with manuscript (\\%)" = 4,
                       "Always agree" = 1)) |>
    kable_styling(latex_options = c("hold_position")) |>
    footnote(general = paste0("Agreement = percentage of the ", n_nets, " re-trained ",
                              "networks in which the same model had the highest PMP as ",
                              "in the manuscript run, computed for each participant and ",
                              "then summarised over the participants of a domain. ",
                              "'Always agree' = number of participants for whom this was ",
                              "the case in all ", n_nets, " networks."),
             threeparttable = TRUE, escape = FALSE)

}


# Run                     -----------------------------------------------------------

domains <- c("COUNTRIES", "FOOD", "MAMMALS")

agree_all <- map_dfr(domains, agreement)

write_csv(agree_all, "Results/Sensitivity Model Comparison/agreement_participants.csv")

tab_domains(domains)
kbl_domains(domains)
