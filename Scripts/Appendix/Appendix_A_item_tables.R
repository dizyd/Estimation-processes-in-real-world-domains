# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(kableExtra)

# Load Item Data         ---------------------------------------------------------------

items_countries <- read_csv2("Materials/design_data_countries.csv") |>
                      mutate(item = ifelse(training == 1, paste0("\\textbf{",item,"}"),item),
                             crit = ifelse(training == 1, paste0("\\textbf{",crit,"}"),crit)) |> 
                      select(item,crit) 

items_mammals   <- read_csv2("Materials/design_data_mammals.csv")   |>
                      mutate(item = ifelse(training == 1, paste0("\\textbf{",item,"}"),item),
                             crit = ifelse(training == 1, paste0("\\textbf{",crit,"}"),crit)) |> 
                      select(item,crit) 

items_food      <- read_csv2("Materials/design_data_food.csv")      |>
                      mutate(item = ifelse(training == 1, paste0("\\textbf{",item,"}"),item),
                             crit = ifelse(training == 1, paste0("\\textbf{",crit,"}"),crit)) |> 
                      select(item,crit) 



bind_cols(items_food[1:20,],
          items_food[21:40,], 
          items_food[41:60,], 
          items_food[61:80,]) |> 
  kable(format    = "latex", digits=2, booktabs=TRUE, align="c",
        col.names = c("Item"," ",
                      "Item"," ",
                      "Item"," ",
                      "Item"," "),
        label="items_food",
        caption   = "Eighty food items with gramm carbohydrates per 100g.",
        escape    = FALSE) |> 
  kable_styling(latex_options ="striped", full_width = F)  |> 
  footnote(general           = "\\\\footnotesize\\{Items learned during the training phase are shown in bold.\\}",
           footnote_as_chunk = TRUE,
           threeparttable    = TRUE,
           escape            = FALSE,
           general_title     = "\\\\footnotesize\\{Note.\\}",
           title_format      = c("italic"))


bind_cols(items_countries[1:20,],
          items_countries[21:40,], 
          items_countries[41:60,], 
          items_countries[61:80,]) |> 
  kable(format    = "latex", digits=2, booktabs=TRUE, align="c",
        col.names = c("Item"," ",
                      "Item"," ",
                      "Item"," ",
                      "Item"," "),
        label="items_countries",
        caption   = "Eighty country items with life expectancy.",
        escape    = FALSE) |> 
  kable_styling(latex_options ="striped", full_width = F)  |> 
  footnote(general           = "\\\\footnotesize\\{Items learned during the training phase are shown in bold.\\}",
           footnote_as_chunk = TRUE,
           threeparttable    = TRUE,
           escape            = FALSE,
           general_title     = "\\\\footnotesize\\{Note.\\}",
           title_format      = c("italic"))




bind_cols(items_mammals[1:20,],
          items_mammals[21:40,], 
          items_mammals[41:60,], 
          items_mammals[61:80,]) |> 
  kable(format    = "latex", digits=2, booktabs=TRUE, align="c",
        col.names = c("Item"," ",
                      "Item"," ",
                      "Item"," ",
                      "Item"," "),
        label="items_mammals",
        caption   = "Eighty mammal items with days until female maturity.",
        escape    = FALSE) |> 
  kable_styling(latex_options ="striped", full_width = F)  |> 
  footnote(general           = "\\\\footnotesize\\{Items learned during the training phase are shown in bold.\\}",
           footnote_as_chunk = TRUE,
           threeparttable    = TRUE,
           escape            = FALSE,
           general_title     = "\\\\footnotesize\\{Note.\\}",
           title_format      = c("italic"))


