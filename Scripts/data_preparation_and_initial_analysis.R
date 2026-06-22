# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(extrafont)
library(viridis)
library(psych)
library(afex)
library(kableExtra)
library(correlation)

source("Scripts/plot_settings.R")

# Load Data              ---------------------------------------------------------------

df_countries0 <- read_csv2("Data/data_raw_estimation_countries.csv") |> rename(knowledge = country_knowledge)
df_mammals0   <- read_csv2("Data/data_raw_estimation_mammals.csv")   |> rename(knowledge = mammal_knowledge)
df_food0      <- read_csv2("Data/data_raw_estimation_food.csv")      |> rename(knowledge = food_knowledge)

# Filter: Mammals        ------------------------------------------------------------------


# … if the mean response times in the testing phase is 3 SDs below the sample

sd_overall <- df_mammals0 |> filter(phase == "testing") |> pull(rt_ms) |> log() |> sd()
m_overall  <- df_mammals0 |> filter(phase == "testing") |> pull(rt_ms) |> log() |> mean()

ID_rts_3SD <-  df_mammals0 |>
                  filter(phase == "testing") |>
                  group_by(ID) |>
                  summarize(s_SD3 = mean(log(rt_ms))<(m_overall-3*sd_overall),
                            l_SD3 = mean(log(rt_ms))>(m_overall+3*sd_overall)) |> 
                  filter(s_SD3 | l_SD3)

df_mammals1 <-  df_mammals0 |> filter(!ID %in% ID_rts_3SD)


# … if they fail one of the three attention checks

ID_AC <-  df_mammals1 |> 
            filter(ID_item == "Basketball") |> 
            select(ID, est)  |> 
            filter(est != 999) |> 
            pull(ID) |> unique()

df_mammals2 <-  df_mammals1 |> filter(!ID %in% ID_AC)


# … if they make only two or less different estimates for all items in the testing phase.

ID_ests <-  df_mammals2 |> 
              filter(phase   == "testing", 
                     ID_item != "Basketball") |> 
              select(ID, est)  |> 
              group_by(ID) |> 
              summarize(n = n_distinct(est)) |> 
              filter(n <= 2) |> 
              pull(ID) |> unique()

df_mammals3 <-  df_mammals2 |> filter(!ID %in% ID_ests)

# … if they indicated non-compliance. That means they either indicate having cheated and or just having clicked through the experiment (Aust et al., 2013)

df_mammals4 <-  df_mammals3 |> filter(quality == "yes")
df_mammals5 <-  df_mammals4 |> filter(cheated == "no")

# … if they indicated and described technical problems or that the experiment did not work as intended.

df_mammals6 <-  df_mammals5 |> filter(technical_problems == "no")

# … if they provided judgments in under 1000 ms for more than 10% of the items in the estimation phase, as such short estimation processes are implausible.

ID_rts_1000 <-  df_mammals6 |> 
                  filter(phase   == "testing") |> 
                  select(ID, rt_ms)  |> 
                  group_by(ID) |> 
                  summarize(rt_1000 = mean(rt_ms < 1000)) |> 
                  filter(rt_1000 >= 0.1) |> 
                  pull(ID) |> unique()

df_mammals7 <-  df_mammals6 |> filter(!ID %in% ID_rts_1000)

# ... if # correct was less then 6 (50%) in the last training block

ID_training <- df_mammals7 |> 
                  filter(phase == "training", block == 10) |> 
                  group_by(ID) |> 
                  summarize(n_correct = sum(true < est*1.1 & true > est*0.9)) |> 
                  filter(n_correct < 6) |> 
                  pull(ID) |> unique()

df_mammals8 <-  df_mammals7 |> filter(!ID %in% ID_training) 
    



df_mammals0 |> pull(ID) |> n_distinct() #| N = 59
df_mammals1 |> pull(ID) |> n_distinct() #| N = 59
df_mammals2 |> pull(ID) |> n_distinct() #| N = 50
df_mammals3 |> pull(ID) |> n_distinct() #| N = 50
df_mammals4 |> pull(ID) |> n_distinct() #| N = 50
df_mammals5 |> pull(ID) |> n_distinct() #| N = 50
df_mammals6 |> pull(ID) |> n_distinct() #| N = 50
df_mammals7 |> pull(ID) |> n_distinct() #| N = 50
df_mammals8 |> pull(ID) |> n_distinct() #| N = 48

df_mammals <- df_mammals8 |> add_column(domain = "Mammals")

# Filter: Food           ------------------------------------------------------------------


# … if the mean response times in the testing phase is 3 SDs below the sample

sd_overall <- df_food0 |> filter(phase == "testing") |> pull(rt_ms) |> log() |> sd()
m_overall  <- df_food0 |> filter(phase == "testing") |> pull(rt_ms) |> log() |> mean()

ID_rts_3SD <-  df_food0 |>
  filter(phase == "testing") |>
  group_by(ID) |>
  summarize(s_SD3 = mean(log(rt_ms))<(m_overall-3*sd_overall),
            l_SD3 = mean(log(rt_ms))>(m_overall+3*sd_overall)) |> 
  filter(s_SD3 | l_SD3)

df_food1 <-  df_food0 |> filter(!ID %in% ID_rts_3SD)


# … if they fail one of the three attention checks

ID_AC <-  df_food1 |> 
            filter(ID_item == "Basketball") |> 
            select(ID, est)  |> 
            filter(est != 999) |> 
            pull(ID) |> unique()

df_food2 <-  df_food1 |> filter(!ID %in% ID_AC)


# … if they make only two or less different estimates for all items in the testing phase.

ID_ests <-  df_food2 |> 
              filter(phase   == "testing", 
                     ID_item != "Basketball") |> 
              select(ID, est)  |> 
              group_by(ID) |> 
              summarize(n = n_distinct(est)) |> 
              filter(n <= 2) |> 
              pull(ID) |> unique()

df_food3 <-  df_food2 |> filter(!ID %in% ID_ests)

# … if they indicated non-compliance. That means they either indicate having cheated and or just having clicked through the experiment (Aust et al., 2013)

df_food4 <-  df_food3 |> filter(quality == "yes")
df_food5 <-  df_food4 |> filter(cheated == "no")

# … if they indicated and described technical problems or that the experiment did not work as intended.

df_food6 <-  df_food5 |> filter(technical_problems == "no")

# … if they provided judgments in under 1000 ms for more than 10% of the items in the estimation phase, as such short estimation processes are implausible.

ID_rts_1000 <-  df_food6 |> 
                  filter(phase   == "testing") |> 
                  select(ID, rt_ms)  |> 
                  group_by(ID) |> 
                  summarize(rt_1000 = mean(rt_ms < 1000)) |> 
                  filter(rt_1000 >= 0.1) |> 
                  pull(ID) |> unique()

df_food7 <-  df_food6 |> filter(!ID %in% ID_rts_1000)

# ... if # correct was less then 6 (50%) in the last training block

ID_training <- df_food7 |> 
                filter(phase == "training", block == 10) |> 
                group_by(ID) |> 
                summarize(n_correct = sum(true < est*1.1 & true > est*0.9)) |> 
                filter(n_correct < 6) |> 
                pull(ID) |> unique()

df_food8 <-  df_food7 |> filter(!ID %in% ID_training) 



df_food0 |> pull(ID) |> n_distinct() #| N = 57
df_food1 |> pull(ID) |> n_distinct() #| N = 57
df_food2 |> pull(ID) |> n_distinct() #| N = 51
df_food3 |> pull(ID) |> n_distinct() #| N = 51
df_food4 |> pull(ID) |> n_distinct() #| N = 51
df_food5 |> pull(ID) |> n_distinct() #| N = 51
df_food6 |> pull(ID) |> n_distinct() #| N = 50
df_food7 |> pull(ID) |> n_distinct() #| N = 50
df_food8 |> pull(ID) |> n_distinct() #| N = 46

df_food <- df_food8 |> add_column(domain = "Food")

# Filter: Countries      ------------------------------------------------------------------


# … if the mean response times in the testing phase is 3 SDs below the sample

sd_overall <- df_countries0 |> filter(phase == "testing") |> pull(rt_ms) |> log() |> sd()
m_overall  <- df_countries0 |> filter(phase == "testing") |> pull(rt_ms) |> log() |> mean()

ID_rts_3SD <-  df_countries0 |>
                filter(phase == "testing") |>
                group_by(ID) |>
                summarize(s_SD3 = mean(log(rt_ms))<(m_overall-3*sd_overall),
                          l_SD3 = mean(log(rt_ms))>(m_overall+3*sd_overall)) |> 
                filter(s_SD3 | l_SD3)

df_countries1 <-  df_countries0 |> filter(!ID %in% ID_rts_3SD)


# … if they fail one of the three attention checks

ID_AC <-  df_countries1 |> 
            filter(ID_item == "Basketball") |> 
            select(ID, est)  |> 
            filter(est != 999) |> 
            pull(ID) |> unique()

df_countries2 <-  df_countries1 |> filter(!ID %in% ID_AC)


# … if they make only two or less different estimates for all items in the testing phase.

ID_ests <-  df_countries2 |> 
              filter(phase   == "testing", 
                     ID_item != "Basketball") |> 
              select(ID, est)  |> 
              group_by(ID) |> 
              summarize(n = n_distinct(est)) |> 
              filter(n <= 2) |> 
              pull(ID) |> unique()

df_countries3 <-  df_countries2 |> filter(!ID %in% ID_ests)

# … if they indicated non-compliance. That means they either indicate having cheated and or just having clicked through the experiment (Aust et al., 2013)

df_countries4 <-  df_countries3 |> filter(quality == "yes")
df_countries5 <-  df_countries4 |> filter(cheated == "no")

# … if they indicated and described technical problems or that the experiment did not work as intended.

df_countries6 <-  df_countries5 |> filter(technical_problems == "no")

# … if they provided judgments in under 1000 ms for more than 10% of the items in the estimation phase, as such short estimation processes are implausible.

ID_rts_1000 <-  df_countries6 |> 
                  filter(phase   == "testing") |> 
                  select(ID, rt_ms)  |> 
                  group_by(ID) |> 
                  summarize(rt_1000 = mean(rt_ms < 1000)) |> 
                  filter(rt_1000 >= 0.1) |> 
                  pull(ID) |> unique()

df_countries7 <-  df_countries6 |> filter(!ID %in% ID_rts_1000)

# ... if # correct was less then 6 (50%) in the last training block

ID_training <- df_countries7 |> 
                  filter(phase == "training", block == 10) |> 
                  group_by(ID) |> 
                  summarize(n_correct = sum(true < est*1.1 & true > est*0.9)) |> 
                  filter(n_correct < 6) |> 
                  pull(ID) |> unique()

df_countries8 <-  df_countries7 |> filter(!ID %in% ID_training)  


df_countries0 |> pull(ID) |> n_distinct() #| N = 66
df_countries1 |> pull(ID) |> n_distinct() #| N = 66
df_countries2 |> pull(ID) |> n_distinct() #| N = 52
df_countries3 |> pull(ID) |> n_distinct() #| N = 52
df_countries4 |> pull(ID) |> n_distinct() #| N = 50
df_countries5 |> pull(ID) |> n_distinct() #| N = 50
df_countries6 |> pull(ID) |> n_distinct() #| N = 48
df_countries7 |> pull(ID) |> n_distinct() #| N = 48
df_countries8 |> pull(ID) |> n_distinct() #| N = 48

df_countries     <- df_countries8 |> add_column(domain = "Countries")
df_countries$est <- as.numeric(df_countries$est)

# Combine                ---------------------------------------------------------


est  <- bind_rows(df_food, df_countries, df_mammals)

# delete old objects
rm(df_countries0,df_countries1,df_countries2,df_countries3,df_countries4,
   df_countries5,df_countries6,df_countries7,df_countries8,
   df_mammals0,df_mammals1,df_mammals2,df_mammals3,df_mammals4,
   df_mammals5,df_mammals6,df_mammals7,df_mammals8,
   df_food0,df_food1,df_food2,df_food3,df_food4,
   df_food5,df_food6,df_food7,df_food8,
   ID_rts_3SD, ID_AC, ID_ests, ID_rts_1000, ID_training, m_overall, sd_overall)


write_csv2(est, file = "Data/data_tidy_combined.csv")



# Demographics           ---------------------------------------------------------

demo <- est |> select(ID,age,gender,domain, knowledge) |> distinct()


table(demo$domain,demo$gender)

describeBy(age ~ domain, data = demo)

describeBy(knowledge ~ domain, data = demo)




# Training Phase         ---------------------------------------------------------

# nr. correct in last block
est |> 
  filter(phase == "training") |> 
  mutate(correct = ifelse(true < est * 1.1 & true > est * 0.9, 1, 0)) |>
  group_by(ID) |> 
  mutate(max_block = max(block)) |> 
  filter(block == max_block) |> 
  group_by(domain, ID) |> 
  summarize(n_correct = sum(correct), .groups = "drop") %>% 
  describeBy(n_correct ~ domain, data = .)

# nr. of blocks
est |> 
  filter(phase == "training") |> 
  mutate(correct = ifelse(true < est * 1.1 & true > est * 0.9, 1, 0)) |>
  group_by(ID) |> 
  mutate(max_block = max(block)) |> 
  filter(block == max_block) |> 
  select(domain, ID, max_block) |> 
  distinct() %>% 
  describeBy(max_block ~ domain, data = .)


# Plot
est |> 
    filter(phase == "training") |> 
    mutate(correct = ifelse(true < est * 1.1 & true > est * 0.9, 1, 0)) |> 
    group_by(ID, block, domain) |> 
    summarize(n_correct = sum(correct), .groups = "drop") |> 
    ggplot(aes(x = block, y = n_correct, color = ID)) +
      geom_line(show.legend = FALSE, linewidth = 0.9) +
      scale_x_continuous(breaks = 1:10) +
      scale_color_viridis_d(option = "plasma") +
      theme_nice() +
      facet_grid(.~domain)



# Correlation of true and estimated values during learning
est |> 
  filter(phase == "training") |>  
  group_by(ID, block, domain) |> 
  summarize(r = cor(est,true), .groups = "drop") |> 
  ggplot(aes(x = block,y = r, color = ID)) +
    geom_line(show.legend = FALSE, linewidth = 0.9) +
    scale_x_continuous(breaks = 1:10) +
    scale_color_viridis_d(option = "plasma") +
    theme_nice() +
    facet_grid(.~domain)


# Plot
est |> 
  filter(phase == "training") |> 
  mutate(correct = ifelse(true < est * 1.1 & true > est * 0.9, 1, 0)) |> 
  group_by(ID, block, domain, ID_item) |> 
  summarize(AE = abs(true-est), .groups = "drop") |> 
  group_by(ID, block, domain) |> 
  summarize(AE = mean(AE), .groups = "drop") |> 
  ggplot(aes(x = block, y = AE, color = ID)) +
    geom_line(show.legend = FALSE, linewidth = 0.9) +
    scale_x_continuous(breaks = 1:10) +
    scale_color_viridis_d(option = "plasma") +
    theme_nice() +
    facet_wrap(.~domain,scales="free")



est |> 
  filter(phase == "training") |> 
  mutate(correct = ifelse(true < est * 1.1 & true > est * 0.9, 1, 0)) |> 
  group_by(ID) |> 
  mutate(max_block = max(block)) |> 
  group_by(ID, block, domain) |> 
  summarize(n_correct = sum(correct), .groups = "drop",
            max_block = mean(max_block))  |> 
  filter(block == max_block) |> 
  group_by(domain) |> 
  summarize(m = mean(n_correct),
            sd = sd(n_correct),
            min = min(n_correct), 
            max = max(n_correct),
            m_max_block = mean(max_block), .groups = "drop")



# Testing Phase          -----------------------------------------------------

testing <- est |> filter(phase == "testing", ID_item != "Basketball")

# Filter Trials
# Number of trials (K) BEFORE:

testing |> filter(domain == "Mammals") |> nrow()   # 3840
testing |> filter(domain == "Food") |> nrow()      # 3680
testing |> filter(domain == "Countries") |> nrow() # 3840

# Set all trials to NA which:
# for Food:      > 100
# for Countries: > 100
# for Mammals:   > 10,000

# Number of trials (K) AFTER:

testing <- testing |> 
            mutate(est = case_when(domain == "Mammals"   & est > 10000 ~ NA,
                                   domain == "Food"      & est > 100   ~ NA,
                                   domain == "Countries" & est > 100   ~ NA,
                                   TRUE                                ~ est))

testing |> filter(domain == "Mammals", !is.na(est))   |> nrow() # 3840
testing |> filter(domain == "Food", !is.na(est))      |> nrow() # 3674
testing |> filter(domain == "Countries", !is.na(est)) |> nrow() # 3838


# Descr. stats of estimated and true criterion values
  
tbl_est <- testing |> 
              filter(training == 0) |> 
              group_by(domain) |> 
              summarize(m_est   = mean(est, na.rm=T),
                        sd_est  = sd(est, na.rm=T),
                        min_est = min(est, na.rm=T),
                        max_est = max(est, na.rm=T),
                        m_T     = mean(true),
                        sd_T    = sd(true),
                        min_T   = min(true),
                        max_T   = max(true)) 

tbl_est 

tbl_est |> 
  kable(format    = "latex", digits=2, booktabs=TRUE, align="c",
        col.names = c("Domain","$M$","$SD$","Min.","Max.",
                      "$M$","$SD$","Min.","Max."),
        label     = "descr_est",
        caption   = "Descriptive statistics for participants’ estimated values 
                    and the corresponding true criterion values
                    during the testing phase, across domains.
        Reported are means ($M$), standard deviations ($SD$), and the observed range (Min, Max) for each domain",
        escape    = FALSE) |> 
  footnote(general           = "\\\\footnotesize\\{.\\}",
           footnote_as_chunk = TRUE,
           threeparttable    = TRUE,
           escape            = FALSE,
           general_title     = "\\\\footnotesize\\{Note.\\}",
           title_format      = c("italic")) |> 
  add_header_above(c(" "=1,"Estimated"=4,"True" = 4))

# Calcualte wisdom of crowds correlation
# (i.e., correlation between average estimated and true)

# Plot continued to script analysis_predictions.R
testing |>
  filter(training == 0) |>
  group_by(domain, ID_item) |>
  summarize(m_true = mean(true, na.rm=T),
            m_est  = mean(est,  na.rm=T)) |>
  group_by(domain) |>
  correlation()

r_df <- data.frame(domain = c("Countries","Food","Mammals"),
                   r      = c("italic('r')~`=`~.84","italic('r')~`=`~.70","italic('r')~`=`~.72"),
                   x      = c(60, 20, 1000),
                   y      = c(80,68, 3700))


# Plot distribution of true and actual estimates

temp <- testing |> select(domain,true,training) |> distinct() |> add_column(y = 0)

p_dists <- testing |>
            ggplot(aes(x = est,color=ID)) +
              geom_density(show.legend = F,  adjust = 2)+
              geom_density(aes(x = true), color  = "black", linewidth=1.5, adjust = 2) +
              geom_point(aes(x = true, y=y), data = temp |> filter(training == 0), pch = "|", stroke=3, size = 4,color = "black") +
              geom_point(aes(x = true, y=y), data = temp |> filter(training == 1), pch = "|", stroke=3, size = 4,color = "red") +
              scale_color_viridis_d(direction =-1) + # option = "plasma"
              theme_nice() + labs(x = "Criterion", y = "Density") +
              facet_wrap(.~domain, scales="free") +
              scale_y_continuous(expand = c(0, 0), limits  = function(lim){
                  lim[2] <- lim[2]+lim[2]/8; return(lim)}) +
              theme(axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())


p_avg <- testing |>
          filter(training == 0) |>
          group_by(domain, ID_item) |>
          summarize(m_true = mean(true, na.rm=T),
                    m_est  = mean(est,  na.rm=T),
                    se     = sd(est, na.rm=T)/sqrt(length(est)),
                    .groups="drop") |>
          ggplot(aes(x = m_true, y = m_est)) +
            geom_errorbar(aes(ymin = m_est-se/2, ymax = m_est+se/2), width = 0) +
            geom_point(size = 2, shape = 21, fill = "grey") +
            geom_abline(intercept = 0, slope = 1, linewidth = 1, lty = "dashed") +
            geom_smooth(method='lm', color = clrs[1], size = 1.5) +
            geom_text(aes(x, y, label=r), data=r_df, vjust=1, size = 5,
                      parse = T) +
            facet_wrap(.~domain, scales="free") +
            theme_nice() + labs(x = "True Criterion", y = "Avg. Estimated Criterion")



p_dists / p_avg + plot_annotation(tag_levels = "A")

save(list = c("p_dists","p_avg"),file = "Figures/ggplot_Figure2.Rdata")
ggsave("Figures/distribution_estimates0.pdf",width=30,height=20,unit="cm",device = cairo_pdf)


# Performane in AE

testing |> 
  mutate(items = ifelse(training == 1, "old","new")) |> 
  group_by(ID, domain, items) |> 
  summarize(RMSE  = sqrt(mean((true-est)^2,na.rm=T)),.groups = "drop") |> 
  group_by(domain, items) |> 
  summarize(m = mean(RMSE),
            sd = sd(RMSE),.groups = "drop")


testing |> 
  mutate(items = ifelse(training == 1, "old","new")) |> 
  group_by(ID, domain, items) |> 
  summarize(RMSE  = sqrt(mean((true-est)^2,na.rm=T)),.groups = "drop") %>%  
  aov_ez(id = "ID", dv = "RMSE", data = ., between = "domain", within = "items")


testing |> 
  mutate(items = ifelse(training == 1, "old","new")) |> 
  group_by(ID, domain, items) |> 
  summarize(RMSE  =  sqrt(mean((true-est)^2,na.rm=T)),.groups = "drop")  |> 
  pivot_wider(names_from  = items,
              values_from = RMSE) |> 
  group_by(domain) |> 
  summarize(t  = t.test(new,old,var.equal=T,paired = T) |> papaja::apa_print() %>% .$statistic,
            d  = lsr::cohensD(new, old, method = "paired"))




testing |> 
  mutate(items = ifelse(training == 1, "old","new")) |> 
  group_by(ID, domain, items) |> 
  summarize(RMSE  =  sqrt(mean((true-est)^2,na.rm=T)),.groups = "drop") |> 
  mutate(domain_f = factor(domain, levels=c("Food","Countries","Mammals"))) |> 
  ggplot(aes(x = items,y = RMSE)) +
  geom_jitter(width=0.1,alpha=0.25, group = 1, size = 1.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar",width=0.1) +
    stat_summary(fun = mean, geom="line",lwd=0.75,aes(group = 1)) +
    stat_summary(fun = mean, geom="point", size = 2, shape = 21,
                 fill = clrs[1], color = "black", stroke = 1) +
    facet_wrap(.~domain_f,scales="free") +
    theme_nice() +
    labs(x = "Items", y = "RMSE")


ggsave("Figures/RMSE_testing.pdf",width=20,height=8,unit="cm",device = cairo_pdf)








# Save Data for Analysis ---------------------------------------------------------------


df_mammals_wide <- testing |>
                    filter(domain == "Mammals") |> 
                    mutate(item    = ID_item,
                           ID_item = str_extract(img, "\\d+") |> as.numeric()) |> 
                    select(ID, ID_item, item, img, training, crit = true, est) |>
                    pivot_wider(names_from = "ID", values_from = "est") |> 
                    arrange(ID_item)


write_csv(df_mammals_wide, file="Data/data_analysis_mammals.csv")



df_food_wide <- testing |>
                    filter(domain == "Food") |>  
                    mutate(item    = ID_item,
                           ID_item = str_extract(img, "\\d+") |> as.numeric()) |> 
                    select(ID, ID_item, item, img, training, crit = true, est) |>
                    pivot_wider(names_from = "ID", values_from = "est") |> 
                    arrange(ID_item)


write_csv(df_food_wide, file="Data/data_analysis_food.csv")



df_countries_wide <- testing |>
                        filter(domain == "Countries") |>  
                        mutate(item    = ID_item,
                               ID_item = str_extract(img, "\\d+") |> as.numeric()) |> 
                        select(ID, ID_item, item, img, training, crit = true, est) |>
                        pivot_wider(names_from = "ID", values_from = "est") |> 
                        arrange(ID_item)


write_csv(df_countries_wide, file="Data/data_analysis_countries.csv")


# Make DF dictionary with IDs and ID_n (i.e., positional codings of IDs)

ids_countries <- names(df_countries_wide[,6:ncol(df_countries_wide)])
ids_food      <- names(df_food_wide[,6:ncol(df_food_wide)])
ids_mammals   <- names(df_mammals_wide[,6:ncol(df_mammals_wide)])

dic_df <- data.frame(IDs    = c(ids_food,ids_countries,ids_mammals),
                     ID_n   = c(1:length(ids_food),1:length(ids_countries),1:length(ids_mammals)),
                     domain = c(rep("Food",length(ids_food)),
                                rep("Countries",length(ids_countries)),
                                rep("Mammals",length(ids_mammals))))

write_csv2(dic_df, file="Data/ID_dictionaries.csv")


