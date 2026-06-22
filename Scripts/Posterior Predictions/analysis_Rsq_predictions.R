# Load Packages               -----------------------------------------------------------

library(tidyverse)
library(viridis)
library(patchwork)
library(extrafont)
library(kableExtra)
library(bayestestR)
library(tidybayes)
library(psych)
library(correlation)

source("Scripts/plot_settings.R")


# Load  Data                  ---------------------------------------------------------------

preds_mammals    <- read_csv("Results/Posterior Predictions/pred_MAMMALS_27082025.csv")   |> select(-...1)
preds_food       <- read_csv("Results/Posterior Predictions/pred_FOOD_27082025.csv")      |> select(-...1)
preds_countries  <- read_csv("Results/Posterior Predictions/pred_COUNTRIES_27082025.csv") |> select(-...1)

best_mods        <- read_csv2("Results/Model Comparison/best_mods.csv") 

# Aggregate & Combine         ---------------------------------------------------------------

df_mammals <- preds_mammals |> 
                  rowwise() |> 
                  mutate(ID_ind  = ID_ind + 1,
                         md_pred = median(i0:i4999)) |> 
                  select(ID_ind,model,test_trial,est,crit,md_pred)  |> 
                  left_join(best_mods |> filter(domain == "Mammals"),
                            by = join_by("ID_ind" == "ID_n"))


df_food <- preds_food |> 
            rowwise() |> 
            mutate(ID_ind  = ID_ind + 1,
                   md_pred = median(i0:i4999)) |> 
            select(ID_ind,model,test_trial,est,crit,md_pred)  |> 
            left_join(best_mods |> filter(domain == "Food"),
                      by = join_by("ID_ind" == "ID_n"))



df_countries <- preds_countries |> 
                  rowwise() |> 
                  mutate(ID_ind  = ID_ind + 1,
                         md_pred = median(i0:i4999)) |> 
                  select(ID_ind,model,test_trial,est,crit,md_pred)  |> 
                  left_join(best_mods |> filter(domain == "Countries"),
                            by = join_by("ID_ind" == "ID_n"))

df <- bind_rows(df_food,df_countries,df_mammals)


# Plot avg. predicted vs. est ---------------------------------------------------------------

aggr_pred <- df |>
              filter(best_mod_ind < 5, model == best_mod) |> 
              select(ID,domain,test_trial,est,md_pred) |> 
              distinct() 

# For Table 2 (Median Predicted)
describeBy(md_pred ~ domain, data = aggr_pred)


# For correlations shown in Figure 4C
aggr_pred |>  
  group_by(domain, test_trial) |> 
  summarize(true_est  = mean(est),
            aggr_pred = mean(md_pred),.groups="drop") |> 
  group_by(domain) |> correlation(select = "true_est", select2 = "aggr_pred")


r_df2 <- data.frame(domain = c("Countries","Food","Mammals"),
                   r      = c("italic('r')~`=`~.89","italic('r')~`=`~.92","italic('r')~`=`~.89"),
                   x      = c(66, 15, 1000),
                   y      = c(79, 54, 3600))

# Plot distribution of true and actual estimates


load("Figures/ggplot_Figure2.Rdata")

p_pred <- df |>
          filter(best_mod_ind < 5, model == best_mod) |> 
          select(ID,domain,test_trial,est,md_pred) |> 
          distinct() |> 
          group_by(domain, test_trial) |> 
          summarize(m_true_est   = mean(est, na.rm=T),
                    m_aggr_pred  = mean(md_pred,  na.rm=T),
                    se           = sd(md_pred, na.rm=T)/sqrt(length(md_pred)),
                    .groups="drop") |> 
          ggplot(aes(x = m_true_est, y = m_aggr_pred)) +
            geom_errorbar(aes(ymin = m_aggr_pred-se/2, ymax = m_aggr_pred+se/2), width = 0) +
            geom_point(size = 2, shape = 21, fill = "grey") +
            geom_abline(intercept = 0, slope = 1, linewidth = 1, lty = "dashed") +
            geom_smooth(method='lm', color = clrs[4], size = 1.5) +
            geom_text(aes(x, y, label=r), data=r_df2, vjust=1, size = 5,
                      parse = T) + 
            facet_wrap(.~domain, scales="free") +
            theme_nice() + labs(x = "Avg. Estimated Criterion", y = "Avg. Predicted Criterion") 



p_dists / p_avg / p_pred + plot_annotation(tag_levels = "A")


ggsave("Figures/distribution_estimates.pdf",width=30,height=31,unit="cm",device = cairo_pdf)





# R² by best fitting model    ---------------------------------------------------------------
  


# Table 4
df |> 
  filter(model == best_mod) |> 
  group_by(domain,ID_ind,model) |> 
  summarize(r = cor(est,md_pred)) |> 
  group_by(domain, model) |> 
  summarize(m  = mean(r^2),
            sd = sd(r^2),
            min = min(r^2),
            max = max(r^2),
            n  = n()) |> 
  kable(format    = "latex", digits=2, booktabs=TRUE, align="c",
        col.names = c("Domain","Model","$M$","$SD$","Min.","Max.","$n$"),
        label     = "r_sq",
        caption   = "Rdasd",
        escape    = FALSE)


# For domain average levels
df |> 
  filter(model == best_mod) |> 
  group_by(domain,ID_ind,model) |> 
  summarize(r = cor(est,md_pred)) |> 
  group_by(domain) |> 
  summarize(m  = mean(r^2),
            sd = sd(r^2))



# Save compute R² table for easy access
df |> 
  filter(model == best_mod) |> 
  group_by(domain,ID_ind,model) |> 
  summarize(r   = cor(est,md_pred),
            r2 = r^2)|> 
  write_csv2("Results/Posterior Predictions/r2_per_person.csv")


# Plot  RMSE ~ R²             ---------------------------------------------------------------

rsq <- df |> 
        filter(model == best_mod) |> 
          group_by(domain,ID,ID_ind,model) |> 
          summarize(r2  = cor(est,md_pred)^2,
                    RMSE = sqrt(mean((est-crit)^2,na.rm=T)),
                    pmp = mean(best_mod_pmp), .groups="drop")

# Correlations for Figure 8
rsq |> 
  # filter((domain == "Countries" & RMSE < 14)| domain == "Food"|domain == "Mammals") |> 
  group_by(domain) |> 
  select(-ID_ind, -pmp) |> 
  correlation() 


r_df2 <- data.frame(domain_f = factor(c("Food","Countries","Mammals"), levels=c("Food","Countries","Mammals")),
                    r      = c("italic('r')~`=`~.34*'*'","italic('r')~`=`~.38*'*' ","italic('r')~`=`~.31* '*' "),
                    y      = c(0.10,0.65,0.75),
                    x      = c(10,15,1100))



# Figure 8
rsq |> 
  # filter((domain == "Countries" & RMSE < 14)| domain == "Food"|domain == "Mammals") |> 
  mutate(domain_f = factor(domain, levels=c("Food","Countries","Mammals"))) |> 
  ggplot(aes(x = RMSE, y = r2)) +
    geom_point(size = 3, shape = 21,aes(fill=pmp)) +
    scale_fill_viridis(name = "Posterior\nModel\nProbability", limits = c(0, 1)) +
    geom_smooth(method='lm', color = "black", size = 1.5) +
    geom_text(aes(x, y, label=r), data=r_df2, vjust=1, size = 5,
              parse = T) + 
    facet_wrap(.~domain_f, scales="free") +
    theme_nice() +
    labs(x = "Estimation Accuracy (RMSE)", y = "Prediction Success (R²)") +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          plot.title = element_text(face="bold"))
  

ggsave("Figures/scatterplot_RMSE_r2_pmp.pdf",width=30,height=10,unit="cm",device = cairo_pdf)
# ggsave("Figures/scatterplot_RMSE_r2_pmp_wo.pdf",width=30,height=10,unit="cm",device = cairo_pdf)


# Playground                  --------------------------------------------------------------

IDs <- rsq |> filter(r2 < 0.1) |> pull(ID)


ggplot(df |> filter(ID %in% IDs, model == best_mod),
         aes(x = est, y  = md_pred)) +
  #geom_line(aes(x = crit, y = crit), lty = "dashed") +
  geom_point(size = 2, shape = 21, fill = "grey") +
  geom_point(aes(y = crit), size = 2, shape = 21, fill = "red",alpha = 0.25) +
  geom_smooth(method='lm', color = "black", size = 1.5) + 
  theme_nice() + 
  facet_wrap(domain~ID, scales="free", ncol = 2) +
  labs(x = "Estimated", y = "Predicted")

ggsave("temp.png",width=15,height=60,unit="cm",bg="white")






ggplot(df |> filter(ID %in% IDs, model == best_mod)) +
  geom_histogram(aes(x = est), fill = clrs[1], bins=30, alpha = 0.5) +
  geom_histogram(aes(x = md_pred), fill = clrs[4], bins=30, alpha = 0.5) +
  geom_histogram(aes(x = crit), fill = "grey", bins=30, alpha = 0.5) +
  theme_nice() + 
  facet_wrap(domain~ID, scales="free", ncol = 2)

ggsave("temp.png",width=15,height=60,unit="cm",bg="white")



