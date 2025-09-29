# load packages   -----------------------------------------------------------
  
library(tidyverse)  # ggplot, dplyr, and friends
library(brms)       # Bayesian modeling through Stan
library(tidybayes)  # Manipulate brms objects in a tidy way
library(ggdist)
library(distributional)
library(parameters) # Nicer output of model results
library(marginaleffects)
library(latex2exp) 
library(ggpubr)
library(patchwork)



theme_nice <- function(){
  theme_minimal(base_family = "Jost") +  
    theme(
      plot.title       = element_text(hjust = 0, size = 23, face="bold"),
      plot.subtitle    = element_text(hjust = 0.5, size = 15, face="bold"),
      panel.grid.minor = element_blank(),
      text             = element_text(size  = 18),
      panel.border     = element_rect(colour = "black", linewidth = 0.5, fill = NA),
      axis.title.x     = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
      axis.title.y     = element_text(margin = unit(c(3, 3, 0, 0), "mm"), angle = 90),
      legend.title     = element_text(face = "bold",size=16),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom"
    )}


set.seed(1234)

n <- 1e4

# Plot Priors CAM -------------------------------------------------

# Food
f1 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_normal(21.7, 20))) +
        labs(y = "Density", x = TeX("$\\beta_{0}$ ~ Normal(21.7, 20)"),
             subtitle = "Food") +
        theme_nice()

f2 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_normal(0, 25))) +
        labs(y = "Density", x = TeX("$\\beta_{i}$ ~ Normal(0, 25)"), subtitle = "") +
        theme_nice()

# Countries
c1 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_normal(73.32, 15))) +
        labs(y = "", x = TeX("$\\beta_{0}$ ~ Normal(73.32, 15)"),
             subtitle = "Countries") +
        theme_nice()

c2 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_normal(0, 15))) +
        labs(y = "", x = TeX("$\\beta_{i}$ ~ Normal(0, 15)"), subtitle = "") +
        theme_nice()


# Mammals
m1 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_normal(984.65, 300))) +
        labs(y = "", x = TeX("$\\beta_{0}$ ~ Normal(984.65, 300)"),
             subtitle = "Mammals") +
        theme_nice()

m2 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_normal(0, 750))) +
        labs(y = "", x = TeX("$\\beta_{i}$ ~ Normal(0, 750)"), subtitle = "") +
        theme_nice()




plot_CAM <- (f1 + c1 + m1)/(f2 + c2 + m2) +
              plot_annotation(title = "CAM") &  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold"))



# Plot Priors General -------------------------------------------------

sigma1 <- ggplot() +
            stat_halfeye(aes(xdist =  dist_exponential(0.25))) +
            labs(y = "Density", x = TeX("$\\sigma$ ~ Exp(0.25)"),
                 subtitle = "Food/Countries") +
            theme_nice() + theme(plot.title = element_text(face="bold", hjust = 0)) +
            theme(axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())



# Mammals
sigma2 <- ggplot() +
            stat_halfeye(aes(xdist =  dist_exponential(0.01))) +
            labs(y = "", x = TeX("$\\sigma$ ~ Exp(0.01)"),
                 subtitle = "Mammals") +
            theme_nice() + theme(plot.title = element_text(face="bold")) +
            theme(axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())

plot_GEN <- sigma1 + sigma2 + plot_annotation(title = "General/RGuess") &  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold"))


# Plot GCM -------------------------------------------------


GCM1 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_exponential(0.1))) +
        labs(y = "Density", x = TeX("$\\c$ ~ Exp(0.1)"), subtitle = "") +
        theme_nice() + 
        theme(axis.text.y  = element_blank(),
              axis.ticks.y = element_blank())

GCM2 <- ggplot() +
        stat_histinterval(aes(xdist =  dist_uniform(0,14))) +
        labs(y = "", x = TeX("$\\w_{i}$ ~ Dirichlet(1)*$n_{dim}$"), subtitle = "") +
        scale_x_continuous(breaks=c(0,5,10,14)) +
        theme_nice() +
        theme(axis.text.y  = element_blank(),
              axis.ticks.y = element_blank())

plot_GCM <- GCM1 + GCM2 + plot_annotation(title = "GCM") &  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold"))


# Plot RULEXJ -------------------------------------------------

rulexj <- ggplot() +
            stat_halfeye(aes(xdist =  dist_uniform(0,1))) +
            labs(y = "Density", x = TeX("$\\alpha$ ~ Uniform(0,1)"), subtitle = "") +
            theme_nice() +
            theme(axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())


plot_RULEXJ <- rulexj + plot_annotation(title = "RulEx-J") &  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold"))

# Plot Mapping -------------------------------------------------

mapping <- ggplot() +
            stat_histinterval(aes(xdist =  dist_poisson(5))) +
            labs(y = "", x = TeX("$\\italic(g)$ ~ $Poisson_{(2,12)}$(5)"),subtitle ="") +
            scale_x_continuous(breaks=c(2,4,6,8,10,12), limits = c(2,12)) +
            theme_nice() + 
            theme(axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())

plot_MAPP <- mapping + plot_annotation(title = "Mapping") &  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold"))

# Plot QEST -------------------------------------------------

QEst1 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_truncated(dist_cauchy(0,10),0.001,100))) +
        labs(y = "Density", x = TeX("$\\sigma_{QEst}$ ~ $Cauchy_{(0.001,100)}$(0,10)"),
             subtitle = "Food/Countries") +
        theme_nice() + 
        theme(axis.text.y  = element_blank(),
              axis.ticks.y = element_blank())


# Mammals
QEst2 <- ggplot() +
        stat_halfeye(aes(xdist =  dist_truncated(dist_cauchy(0,100),0.001,1000)))+
        labs(y = "", x = TeX("$\\sigma_{QEst}$ ~ $Cauchy_{(0.001,1000)}$(0,100)"),
             subtitle = "Mammals") +
        theme_nice() +
        theme(axis.text.y  = element_blank(),
              axis.ticks.y = element_blank())

plot_QEst <- (QEst1 +  plot_spacer() + QEst2 +  plot_spacer()) + plot_layout(ncol = 4, widths=c(3,1,3,1)) +
  plot_annotation(title = "Qest") &  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) 
  


# Plot Combine -------------------------------------------------


r1 <- wrap_elements(plot_GEN) + wrap_elements(plot_MAPP) + plot_layout(widths = c(2, 1))
r2 <- wrap_elements(plot_GCM) + wrap_elements(plot_RULEXJ) + plot_layout(widths = c(2, 1))
r3 <- wrap_elements(plot_CAM)
r4 <- wrap_elements(plot_QEst) 


r1/r2/r3/r4 + plot_layout(heights = c(1, 1, 2, 1))

ggsave("Figures/priors.pdf",height=36,width=23,unit="cm",device = cairo_pdf)
