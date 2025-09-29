clrs <- c("#54AA8F"  ,"#00335B",
          "#22A884FF","#414487FF",
          "#496aa2"  ,"#e46c0a"  ,"#90b6d4")


# ggplot theme
theme_nice <- function(){
  theme_minimal(base_family = "Jost") +  
    theme(
      plot.title       = element_text(hjust = 0.5, size = 20),
      panel.grid.minor = element_blank(),
      text             = element_text(size  = 20),
      panel.border     = element_rect(colour = "black", linewidth = 0.5, fill = NA),
      axis.title.x     = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
      axis.title.y     = element_text(margin = unit(c(3, 3, 0, 0), "mm"), angle = 90),
      legend.title     = element_text(face = "bold",size=16),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom"
    )}