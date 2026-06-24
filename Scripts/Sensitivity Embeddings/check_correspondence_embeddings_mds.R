# Load Packages          -----------------------------------------------------------

library(tidyverse)
library(rdist)

library(patchwork)
library(extrafont)
library(viridis)
library(psych)
library(afex)
library(kableExtra)
library(correlation)

source("Scripts/plot_settings.R")


# helper functions --------------------------------------------------------

lower_triangle <- function(X){
  # Extract lower triangular values of matrix
  
  X[lower.tri(X)]
  
}
cosine_mat     <- function(X){

  sim   <- X / sqrt(rowSums(X * X))
  sim   <- sim %*% t(sim)
  
  D_sim <- (1 - sim)
  
  return(D_sim)
}

# Load Data              ---------------------------------------------------------------

# MDS Dimensions
mds_mammals   <- read_csv2("Data/Multidimensional Scaling/MDS_config_mammals.csv")  
mds_food      <- read_csv2("Data/Multidimensional Scaling/MDS_config_food.csv")  
mds_countries <- read_csv2("Data/Multidimensional Scaling/MDS_config_countries.csv")  

# Embeddings
word_embed_mammals <- read_csv("Data/Embeddings/embeddings_mammals_bge.csv") 
img_embed_mammals  <- read_csv("Data/Embeddings/image_embeddings_mammals_vgg16.csv") 

word_embed_food    <- read_csv("Data/Embeddings/embeddings_food_bge.csv") 
img_embed_food     <- read_csv("Data/Embeddings/image_embeddings_food_vgg16.csv") 


embed_countries    <- read_csv("Data/Embeddings/embeddings_countries_bge.csv") 

# Concat Embeddings (see De Deyne et al. 2021) ----------------------------

embed_mammals   <- cbind(word_embed_mammals[,5:1028],img_embed_mammals[,2:4096]) |> as.matrix()

embed_food      <- cbind(word_embed_food[,5:1028],img_embed_food[,2:4096]) |> as.matrix()

embed_countries <- embed_countries[,5:1028] |> as.matrix()

# Compute Distances -------------------------------------------------------

dist_mammals_mds   <- mds_mammals   |> as.matrix() |> pdist() |> lower_triangle()
dist_food_mds      <- mds_food      |> as.matrix() |> pdist() |> lower_triangle()
dist_countries_mds <- mds_countries |> as.matrix() |> pdist() |> lower_triangle()


dist_mammals_embed   <- embed_mammals   |> cosine_mat() |> lower_triangle()
dist_food_embed      <- embed_food      |> cosine_mat() |> lower_triangle()
dist_countries_embed <- embed_countries |> cosine_mat() |> lower_triangle()


# Create PCAs

temp                <- embed_mammals |> prcomp()
pca_mammals_embed   <- temp$x[,1:20] |> pdist() |> lower_triangle()

temp                <- embed_food    |> prcomp()
pca_food_embed      <- temp$x[,1:20] |> pdist() |> lower_triangle()

temp                <- embed_countries |> prcomp()
pca_countries_embed <- temp$x[,1:20]   |> pdist() |> lower_triangle()

# Analysis ----------------------------------------------------------------

dists <- data.frame(dist_mammals_mds,dist_food_mds,dist_countries_mds,
                    dist_mammals_embed,dist_food_embed,dist_countries_embed,
                    pca_mammals_embed,pca_food_embed,pca_countries_embed)


correlation(dists |> select(contains("mammals")))
correlation(dists |> select(contains("food")))
correlation(dists |> select(contains("countries")))

