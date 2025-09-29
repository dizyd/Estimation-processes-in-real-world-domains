library(ggplot2)


# 5 Cues ------------------------------------------------------------------


stim <- c(0,0,0,1,1,70)
ex1  <- c(0,1,0,1,0,50)
ex2  <- c(0,0,1,1,1,80)
ex3  <- c(1,1,0,1,0,30)


sum(stim[1:5] == ex1[1:5]) # Matching between cues = 3
sum(stim[1:5] == ex2[1:5]) # Matching between cues = 4
sum(stim[1:5] == ex3[1:5]) # Matching between cues = 2

# Compute distances

ds_e1 <- sqrt((0-0)^2 + (0-1)^2 + (0-0)^2 + (1-1)^2 + (1-0)^2)
ds_e2 <- sqrt((0-0)^2 + (0-0)^2 + (0-1)^2 + (1-1)^2 + (1-1)^2)
ds_e3 <- sqrt((0-1)^2 + (0-1)^2 + (0-0)^2 + (1-1)^2 + (1-0)^2)

# ds_e1 = 1.414214
# ds_e2 = 1
# ds_e3 = 1.732051

# Relative difference in differences
ds_e1/ds_e2 # = 1.414214
ds_e1/ds_e3 # = 0.8164966
ds_e2/ds_e3 # = 0.5773503

# Compute similarities 
h     <- 2

ss_e1 <- exp(-1*h*ds_e1)
ss_e2 <- exp(-1*h*ds_e2)
ss_e3 <- exp(-1*h*ds_e3)

# ss_e1 = 0.05910575
# ss_e2 = 0.1353353
# ss_e3 = 0.03130111

# Relative difference in similarities
ss_e2/ss_e1 # = 2.289714
ss_e3/ss_e1 # = 0.5295782
ss_e3/ss_e2 # = 0.2312857

# Compute Predictions

(ss_e1*50 + ss_e2*80 + ss_e3*30)/(ss_e1+ss_e2+ss_e3)
# = 65.21221

##### With (equal) attetion weights but same h

# Compute distances

w     <- 1/5 

ds_e1 <- sqrt(w*(0-0)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2)
ds_e2 <- sqrt(w*(0-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(1-1)^2 + w*(1-1)^2)
ds_e3 <- sqrt(w*(0-1)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2)

# ds_e1 = 0.6324555
# ds_e2 = 0.4472136
# ds_e3 = 0.7745967

# Relative difference in differences
ds_e1/ds_e2 # = 1.414214
ds_e1/ds_e3 # = 0.8164966
ds_e2/ds_e3 # = 0.5773503

# Compute similarities 
h     <- 2

ss_e1 <- exp(-1*h*ds_e1)
ss_e2 <- exp(-1*h*ds_e2)
ss_e3 <- exp(-1*h*ds_e3)

# ss_e1 = 0.2822644
# ss_e2 = 0.4088417
# ss_e3 = 0.1353353

# Relative difference in similarities
ss_e2/ss_e1 # = 1.448435
ss_e3/ss_e1 # = 0.7525542
ss_e3/ss_e2 # = 0.5195635

# Compute Predictions

(ss_e1*50 + ss_e2*80 + ss_e3*30)/(ss_e1+ss_e2+ss_e3)

# = 58.87287



##### With (equal) attetion weights AND adjusted



foo <- function(h){
  
  # Compute similarities 
  temp_ss_e1 <- exp(-1*h*ds_e1)
  temp_ss_e2 <- exp(-1*h*ds_e2)
  temp_ss_e3 <- exp(-1*h*ds_e3)
  
  # Compute Predictions
  pred = (temp_ss_e1*50 + temp_ss_e2*80 + temp_ss_e3*30)/(temp_ss_e1+temp_ss_e2+temp_ss_e3)
  
  return(pred)
  
}

h_seq <- seq(0, 20, 0.1)
pred  <- foo(h_seq)

ggplot(data.frame(h = h_seq, pred = pred),  mapping = aes(x = h, y = pred)) +
  geom_point() +
  geom_line() + 
  annotate("point", x = 2, y = 65.21221, colour = "tomato1", size = 3) + 
  geom_hline(yintercept = 65.21221, colour = "tomato1") + 
  geom_vline(xintercept = 2, colour = "tomato1") + 
  geom_vline(xintercept = 4.5) +
  theme_minimal()


# 10 Cues ------------------------------------------------------------------

stim <- c(0,0,0,1,1,0,0,0,1,1,70)
ex1  <- c(0,1,0,1,0,0,1,0,1,0,50)
ex2  <- c(0,0,1,1,1,0,0,1,1,1,80)
ex3  <- c(1,1,0,1,0,1,1,0,1,0,30)

# Compute distances
ds_e1 <- sqrt((0-0)^2 + (0-1)^2 + (0-0)^2 + (1-1)^2 + (1-0)^2 + (0-0)^2 + (0-1)^2 + (0-0)^2 + (1-1)^2 + (1-0)^2)
ds_e2 <- sqrt((0-0)^2 + (0-0)^2 + (0-1)^2 + (1-1)^2 + (1-1)^2 + (0-0)^2 + (0-0)^2 + (0-1)^2 + (1-1)^2 + (1-1)^2)
ds_e3 <- sqrt((0-1)^2 + (0-1)^2 + (0-0)^2 + (1-1)^2 + (1-0)^2 + (0-1)^2 + (0-1)^2 + (0-0)^2 + (1-1)^2 + (1-0)^2)

# Compute similarities 
h     <- 2

ss_e1 <- exp(-1*h*ds_e1)
ss_e2 <- exp(-1*h*ds_e2)
ss_e3 <- exp(-1*h*ds_e3)

# Compute Predictions

(ss_e1*50 + ss_e2*80 + ss_e3*30)/(ss_e1+ss_e2+ss_e3)
# = 69.13494

##### With (equal) attetion weights but same h

# Compute distances

w     <- 1/10

ds_e1 <- sqrt(w*(0-0)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2)
ds_e2 <- sqrt(w*(0-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(1-1)^2 + w*(1-1)^2 + w*(0-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(1-1)^2 + w*(1-1)^2)
ds_e3 <- sqrt(w*(0-1)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2 + w*(0-1)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2)

h_seq <- seq(0, 20, 0.1)
pred  <- foo(h_seq)

ggplot(data.frame(h = h_seq, pred = pred),  mapping = aes(x = h, y = pred)) +
  geom_point() +
  geom_line() + 
  annotate("point", x = 2, y = 69.13494, colour = "tomato1", size = 3) + 
  geom_hline(yintercept = 69.13494, colour = "tomato1") + 
  geom_vline(xintercept = 2, colour = "tomato1") + 
  geom_vline(xintercept = 6.35) +
  theme_minimal()



##### With (equal) attetion weights but same h

# Compute distances

w     <- 1/10

ds_e1 <- sqrt((w*(0-0)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2)*10)
ds_e2 <- sqrt((w*(0-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(1-1)^2 + w*(1-1)^2 + w*(0-0)^2 + w*(0-0)^2 + w*(0-1)^2 + w*(1-1)^2 + w*(1-1)^2)*10)
ds_e3 <- sqrt((w*(0-1)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2 + w*(0-1)^2 + w*(0-1)^2 + w*(0-0)^2 + w*(1-1)^2 + w*(1-0)^2)*10)

h_seq <- seq(0, 20, 0.1)
pred  <- foo(h_seq)

ggplot(data.frame(h = h_seq, pred = pred),  mapping = aes(x = h, y = pred)) +
  geom_point() +
  geom_line() + 
  annotate("point", x = 2, y = 69.13494, colour = "tomato1", size = 3) + 
  geom_hline(yintercept = 69.13494, colour = "tomato1") + 
  geom_vline(xintercept = 2, colour = "tomato1") + 
  theme_minimal()





