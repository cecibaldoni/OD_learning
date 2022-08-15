library(rethinking)
library(rstan)
library(janitor)
library(cmdstanr)
library(stringr)
library("bayesplot")

d <- read.csv("OD_Task.csv")
d <- clean_names(d)
str(d)
d$id <- str_trim(d$id)
unique(d$id)
d$obs_id <- d$test_id
for (i in 1:nrow(d) ){
  d$obs_id[i] <- ifelse(d$season[i]=="winter" , d$obs_id[i] + 10 , d$obs_id[i] )
  d$obs_id[i] <- ifelse(d$season[i]=="spring" , d$obs_id[i] + 20 , d$obs_id[i] )
}
d$right <- ifelse(d$left==0 , 1 , 0)
d$id_index <- as.integer(as.factor(d$id))
d$datapt_index <- 1:nrow(d)
#fit RL model no seasons
### individual learning models
datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$id_index) ),         #number of individuals
  K = 2,                              # number of choice (L or R)
  tech = d$left + 1,                     #technique index  ##Add L o R
  pay_i = cbind( d$left*d$success , d$right*d$success ),  #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$obs_id,                          #processing bout unique to individual J
  id = d$id_index ,                      #individual ID
  N_effects=2                           #number of parameters to estimates
)

file <- file.path("il.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_i <- mod$sample(
  data = datalist_i,
  seed = 23,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=2,
  adapt_delta = 0.95,
)

extract_samples2 <- function(cmdstanrfit) {
  cmdstanrfit$output_files() |>
    rstan::read_stan_csv() |>
    rethinking::extract.samples()
}

post <- extract_samples2(fit_i)
str(post)
precis(post)
apply(post$phi_i , 2 , median)
apply(post$lambda_i , 2 , median)
apply(post$PrPreds[,,1])
str(post)
hist(post$lambda)
hist(post$phi)

pdf(file = "rl_plot_preds.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 11) # The height of the plot in inches
par(mfrow=c(7,2) , mar=c(2,2,0,0)+1)
for (i in 1:max(d$id_index)){
  shrew <- d[d$id_index==i,]
  shrew$pred_med <- apply(post$PrPreds[,shrew$datapt_index,1] , 2, median)
  pch_index <- c(1,19)
  col_index <- c("slateblue","pink3")
  plot(shrew$obs_id,rep(1.05, nrow(shrew) ), ylim=c(0,1.1),data=shrew , pch=pch_index[shrew$success +1] , col=col_index[shrew$right+1])
  lines(shrew$obs_id , shrew$pred_med , col=col_index[1], lw=3)
  for (j in 1:100){
    lines(shrew$obs_id , post$PrPreds[j,shrew$datapt_index,1] , col=col.alpha(col_index[1] , alpha=0.20), lw=1)
  }
  abline(v=10.5)
  abline(v=20.5)
}
dev.off()

