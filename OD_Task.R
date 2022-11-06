library(dplyr) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(ROCR)#for calculating area under the curve (AUC) statistics
library(pROC)
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model
library(ggplot2)
library(rstan)
library(tidyr)

od_data = read.csv(file = "C:/Users/cecil/Desktop/SHREW/DATA/OD_Task.csv",
                   header = TRUE, sep = ",", dec = ".", na.strings = "NA")
#od_data = read.csv(file = "OD_Task.csv",
#                   header = TRUE, sep = ",", dec = ".", na.strings = "NA")

# make winter reference category
od_data$seasonF <- relevel(as.factor(od_data$season), ref = "winter")
od_data$test_ID <- factor(od_data$test_ID, ordered = TRUE)
od_data$ID <- as.factor(od_data$ID)


subset <- od_data %>%
  select(ID, season, seasonF, test_ID, success)
str(subset)

contrasts(od_data$test_ID, how.many=3) <- contr.poly(10)
contrasts(od_data$test_ID)
#if I want second order I just have linear+quadratic -> how.many=2

## which priors you can set
get_prior(success ~ seasonF + (1|ID), 
          data = subset, family = bernoulli(link="logit"))

bm_prior <- c(prior(normal(0,1), class = b),
              prior(exponential(1), class = sd))

bm_prior2 <- c(prior(normal(0,1), class = b, coef = seasonFspring),
               prior(normal(1,1), class = b , coef = seasonFsummer),
              prior(exponential(1), class = sd))

bm_prior1 <- c(prior(normal(0,1), class = b, coef = seasonFspring),
               prior(normal(0,1), class = b, coef = seasonFwinter),
               prior(exponential(1), class = sd))

# run prior predictive simulation
Bayes_Model_Binary_prior <- brm(formula = success ~ seasonF + (1|ID), 
                          data = subset, family = bernoulli(link="logit"), 
                          warmup = 500, iter = 20000, chains = 4, thin = 10, 
                          cores = 4, prior = bm_prior2, sample_prior = "only")

plot(Bayes_Model_Binary_prior)
summary(Bayes_Model_Binary_prior)

## get posterior predictions
Bayes_Model_Binary <- brm(formula = success ~ seasonF + (1|ID), 
                          data = subset, family = bernoulli(link="logit"), 
                          warmup = 500, iter = 20000, chains = 4, thin = 10, 
                          cores = 4, #backend = "cmdstanr", 
                          prior = bm_prior2)

prior_summary(Bayes_Model_Binary)
plot(Bayes_Model_Binary)
mcmc_plot(Bayes_Model_Binary,
          type = "trace")
mcmc_plot(Bayes_Model_Binary, type = "hist") #show histograms of the posterior distributions
mcmc_plot(Bayes_Model_Binary) #plot posterior intervals
mcmc_plot(Bayes_Model_Binary,
          type = "acf_bar")

summary(Bayes_Model_Binary)
#it cannot estimate the variance from ID, probably there's no effect of ID on predicting success rate
mcmc_plot(Bayes_Model_Binary,
          type = "areas",
          prob = 0.95)
#No significative effect

#######################
###new changes, 4/11###
#######################
get_prior(success ~ seasonF*test_ID + (1+test_ID|ID), 
          #(1+test_ID|ID): individual have different trajectories, but same trajectories between seasons
          data = od_data, family = bernoulli(link="logit"))

bm_prior_new <- c(prior(normal(0,1), class = b, coef = seasonFsummer),
                  prior(normal(0,1), class = b, coef = seasonFspring),
                  prior(normal(0,1), class = b, coef = test_ID.C),
                  prior(normal(0,1), class = b, coef = test_ID.L),
                  prior(normal(0,1), class = b, coef = test_ID.Q),
                  prior(normal(0,1), class = b, coef = seasonFspring:test_ID.C),
                  prior(normal(0,1), class = b, coef = seasonFspring:test_ID.Q),
                  prior(normal(0,1), class = b, coef = seasonFspring:test_ID.L),
                  prior(normal(0,1), class = b, coef = seasonFsummer:test_ID.C),
                  prior(normal(0,1), class = b, coef = seasonFsummer:test_ID.L),
                  prior(normal(0,1), class = b, coef = seasonFsummer:test_ID.Q),
                  prior(exponential(1), class = sd))
prior1 <- get_prior(success ~ seasonF*test_ID + (1+test_ID|ID), 
                   data = od_data, family = bernoulli(link="logit"))
make_stancode(success ~ seasonF*test_ID + (1+test_ID|ID), 
              data = od_data, family = bernoulli(link="logit"), prior = bm_prior_new)

Bayes_Model_Binary_new <- brm(formula = success ~ seasonF*test_ID + (1+seasonF*test_ID|ID), 
                              data = od_data, family = bernoulli(link="logit"), 
                              warmup = 500, iter = 2000, chains = 4, #thin = 10, 
                              cores = 4, 
                              prior = bm_prior_new)
#try fix prior with the ones we modified (brm_prior_new) DONE

Bayes_Model_Binary_new
#seasonFsummer               0.74      0.36     0.05     1.47 1.00     7253     4239
#upper and lower levels are on one side of the 0, so positive effect of season to success

#Test_ID.L ID.Q ID.C are of winter (intercept)
#everything else underneath are the adjustments to the winter effects (e.g. seasonFspring:testid 1.70 = -0.93+1.70)

#(1+test_ID|ID) individual have different trajectories, but same trajectoriesbetween seasons
#(1+seasonF*test_ID|ID) individuals have different trajectories, and i expect different trajectories per ID per season.

#make plot with average trend and each shrew trend per season

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
theme_set(theme_tidybayes() + panel_border())

get_variables(Bayes_Model_Binary_new)

Bayes_Model_Binary_new %>% 
  spread_draws(r_ID[20210802-1,Intercept]) %>% 
  head(10)


# hypothesis testing 
h1 <- hypothesis(Bayes_Model_Binary_new, c("seasonFsummer > 0", "seasonFsummer <0" ))
print(h1,digits = 3)
cond_eff <- conditional_effects(Bayes_Model_Binary_new)

  
df <- as.data.frame(cond_eff$seasonF)
cond_plot <- ggplot(df, aes(x=seasonF, y = estimate__)) +
  scale_color_manual(values = c("blue", "green", "red")) +
  geom_point(aes(color=seasonF), size = 4) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), color = "darkgrey") +
  labs(y = "Success Rate", x = "Season") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__)) +
  theme(legend.position="none")

cond_plot
df$upper__
df$lower__

Season <- c("2", "3", "1")
new<- df %>%
  mutate(seasonF, levels = Season) %>%
  arrange(Season)
new$seasonF <- relevel(new$seasonF, ref = "summer")
cond_2 <- ggplot(new, aes(x=seasonF, y = estimate__)) +
  scale_color_manual(values = c("red", "blue", "green")) +
  geom_point(aes(color=seasonF), size = 4) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), color = "darkgrey") +
  labs(y = "Success Rate", x = "Season") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__)) +
  theme(legend.position="none")
cond_2

Prob <- predict(Bayes_Model_Binary, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(subset, success)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#With an AUC score of close to 0.60, the model does not discriminate well.


subset$test_ID <- as.ordered(subset$test_ID)
subset$success <- as.numeric(subset$success)
subset$season <- as.factor(subset$season)


plot2 <- ggplot(od_data, aes(x = as.numeric(test_ID), y = success, color = season)) +
  geom_jitter(width =  0.05, height = 0.05) + 
  geom_smooth(mapping = aes(x = as.numeric(test_ID), y = success), ) + 
  labs(x = "Trial", y = "Success rate") +
  labs(col="Season") +
  scale_color_manual(values = c("green", "#D16103","#4E84C4")) 
plot2


df <- subset %>%  group_by(test_ID,season) %>%  summarise_at(vars(success), list(mean_success= mean,sd_success=sd))
df$se_success <-df$sd_success/sqrt(3)  #CONTROLLA SE HO CALCOLATO L'ERRORE STANDARD CORRETTAMENTE
plot3 <- ggplot(df, aes(x = test_ID, y = mean_success, color = season)) +
  geom_point(position = position_dodge(width = .6))+geom_errorbar(aes(ymin=mean_success-se_success, ymax=mean_success+se_success), width=.2,position = position_dodge(width = .6))
plot3


subset1 <- od_data %>%
  summarise(success = sum(success),
            total = n())
subset1

Bayes_Model_Prop <- brm(success | trials(250) ~ season,
                        data = subset,
                        family = binomial(link = "logit"),
                        warmup = 500,
                        iter = 2000,
                        chains = 4,
                        #thin = 10,
                        cores = 4)

mcmc_plot(Bayes_Model_Prop,
          type = "trace")
mcmc_plot(Bayes_Model_Prop,
          type = "acf_bar")
summary(Bayes_Model_Prop)
fixef(Bayes_Model_Prop)


#are there any other parameters that can explain the data?
library(lme4)
library(lmerTest)

subset2 <- select(od_data, ID, seasonF, test_ID, success, latency, test_time, c.of.d.)
subset2$test_ID <- factor(subset2$test_ID, ordered = TRUE)

str(subset2)
model1 <- lmer(formula = success ~ c.of.d. + scale(latency) + 
                 scale(test_time) + seasonF + (1|ID), data = subset2)
summary(model1)
#success is higher in summer? Higher than the Intercept (spring, latency zero, test time zero)

model2 <- lmer(formula = success ~ c.of.d.*seasonF + (1|ID), data = subset2)
summary(model2)

model3 <- lmer(formula = success ~ c.of.d.*test_ID  + (1|ID), data = subset2)
summary(model3)
#i want to see if change of directions (c.of.d.) increase/decrease 
#with the increasing/decreasing of the test_ID number, 
model4 <- lmer(formula = c.of.d. ~ test_ID  + (1|ID), data = subset2)
summary(model4)


### add ordered test number
subset$test_ID <- factor(subset$test_ID, ordered = TRUE)

Bayes_Model_Binary_2 <- brm(formula = success ~  (1|ID) + test_ID*seasonF, 
                          data = subset, family = bernoulli(link="logit"), 
                          warmup = 500, iter = 20000, chains = 4, thin = 10, cores = 4,
                          prior = bm_prior2)

mcmc_plot(Bayes_Model_Binary_2,
          type = "trace")
mcmc_plot(Bayes_Model_Binary_2, type = "hist") #show histograms of the posterior distributions
mcmc_plot(Bayes_Model_Binary_2) #plot posterior intervals
mcmc_plot(Bayes_Model_Binary_2,
          type = "acf_bar")
plot(Bayes_Model_Binary_2)
summary(Bayes_Model_Binary_2)

mcmc_plot(Bayes_Model_Binary_2,
          type = "areas",
          prob = 0.95)

# hypothesis testing 
h2 <- hypothesis(Bayes_Model_Binary_2, c("seasonFsummer > 0", "seasonFsummer <0" ))
print(h2,digits = 3)

plot(conditional_effects(Bayes_Model_Binary_2))

# complete model
Bayes_Model_Binary_3 <- brm(formula = success ~  (1|ID) + test_ID*seasonF 
                            + latency + test_time + c.of.d., data = od_data, 
                            family = bernoulli(link="logit"), warmup = 500, 
                            iter = 20000, chains = 4, thin = 10, cores = 4, 
                            prior = bm_prior2)

summary(Bayes_Model_Binary_3)

od_data$latency <- as.numeric(od_data$latency)
mcmc_plot(Bayes_Model_Binary_3,
          type = "areas",
          prob = 0.95)
mcmc_plot(Bayes_Model_Binary_3, variable = c("b_c.of.d."),
          type = "areas",
          prob = 0.95)
plot(conditional_effects(Bayes_Model_Binary_3))

?make_conditions
condition <- make_conditions(Bayes_Model_Binary_3, "c.of.d.")

cond <- conditional_effects(Bayes_Model_Binary_3, "c.of.d.", conditions = condition)
plot(cond)

cond2 <- conditional_effects(Bayes_Model_Binary_3, "latency", conditions = condition)
plot(cond2)

h3 <- hypothesis(Bayes_Model_Binary_3, c("c.of.d.>0", "c.of.d.<0" ))
print(h3,digits = 3)
h4 <- hypothesis(Bayes_Model_Binary_3, c("test_time>0", "test_time<0"))
print(h4, digits = 4)
Bayes_Model_Binary_3$data

full_dir_bm <- conditional_effects(Bayes_Model_Binary_3, effects = "c.of.d.", prob = 0.95)
full_season_bm <- conditional_effects(Bayes_Model_Binary_3, effects = "seasonF", prob = 0.95)
season_bm <- conditional_effects(Bayes_Model_Binary_2, effects = "seasonF", prob = 0.95)
print(season_bm)

