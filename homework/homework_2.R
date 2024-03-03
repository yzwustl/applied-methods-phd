library(tidyr)
library(dplyr)
library(fixest)
library(ggplot2)

nsw <- read.csv("D:/Dropbox/Finance PhD/applied-methods-phd/homework/data/lalonde_nsw.csv")
psid <- read.csv("D:/Dropbox/Finance PhD/applied-methods-phd/homework/data/lalonde_psid.csv")

nsw_treated <- nsw[nsw$treat == 1,]

# a: Construct propensity score using logit
data <- rbind(nsw_treated, psid)
logit_model <- feglm(treat ~ age + education + black + hispanic + married + nodegree + re74 + re75, data = data, family = binomial)
data$pscore_logit <- predict(logit_model, newdata = data, type = "response")

data %>% group_by(treat) %>% summarize(mean_pscore = mean(pscore_logit))

ggplot() + 
  geom_density(data = data, aes(x = pscore_logit, fill = factor(treat)), alpha = 0.5) + theme_minimal()

ggplot(data %>% filter(pscore_logit > 0.1 & pscore_logit < 0.9)) +
  geom_density(aes(x = pscore_logit, color = factor(treat), group = factor(treat))) +
  labs(x = "Propensity Score", y = "Density")

#b: EY1 AND EY0 BY IPW AND SIPW
ipw_estimate <- function(outcome,treatment,pscore){
  control_mean <- mean(outcome * (1 - treatment) / (1 - pscore)) 
  treatment_mean <- mean(outcome * treatment / pscore)
  ate            <- treatment_mean - control_mean
}

sipw_estimate <- function(outcome,treatment,pscore){
  control_mean <- (mean(outcome * (1 - treatment) / (1 - pscore)) / mean((1 - treatment) / (1 - pscore)))
  treatment_mean <- (mean(outcome * treatment / pscore) / mean(treatment / pscore))
  ate            <- treatment_mean - control_mean
}

ipw_ate  =  ipw_estimate(data$re78,data$treat,data$pscore_logit)
sipw_ate =  sipw_estimate(data$re78,data$treat,data$pscore_logit)

#c: 
linear_model_outcome <- feols(re78 ~ treat + age + education + hispanic + black + married + nodegree + re74 + re75, data = data)
summary(linear_model_outcome)

#d:
data_trimmed <- data %>% filter(pscore_logit > 0.1 & pscore_logit < 0.9)

ipw_ate_trimmed <- ipw_estimate(data_trimmed$re78,data_trimmed$treat,data_trimmed$pscore_logit)
sipw_ate_trimmed <- sipw_estimate(data_trimmed$re78,data_trimmed$treat,data_trimmed$pscore_logit)

#e:
data_trimmed_black <- data %>% filter(pscore_logit > 0.1 & pscore_logit < 0.9 & black == 1)
data_trimmed_nonblack <- data %>% filter(pscore_logit > 0.1 & pscore_logit < 0.9 & black == 0)

ipw_ate_trimmed_black <- ipw_estimate(data_trimmed_black$re78,data_trimmed_black$treat,data_trimmed_black$pscore_logit)
sipw_ate_trimmed_black <- sipw_estimate(data_trimmed_black$re78,data_trimmed_black$treat,data_trimmed_black$pscore_logit)

ipw_ate_trimmed_nonblack <- ipw_estimate(data_trimmed_nonblack$re78,data_trimmed_nonblack$treat,data_trimmed_nonblack$pscore_logit)
sipw_ate_trimmed_nonblack <- sipw_estimate(data_trimmed_nonblack$re78,data_trimmed_nonblack$treat,data_trimmed_nonblack$pscore_logit)