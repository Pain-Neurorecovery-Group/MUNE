pacman::p_load(tidyverse, ggplot2, cmdstanr, here, bayesplot, posterior)

# Set parameters
nStim = 100
mu = 10
mu_sigma = 1
t_sigma_mu = 0.4        
t_sigma_sigma = 0.4
N = 5
m_amp_log = -3
sd_amp_log = 0.3
noise_mean_log = -3
noise_sd_log = 0.1

# Load dataset and plot data
source(here("Simulations", "generative_model.R"))

ggplot(Y, aes(x = S, y = CMAP))+geom_point()+theme_classic()
ggplot(p, aes(x=S, y=p))+geom_point()+theme_classic()+facet_wrap(~id)
ggplot(p, aes(x=S, y = dnorm(S,t_mu,t_sigma), color = factor(id), group = id))+geom_line()+theme_classic()

# Select model
name = "poisson_binomial1.stan"

#initiating fitting
mod = cmdstan_model(here("models", name))

data_stan = list(N =nrow(Y), U=5, CMAP = Y$CMAP, S = Y$S)

fit = mod$sample(data = data_stan, chains = 4, 
                 parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)

# Investigate fit
mcmc_trace(fit$draws(), pars = c("t_mu_mu","t_mu_sigma_log","t_sigma_mu_log","t_sigma_sigma_log_log",
  "amp_mu","amp_sigma","noise_mean","noise_sigma"))

mcmc_pairs(fit$draws(),pars = c("t_mu_mu","t_mu_sigma_log","t_sigma_mu_log","t_sigma_sigma_log_log"))
mcmc_pairs(fit$draws(),pars = c("amp_mu", "amp_sigma","noise_mean", "noise_sigma"))

