pacman::p_load(cmdstanr,ggplot2, poisbinom, here, posterior, tidyverse)

# Generate data
N=200
Stim = seq(0, 40, length.out = N)
U=4

t_mu_mu = 20
t_mu_sigma = 7
t_sigma = exp(rnorm(1, 0.6,0.2))

t_mu = sort(rnorm(U, t_mu_mu, t_mu_sigma))

Units =  expand.grid(unit = seq_len(U),Stim = Stim) %>%mutate(t_mu = t_mu[unit]) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) 


outcome = Units %>% group_by(Stim) %>% summarise(Y = rpoisbinom(1, pp))

# Calculate log likelihood
#k = 0:U

#r_prob_log = dpoisbinom(k,pp, log = TRUE)

# Fit model
data_stan = list(N=N, U = U, S = Stim, Y = outcome$Y)

mod = cmdstan_model(here("models", "poisson_binomial_test2.stan")) 

fit = mod$sample(data = data_stan, iter_warmup = 1000, iter_sampling = 1000, chains = 4, parallel_chains = 4, adapt_delta = 0.95)

# Investigate fit

# Posterior update
t_sigma_draws = tibble(posterior = as_draws_df(fit$draws("t_sigma"))$t_sigma, prior = exp(rnorm(4000,0.6,0.2)), sim =t_sigma)

ggplot(t_sigma_draws) + geom_density(aes(x = prior), fill = "green")+geom_density(aes(x=posterior), fill = "red", alpha =0.7)+
  geom_vline(aes(xintercept = sim), linewidth = 1)+theme_classic()+labs(x="parameter value", title = "Prior-Posterior update compared to simulated value")+
  theme(plot.title = element_text(hjust = 0.5))

t_draws = bind_rows(lapply(1:U, function(i) {
  tibble(parameter = paste0("t_mu[", i, "]"),posterior = as_draws_df(fit$draws("t_mu"))[[paste0("t_mu[", i, "]")]],
         prior = rnorm(4000, 9, 2),sim = t_mu[i])}))

ggplot(t_draws) + geom_density(aes(x = prior), fill = "green")+geom_density(aes(x=posterior), fill = "red", alpha =0.7)+
  geom_vline(aes(xintercept = sim), linewidth = 1)+theme_classic()+facet_wrap(~parameter, scales  ="free_x")+labs(x="parameter value", title = "Prior-Posterior update compared to simulated value")+theme(plot.title = element_text(hjust = 0.5))

# Compare simulated trace to simulated
p_draws= as_draws_df(fit$draws("pp")) %>% select(!contains(".")) %>% 
  pivot_longer(cols = everything(),names_to = "p",values_to = "posterior") %>% group_by(p) %>% 
  summarise(mean_p = mean(posterior), hdi = list(hdi(posterior, ci = 0.95)),.groups = "drop") %>% 
  mutate(lower = map_dbl(hdi, ~ .x$CI_low),upper = map_dbl(hdi, ~ .x$CI_high)) %>%select(-hdi)%>% 
  mutate(Stim = Stim[as.integer(str_extract(p, "(?<=\\[)\\d+"))],unit = as.integer(str_extract(p, "(?<=,)\\d+(?=\\])"))) %>% select(!p) %>% 
  left_join(Units, by = c("unit", "Stim"))

ggplot(p_draws)+geom_line(aes(x=Stim, y = pp), linewidth = 2, color = "green")+geom_line(aes(x=Stim, y = mean_p),linewidth = 2, color = "red", alpha = 0.5)+
  theme_classic()+  geom_ribbon(aes(x = Stim, ymin = lower, ymax = upper),fill = "red",alpha = 0.2) +facet_wrap(~unit)+
  labs(title = "Comparison simulated & estimated probabilistic response curves", y = "P(fire)",x="Stimulus (mV)")+
  theme(plot.title = element_text(hjust = 0.5))


