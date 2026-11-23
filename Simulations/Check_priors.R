pacman::p_load(ggplot2, here, poisbinom)
# test t_mu
t_mu_prior = function(N){
  t_mu_mu = rnorm(N, 12,0.5)
  t_mu_sigma = exp(rnorm(N, 0,0.2))
  
  t_mu = c()
  for (i in 1:N){
  t = rnorm(1000, mean = t_mu_mu[i], sd = t_mu_sigma[i])
  t_mu = c(t_mu, t)}
  
  return(t_mu)
}

t_sigma_prior = function(N){
  t_sigma_mu_log = rnorm(N,-0.3,0.5)
  t_sigma_sigma = exp(rnorm(N, -1, 0.3))
  
  t_sigma = c()
  for (i in 1:N){
    t = exp(rnorm(1000, mean = t_sigma_mu_log[i], sd = t_sigma_sigma[i]))
    t_sigma = c(t_sigma, t)}
  
  return(t_sigma)
}

t_mu = t_mu_prior(100)
t_df = data.frame(t_mu = t_mu)
ggplot(t_df)+geom_histogram(aes(x=t_mu), stat = "bin", col = "black", fill = "grey", bins = 30)+theme_classic()


t_sigma = t_sigma_prior(100)
t_df = data.frame(t_sigma = t_sigma)
ggplot(t_df)+geom_histogram(aes(x=t_sigma), stat = "bin", col = "black", fill = "grey", bins = 50)+theme_classic()
# current data generating function
N = 200
U = 300
Stim = seq(0, 25, length.out = N)

t_mu_mu = rnorm(1, 12,0.5)
t_mu_sigma = exp(rnorm(1, 0,0.2))

t_sigma_mu_log = rnorm(1,-0.3,0.5)
t_sigma_sigma = exp(rnorm(1, -1, 0.3))

t_sigma = exp(rnorm(U, t_sigma_mu_log, t_sigma_sigma))

t_mu = sort(rnorm(U, t_mu_mu, t_mu_sigma))

Units =  expand.grid(unit = seq_len(U),Stim = Stim) %>%mutate(t_mu = t_mu[unit], t_sigma = t_sigma[unit]) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>% 
  mutate(t_sigma_mu_log = t_sigma_mu_log, t_mu_mu = t_mu_mu, t_mu_sigma = t_mu_sigma, U = U, t_sigma_sigma=t_sigma_sigma) %>% 
  group_by(Stim) %>% mutate(spacing = t_mu - lag(t_mu)) %>% ungroup()

outcome = Units %>% group_by(Stim) %>% summarise(Y = rpoisbinom(1, pp))

ggplot(Units)+geom_line(aes(x=Stim, y =pp, group = as.factor(unit)), alpha = 0.2)+theme_classic()+scale_x_continuous(limits=c(5,17))

# fit attempt
data_stan = list(U=U, Y = outcome$Y, S= Stim, N=N)

mod = cmdstan_model(here("models", "MU_t_sigma.stan"))

fit = mod$sample(data = data_stan, iter_warmup = 1000, iter_sampling = 1000, 
               chains = 4, parallel_chains = 4, adapt_delta = 0.99)