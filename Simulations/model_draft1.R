pacman::p_load(tidyverse, ggplot2, poisbinom)

# Stimuli administered 
nStim = 450 
S = exp(seq(log(15),log(5), length.out = nStim))

# Parameters
N_values = c(20, 50, 100)            # Poisson distributed (extra noise necessary?) 
t_SD_values = c(0.01, 0.2, 1.2)          # set constant for simplicity
t_m_values = c(8, 11, 13)
t_SD_pop_values = c(0.2, 0.6, 1)
amp_values = c(0.02, 0.05, 0.2)

# Middle/baseline values
N0 = 50
t_SD0 = 0.6
t_m0 = 11
t_SD_pop0 = 0.01
amp0 = 0.05

parameter_grid = bind_rows(tibble(parameter = "N",N = N_values,t_SD = t_SD0,t_m = t_m0,t_SD_pop = t_SD_pop0,amp = amp0),
    tibble(parameter = "t_SD",N = N0,t_SD = t_SD_values,t_m = t_m0,t_SD_pop = t_SD_pop0,amp = amp0),
    tibble(parameter = "t_m",N = N0,t_SD = t_SD0,t_m = t_m_values,t_SD_pop = t_SD_pop0,amp = amp0),
    tibble(parameter = "t_SD_pop", N = N0, t_SD = t_SD0, t_m = t_m0,t_SD_pop = t_SD_pop_values,amp = amp0),
    tibble(parameter = "amp",N = N0,t_SD = t_SD0,t_m = t_m0,t_SD_pop = t_SD_pop0,amp = amp_values))
# m_amp_log = -3
# sd_amp_log = 0.3

#rlnorm(N, m_amp_log, sd_amp_log)

p = expand.grid(S = S, id = 1:N0) %>% mutate(id = row_number(),by = "id", amp = amp0) %>%
  mutate(p = pnorm((S - t_m0)/sqrt(t_SD0^2 + t_SD_pop0^2)),fire = rbinom(n(), 1, p))  # Not sure about the integrating over 2 normals, ASK or derive!!!


# Outcome
Y = p %>% group_by(S) %>% summarise(N_S = sum(fire), CMAP = sum(fire*amp))  

# plot outcome
ggplot(Y, aes(x = S, y = CMAP))+geom_point()+theme_classic()

# Simulate for multiple parameter values
simulations = parameter_grid %>%mutate(condition = row_number()) %>%group_by(condition) %>%
  group_modify(~ {pars = .x
  p = expand_grid(S = S,id = 1:pars$N) %>%mutate(p = pnorm((S - pars$t_m) /sqrt(pars$t_SD^2 + pars$t_SD_pop^2)),
                                                 fire = rbinom(n(), 1, p),amp = pars$amp)
    
    p %>% group_by(S) %>%summarise(N_S = sum(fire),CMAP = sum(fire * amp),.groups = "drop") %>%
      mutate(parameter = pars$parameter,N = pars$N,t_SD = pars$t_SD,t_m = pars$t_m,t_SD_pop = pars$t_SD_pop,amp = pars$amp)}) %>%
  ungroup() 

#plot different combinations
ggplot(simulations %>% filter(parameter == "N"),aes(x = S, y = CMAP, group = condition, color = as.factor(N))) +
  geom_point() +facet_wrap(~ parameter, scales = "free") +
  theme_classic()

ggplot(simulations %>% filter(parameter == "t_m"),aes(x = S, y = CMAP, group = condition, color = as.factor(t_m))) +
  geom_point() +facet_wrap(~ parameter, scales = "free") +
  theme_classic()

ggplot(simulations %>% filter(parameter == "amp"),aes(x = S, y = CMAP, group = condition, color = as.factor(amp))) +
  geom_point() +facet_wrap(~ parameter, scales = "free") +
  theme_classic()

ggplot(simulations %>% filter(parameter == "t_SD"),aes(x = S, y = CMAP)) +
  geom_point() +facet_wrap(~ t_SD) +
  theme_classic() 

ggplot(simulations %>% filter(parameter == "t_SD_pop"),aes(x = S, y = CMAP)) +
  geom_point() +facet_wrap(~ t_SD_pop) +
  theme_classic() 









