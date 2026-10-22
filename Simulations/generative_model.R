pacman::p_load(tidyverse, ggplot2, truncnorm, patchwork, Polychrome)

# Stimuli administered 
nStim = 500
S = seq(10,50, length.out = nStim)

# Parameters
mu = 30
mu_sigma = 4
t_sigma_mu = 0.5
t_sigma_sigma = 0.4
N = 20
m_amp_log = -2
sd_amp_log = 0.3
#noise_mean_log = -3
#noise_sd_log = 0.1

t_mu = sort(rnorm(N, mu, mu_sigma))
#t_mu = c(15,22,30,32,40)
t_sigma = exp(rnorm(N, t_sigma_mu, t_sigma_sigma))
#t_sigma =c(1, 2.5, 0.8, 1.2, 0.5)
amp = exp(rnorm(N, m_amp_log, sd_amp_log))

Units = expand.grid(unit = seq_len(N),Stim = S) %>% mutate(t_mu = t_mu[unit], amp = amp[unit], t_sigma = t_sigma[unit]) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>% 
  mutate(fire = rbinom(n(),1,pp)) %>% mutate(con = amp * fire)

outcome = Units %>% group_by(Stim) %>% summarise(CMAP = sum(con))

# Plot outcome
set.seed(223)
unit_cols <- createPalette(
  N,
  c("#536878", "#C17C74", "#8A9A5B", "#9B89B3",
    "#D4A259", "#6F9E9A", "#B8758B", "#8C8175"),
  range = c(20, 80)
)

names(unit_cols) <- as.character(seq_len(N))
CMAP = ggplot(outcome)+geom_point(aes(x = Stim, y = CMAP), size = 1.2)+scale_x_continuous(limits = c(10, 50), breaks = seq(10, 50, 10))+
  theme_light(base_size = 14)+labs(x="")

prob = ggplot(Units)+geom_line(aes(x=Stim, y=pp, col = as.factor(unit)), linewidth =1)+scale_x_continuous(limits = c(10, 50), breaks = seq(10, 50, 10))+
                          theme_classic(base_size =14)+theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+labs(y="P(fire)", x = "Stimulus (mA)")


CMAP/prob + plot_layout(heights = c(2, 1))

#plot amp
amp_distr = data.frame(MUAP = exp(rnorm(100000,-2,0.3)))
density = ggplot()+geom_density(data = amp_distr,aes(x=MUAP),linewidth =1, fill = 'grey70', alpha =0.5, col = "grey70")+theme_classic(base_size =14)+
  geom_point(data = distinct(Units, unit, amp),aes(x = amp,y =0, col = as.factor(unit)),size =3)+
  theme(legend.position = "none")+labs(x= "Unit-sepecific MUAP", y = "density")+scale_x_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.15))

amp = Units %>% group_by(unit, Stim) %>% mutate(E = amp*pp) %>% group_by(Stim) %>% summarise(E_CMAP = sum(E))
amp_units = Units %>%distinct(unit, Stim, amp, pp) %>%mutate(E = amp * pp)

contribution = ggplot() + geom_area(data = amp_units, aes(x = Stim, y = E, fill = factor(unit)),position = "stack",alpha =0.5, color = "white",linewidth = 0.15) + 
  geom_line(data = amp, aes(x=Stim, y=E_CMAP), linewidth = 1.5)+theme_classic(base_size =14)+theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x="Stimulus (mA)", y = "E[CMAP]", title = "Expected CMAP Contribution by Motor Unit (MUAP)")

contribution/density + plot_layout(heights = c(2, 1))