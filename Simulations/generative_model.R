pacman::p_load(tidyverse, ggplot2, truncnorm, patchwork, purrr)

# Stimuli administered 
nStim = 300
S = seq(10,50, length.out = nStim)

# Parameters

# mu = 30
mu =c(17,35)
# mu_sigma =3
mu_sigma = c(0.5, 4)
mu_params = expand.grid(mu =mu, mu_sigma = mu_sigma)
t_sigma_mu =0.3
# t_sigma_mu = c(-0.5,1.2)
# t_sigma_sigma = c(0.5,1.2)
t_sigma_sigma = 0.2
# t_sigma_params = expand.grid(t_sigma_mu = t_sigma_mu, t_sigma_sigma =t_sigma_sigma)
N = 50
# N = c(10,30,100)
m_amp_log = -2
# m_amp_log = c(-2,-0.5)
sd_amp_log = 0.5
# sd_amp_log = c(0.001,0.8)
# amp_params = expand.grid(m_amp_log = m_amp_log, sd_amp_log =sd_amp_log)
noise_mean = 0
noise_sd = 0.1
t_mu = sort(rnorm(N, mu, mu_sigma))

t_sigma = exp(rnorm(N, t_sigma_mu, t_sigma_sigma))
# t_sigma =c(1, 2.5, 0.8, 1.2, 0.5)
amp = exp(rnorm(N, m_amp_log, sd_amp_log))
noise = rnorm(nStim, noise_mean, noise_sd) 

params = crossing(mu_params, unit = seq_len(N)) %>% group_by(mu,mu_sigma) %>% mutate(t_mu = sort(rnorm(N, mu, mu_sigma))) %>% 
  mutate(t_sigma = t_sigma[unit], amp = amp[unit]) %>% ungroup() %>% crossing(Stim = S) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>%
  mutate(fire = rbinom(n(),1,pp)) %>% mutate(con = amp * fire) %>% mutate(E_amp = pp*amp)

#params = crossing(t_sigma_params, unit = seq_len(N)) %>% group_by(t_sigma_mu,t_sigma_sigma) %>% mutate(t_sigma = exp(rnorm(N, t_sigma_mu, t_sigma_sigma))) %>% 
  #mutate(t_mu = t_mu[unit], amp = amp[unit]) %>% ungroup() %>% crossing(Stim = S) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>%
  #mutate(fire = rbinom(n(),1,pp)) %>% mutate(con = amp * fire) %>% mutate(E_amp = pp*amp)

#params = map_dfr(N, ~tibble(N=.x, unit = 1:.x)) %>% group_by(N) %>% mutate(t_mu = rnorm(n(),mu,mu_sigma)) %>% mutate(t_sigma = exp(rnorm(n(), t_sigma_mu, t_sigma_sigma))) %>% 
  #mutate(amp = exp(rnorm(n(), m_amp_log,sd_amp_log))) %>% ungroup() %>% crossing(Stim = S) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>% mutate(fire = rbinom(n(),1,pp)) %>% 
  #mutate(con = amp * fire) %>% mutate(E_amp = pp*amp)

#params = crossing(amp_params, unit = seq_len(N)) %>% group_by(m_amp_log,sd_amp_log) %>% mutate(amp = exp(rnorm(N, m_amp_log, sd_amp_log))) %>% 
  #mutate(t_mu = t_mu[unit], t_sigma = t_sigma[unit]) %>% ungroup() %>% crossing(Stim = S) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>%
 #mutate(fire = rbinom(n(),1,pp)) %>% mutate(con = amp * fire) %>% mutate(E_amp = pp*amp)
  
outcome = params %>% group_by(mu, mu_sigma,Stim) %>% summarise(true_CMAP = sum(con), E_CMAP = sum(E_amp)) %>% mutate(noise = noise[match(Stim, S)]) %>% mutate(CMAP = pmax(0,true_CMAP+ noise)) 

Units = expand.grid(unit = seq_len(N),Stim = S) %>% mutate(t_mu = t_mu[unit], amp = amp[unit], t_sigma = t_sigma[unit]) %>% mutate(pp = pnorm((Stim-t_mu)/t_sigma)) %>% 
  mutate(fire = rbinom(n(),1,pp)) %>% mutate(con = amp * fire) %>% mutate(E_amp = pp*amp)

outcome = Units %>% group_by(Stim) %>% summarise(CMAP_t = sum(con),E_CMAP = sum(E_amp)) %>%  mutate(noise_low = rnorm(n(), noise_mean, noise_sd[1]), noise_high = rnorm(n(), noise_mean, noise_sd[2])) %>% 
  mutate(`Low noise`= pmax(0,CMAP_t+ noise_low), `High noise` =pmax(0,CMAP_t + noise_high)) %>% pivot_longer(
    cols = c(`Low noise`, `High noise`),
    names_to = "Noise",
    values_to = "CMAP"
  ) 
# Plot outcome


CMAP = ggplot(outcome)+geom_ribbon(aes(x = Stim, ymin =lowest, ymax = highest, fill = "Higher noise"), alpha = 0.5)+geom_ribbon(aes(x = Stim, ymin =low, ymax = high, fill = "Lower noise"), alpha = 0.75)+ 
  geom_point(aes(x = Stim, y = CMAP_t), size = 2)+scale_x_continuous(limits = c(10, 50), breaks = seq(10, 50, 10))+
  theme_light(base_size = 22)+theme(legend.position = "none")+
  scale_fill_manual(name = "Measuring noise", values = c("Lower noise" = "#397D83","Higher noise" = "#A9C7C9"))+
  labs(x = "Stimulus (mA)", y = "CMAP")
  

noise_df <- tibble(value = c(rnorm(10000, 0, 0.2),rnorm(10000, 0, 1)),
  sigma_b = factor(rep(c("Low", "High"), each = 10000),levels = c("Low", "High")))

noise = ggplot(noise_df, aes(x = value, fill = sigma_b, color = sigma_b)) +
  geom_density(alpha = 0.3, linewidth = 1.5) +
  scale_fill_manual(
    values = c("#397D83", "#A9C7C9"),
    name =  bquote("Noise (" * sigma[b] * ")")
  ) +
  scale_color_manual(
    values = c("#397D83", "#A9C7C9"),
    name =  bquote("Noise (" * sigma[b] * ")")
  ) +
  labs(
    x = expression(b[i]),
    y = "Density"
  ) +
  theme_classic(base_size = 22)+
  theme(legend.position = 'inside', legend.position.inside = c(0.2,0.7), legend.background = element_rect(colour = "black", linewidth = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_continuous( breaks = c(-3, 0, 3),labels = c("-3", "0 (True CMAP)", "3"))+labs(title = "Baseline noise distribution")

noise/CMAP
prob = ggplot(Units)+geom_line(aes(x=Stim, y=pp, col = as.factor(unit)), linewidth =1)+scale_x_continuous(limits = c(10, 50), breaks = seq(10, 50, 10))+
                          theme_classic(base_size =22)+theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+labs(y="P(fire)", x = "Stimulus (mA)")


CMAP/prob + plot_layout(heights = c(2, 1))
# Example 3 Units
mu = c(26,30,32)
t_sigma = c(2, 4, 1.5)

data_density <- data.frame(
  Stimulus = rep(S, times = 3),
  Density = c(dnorm(S, mean = mu[1], sd = t_sigma[1]),
              dnorm(S, mean = mu[2], sd = t_sigma[2]),
              dnorm(S, mean = mu[3], sd = t_sigma[3])),
  Probability = c(pnorm(S, mean = mu[1], sd = t_sigma[1]),
                  pnorm(S, mean = mu[2], sd = t_sigma[2]),
                  pnorm(S, mean = mu[3], sd = t_sigma[3])),
  Punt = rep(c("Punt_1", "Punt_2", "Punt_3"), each = nStim)
)

punten_data <- data.frame(
  Stimulus = c(30, 30, 30),
  Probability = pnorm(30, mean = mu, sd = t_sigma),
  Punt = as.factor(c("Punt_1", "Punt_2", "Punt_3"))
)

distr = ggplot(data_density)+geom_line(aes(x=Stimulus, y= Density,col = as.factor(Punt)))+theme_light(base_size = 22)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x= '', y = "Density", title = expression(paste(tau[u * "," * i]," distributions for 3 units"))) + 
  scale_x_continuous(limits = c(15, 45), breaks = seq(10, 50, 10))+
  geom_vline(xintercept = 30, linetype = "dashed", color = "black", alpha = 0.7)+
  annotate("text", x = 27.5,y = 0.23,label = "Stim", color = "black", size = 10, fontface = "italic", alpha = 0.7)+
  geom_area(data = filter(data_density, Stimulus<=30), aes(x = Stimulus, y = Density, fill = as.factor(Punt)),alpha = 0.3, position = "identity")+
  scale_color_brewer(palette = "Set1")+scale_fill_brewer(palette = "Set1")

prob = ggplot(data_density)+geom_line(aes(x=Stimulus, y= Probability,col = as.factor(Punt)))+theme_minimal(base_size = 22)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x= 'Stimulus(mA)', y = "P(fire)")+
  scale_x_continuous(limits = c(15, 45), breaks = seq(10, 50, 10))+
  geom_vline(xintercept = 30, linetype = "dashed", color = "black", alpha = 0.7)+
  scale_color_brewer(palette = "Set1")+
  geom_point(data = punten_data, aes(x=Stimulus, y = Probability, col = as.factor(Punt)), size = 4)+
  annotate("text", x = 39,y = 0.09,label = 'P(tau[3 * "," * i] < Stim)', parse = T, color = "#4DAF4A", size = 8, fontface = "italic", alpha = 0.7)+
  annotate("text", x = 39,y = 0.5,label = 'P(tau[2 * "," * i] < Stim)', parse = T, color = "#377EB8", size = 8, fontface = "italic", alpha = 0.7)+
  annotate("text", x = 22,y = 0.98,label = 'P(tau[1 * "," * i] < Stim)', parse = T, color = "#E41A1C", size = 8, fontface = "italic", alpha = 0.7)
  

distr/prob

#plot amp

amp_distr = data.frame(MUAP = exp(rnorm(100000,-2,0.3)))
density = ggplot()+geom_density(data = amp_distr,aes(x=MUAP),linewidth =1, fill = 'grey70', alpha =0.5, col = "grey70")+theme_classic(base_size =14)+
  geom_point(data = distinct(Units, unit, amp),aes(x = amp,y =0, col = as.factor(unit)),size =3)+
  theme(legend.position = "none")+labs(x= "Unit-sepecific MUAP", y = "density")+scale_x_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.15))

amp = Units %>% group_by(unit, Stim) %>% mutate(E = amp*pp) %>% group_by(Stim) %>% summarise(E_CMAP = sum(E))
amp_units = Units %>%distinct(unit, Stim, amp, pp) %>%mutate(E = amp * pp)

contribution = ggplot(amp_units) + geom_area(data = amp_units, aes(x = Stim, y = E, fill = factor(unit)),position = "stack",alpha =0.5, color = "white",linewidth = 0.15) + 
  geom_line(data = amp, aes(x=Stim, y=E_CMAP), linewidth = 1.5)+theme_classic(base_size =22)+theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x="Stimulus (mA)", y = "E[CMAP]", title = "Expected CMAP Contribution")+
  annotate("text", x = 22,y = 1,label =  expression(E * group("[", CMAP, "]") == sum(group("(", P(fire)[u * "," * i] %.% lambda[u], ")"))), parse = T, color = "black", size = 7, fontface = "italic", alpha = 0.8,  inherit.aes = FALSE)
  
amp = ggplot(Units)+geom_bar(aes(x=as.factor(unit), y = amp, fill = as.factor(unit)), stat = "summary", fun = "mean", col = "black", alpha =0.7)+
  theme_classic(base_size = 22)+theme(legend.position = 'none')+labs(x="Units (u)", y= expression(lambda[u]))

contribution/amp + plot_layout(heights = c(2, 1))

#plot comb of mu
p1 = ggplot(outcome)+geom_point(aes(x=Stim, y=CMAP, col = interaction(mu,mu_sigma)), size = 2, alpha =0.5)+
  theme_light(base_size = 22)+geom_line(aes(x=Stim, y=E_CMAP, col = interaction(mu,mu_sigma)), linewidth = 2)+
  labs(x="Stimulus (mA)")+theme(legend.position = 'none')
p2 = ggplot(params %>% filter(mu==17, mu_sigma ==0.5))+geom_line(aes(x=Stim, y=dnorm(Stim,t_mu,t_sigma), group = unit),col = "#F8766D", alpha =0.5 , linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#F8766D",fill = NA,linewidth = 4))+
  labs(x="", y = "density")+scale_y_continuous(limits = c(0,0.6), breaks = c(0,0.3,0.6))
p3 =ggplot(params %>% filter(mu==17, mu_sigma ==4))+geom_line(aes(x=Stim, y=dnorm(Stim,t_mu,t_sigma), group = unit),col = "#00BFC4", alpha =0.5, linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#00BFC4",fill = NA,linewidth = 4))+
  labs(x="", y = "")+scale_y_continuous(limits = c(0,0.6), breaks = c(0,0.3,0.6))
p4 =ggplot(params %>% filter(mu==35, mu_sigma ==0.5))+geom_line(aes(x=Stim, y=dnorm(Stim,t_mu,t_sigma), group = unit),col = "#7CAE00", alpha =0.5, linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#7CAE00",fill = NA,linewidth = 4))+
  labs(x="Stimulus (mA)", y = "density")+scale_y_continuous(limits = c(0,0.6), breaks = c(0,0.3,0.6))
p5 =ggplot(params %>% filter(mu==35, mu_sigma ==4))+geom_line(aes(x=Stim, y=dnorm(Stim,t_mu,t_sigma), group = unit), col = "#C77CFF", alpha =0.5, linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#C77CFF",fill = NA,linewidth = 4))+
  labs(x="Stimulus (mA)", y = "")+scale_y_continuous(limits = c(0,0.6), breaks = c(0,0.3,0.6))

p1 / (p2 | p3) / (p4 | p5) +
  plot_layout(heights = c(2, 1, 1))

#plot comb of t_sigma
p1 = ggplot(outcome %>% filter(t_sigma_mu==-0.5, t_sigma_sigma == 0.5))+geom_point(aes(x=Stim, y=CMAP),col = "#F8766D", size = 2, alpha =0.5)+
  theme_light(base_size = 22)+geom_line(aes(x=Stim, y=E_CMAP),col = "#F8766D", linewidth = 1.2)+
  labs(x="", y = "CMAP")+theme(legend.position = 'none')
p2 = ggplot(outcome %>% filter(t_sigma_mu==-0.5, t_sigma_sigma == 1.2))+geom_point(aes(x=Stim, y=CMAP),col = "#00BFC4", size = 2, alpha =0.5)+
  theme_light(base_size = 22)+geom_line(aes(x=Stim, y=E_CMAP),col = "#00BFC4", linewidth = 1.2)+
  labs(x="", y="")+theme(legend.position = 'none')
p3 = ggplot(outcome %>% filter(t_sigma_mu==1.2, t_sigma_sigma == 0.5))+geom_point(aes(x=Stim, y=CMAP),col = "#7CAE00", size = 2, alpha =0.5)+
  theme_light(base_size = 22)+geom_line(aes(x=Stim, y=E_CMAP),col = "#7CAE00", linewidth = 1.2)+
  labs(x="", y="CMAP")+theme(legend.position = 'none')
p4 = ggplot(outcome %>% filter(t_sigma_mu==1.2, t_sigma_sigma == 1.2))+geom_point(aes(x=Stim, y=CMAP),col = "#C77CFF", size = 2, alpha =0.5)+
  theme_light(base_size = 22)+geom_line(aes(x=Stim, y=E_CMAP),col = "#C77CFF", linewidth = 1.2)+
  labs(x="", y="")+theme(legend.position = 'none')

p5 = ggplot(params %>% filter(t_sigma_mu==-0.5, t_sigma_sigma == 0.5))+geom_line(aes(x=Stim, y=pp, group = unit),col = "#F8766D", alpha =0.5 , linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#F8766D",fill = NA,linewidth = 2))+
  labs(x="Stimulus (mA)", y = "P(fire)")+scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1))
p6 = ggplot(params %>% filter(t_sigma_mu==-0.5, t_sigma_sigma == 1.2))+geom_line(aes(x=Stim, y=pp, group = unit),col = "#00BFC4", alpha =0.5 , linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#00BFC4",fill = NA,linewidth = 2))+
  labs(x="Stimulus (mA)", y = "")+scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1))
p7 = ggplot(params %>% filter(t_sigma_mu==1.2, t_sigma_sigma == 0.5))+geom_line(aes(x=Stim, y=pp, group = unit),col = "#7CAE00", alpha =0.5 , linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#7CAE00",fill = NA,linewidth = 2))+
  labs(x="Stimulus (mA)", y = "p(fire)")+scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1))
p8 = ggplot(params %>% filter(t_sigma_mu==1.2, t_sigma_sigma == 1.2))+geom_line(aes(x=Stim, y=pp, group = unit),col = "#C77CFF", alpha =0.5 , linewidth = 1)+
  theme_classic(base_size = 22)+theme(legend.position='none', panel.border = element_rect(colour = "#C77CFF",fill = NA,linewidth = 2))+
  labs(x="Stimulus (mA)", y = "")+scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1))


((p1 / p5) + plot_layout(heights = c(2, 1)) |
    (p2 / p6) + plot_layout(heights = c(2, 1))) /
  ((p3 / p7) + plot_layout(heights = c(2, 1)) |
     (p4 / p8) + plot_layout(heights = c(2, 1)))

#plot N
ggplot(outcome)+geom_point(aes(x=Stim,y=CMAP,col=as.factor(N)), size = 2.2)+
  scale_color_discrete(name = "Motor Units",labels = c("N = 5", "N = 30", "N = 100"))+theme_light(base_size = 22)+
  theme(legend.position = 'inside', legend.position.inside = c(0.2,0.7), legend.background = element_rect(colour = "black", linewidth = 0.3))+
  labs(x="Stimulus (mA)")

#plot amp
ggplot(outcome %>% filter(m_amp_log == -2)) + geom_point(aes(x=Stim,y=CMAP, col=as.factor(sd_amp_log)),alpha =0.5)+
  geom_line(aes(x=Stim, y=E_CMAP, col = as.factor(sd_amp_log)), linewidth =1.2) + theme_light(base_size = 22)+
  scale_color_discrete(name = "MUAP variance",labels = c("Low", "High"))+
  theme(legend.position = 'inside', legend.position.inside = c(0.2,0.7), legend.background = element_rect(colour = "black", linewidth = 0.3))+
  labs(x="Stimulus (mA)")

#plot noise

ggplot(outcome)+ geom_point(aes(x= Stim, y = CMAP, col = as.factor(Noise)))+
  geom_line(aes(x=Stim, y = E_CMAP, col = as.factor(Noise)))+facet_wrap(~Noise)+
  theme_light(base_size = 22)+theme(legend.position = "none", strip.text = element_text(colour = "black", face = "bold"))+
  labs(x= "Stimulus (mA)")