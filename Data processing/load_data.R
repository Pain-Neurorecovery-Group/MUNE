# Load in packages
pacman::p_load("tidyverse", "here", "ggplot2", "patchwork")
source(here("Data processing", "Utility.R"))

# Load in data for 1 MEM file 
MEM = "D:\\Mune\\HTC60304A.MEM"

data = load_MEM_data(MEM)

# Load in data for all MEM files

dir_files = "D:\\Mune"

data_all = load_all_MEM(dir_files)

# Inspect data 
plot_S_R(data_all, all =T)

data = data_all %>% filter(ID == 'HTC60304A')
data <- data %>%
  mutate(highlight = case_when( abs(`Resp(mV)` - 2.707) < 0.001 ~ "Punt_1",
    abs(`Resp(mV)` - 5.545) < 0.001 ~ "Punt_2",abs(`Resp(mV)` - 8.002) < 0.001 ~ "Punt_3",
    TRUE                            ~ "Normaal"  ))

data <- data %>% arrange(highlight)

ggplot(data)+geom_point(aes(x= `Stim(mA)`, y = `Resp(mV)`, col = highlight, size = highlight, alpha = highlight))+theme_light(base_size = 22)+
  labs(x= 'Stimulus (mA)', y = "CMAP peak (mV)", title = "CMAP Scan")+
  scale_color_manual(values = c("Normaal" = "black", "Punt_1" = "#FFD92F", "Punt_2"= "#00A65A", "Punt_3" = "#4C78D0"))+
  scale_size_manual(values=c("Normaal" = 2, "Punt_1" =6.5, "Punt_2"= 6.5, "Punt_3" = 6.5))+
  scale_alpha_manual(values = c("Normaal" = 0.5, "Punt_1" =1, "Punt_2"= 1, "Punt_3" = 1))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggplot(data) +
  # Laag 1: Eerst alle zwarte punten (Normaal)
  geom_point(data = filter(data, highlight == "Normaal"), 
             aes(x = `Stim(mA)`, y = `Resp(mV)`), 
             color = "black", size = 2.5) +
  
  # Laag 2: De gekleurde punten eroverheen, een slag groter (size = 4.5)
  geom_point(data = filter(data, highlight != "Normaal"), 
             aes(x = `Stim(mA)`, y = `Resp(mV)`, col = highlight), 
             size = 4.5) +
  
  theme_light(base_size = 22) +
  labs(x = 'Stimulus (mA)', y = "CMAP peak (mV)", title = "CMAP Scan") +
  scale_color_manual(values = c("Punt_1" = "#F8766D", "Punt_2" = '#00BA38', "Punt_3" = '#619CFF')) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 36)
  )