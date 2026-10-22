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