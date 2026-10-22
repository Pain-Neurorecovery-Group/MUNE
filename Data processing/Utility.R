# Function for loading the Stimulus-response data from the MEM files for one file

load_MEM_data = function(path){
  mem = readLines(path)
  
  ms_lines = grep("^MS\\.", mem, value = TRUE)
  
  data = read.table(text = ms_lines, header = F)
  
  names(data) = c("MS", "Stim(mA)", "Resp(mV)")
  
  data$ID = tools::file_path_sans_ext(basename(path))
  
  return(data)}

# Function to plot Stimuli over time and Response over time 

plot_S_R = function(data, all = F){
  if (all == F){ 
    P_S = ggplot(data, aes(x = `Stim(mA)`, y = `Stim(mA)`))+
      geom_point(col = "red")+theme_classic()+labs(x= "Stimulus number", title = "Stimulus Level over time")+
      theme(plot.title = element_text(hjust = 0.5))
    
    P_R = ggplot(data, aes(x = `Stim(mA)`, y = `Resp(mV)`))+
      geom_point(col = "blue")+theme_classic()+labs(x= "Stimulus number", title = "Peak Response over time")+
      theme(plot.title = element_text(hjust = 0.5))
    
    print(P_S/ P_R)} 
  
  if (all ==T){ 
    plots = lapply(split(data, data$ID), plot_PP)
    print(wrap_plots(plots))
    }
}

plot_PP = function(data){
  P_S = ggplot(data, aes(x = seq_along(`Stim(mA)`), y = `Stim(mA)`)) + geom_point(color = "red") +
                 theme_classic() +labs(x = "Stimulus number",y = "Stimulus (mA)", title = unique(data$ID))
               
   P_R <- ggplot(data, aes(x = seq_along(`Stim(mA)`), y = `Resp(mV)`))+geom_point(color = "blue") +
                   theme_classic() + labs(x = "Stimulus number",y = "Peak response (mV)")
                             
   P_S/ P_R
}

# Function for loading all the Stimulus-response data from the MEM files 
load_all_MEM = function(dir_files){
  files = list.files(path = dir_files, pattern = 'MEM', full.names = T)
  
  data = lapply(files, load_MEM_data)
  data = dplyr::bind_rows(data)
  return(data)
}