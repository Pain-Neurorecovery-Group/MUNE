pacman::p_load('biosignalEMG', 'tidyverse', 'ggplot2')

generate_cmap_curve <- function(time, amplitude, onset, zero_crossing, decay = 0.3) {
  # Initialize baseline voltage vector with zeros
  voltage <- rep(0, length(time))
  
  # Only calculate where time is after the onset
  active_mask <- time >= onset
  t_active <- time[active_mask] - onset
  
  # Calculate the exact frequency needed to force the zero-crossing at the right time
  duration_to_zero <- zero_crossing - onset
  omega <- pi / duration_to_zero
  
  # Generate the raw biphasic wave shape (Sine wave + Exponential decay)
  raw_wave <- sin(omega * t_active) * exp(-decay * t_active)
  
  # Normalize the wave so that the 'amplitude' parameter matches the peak exactly
  # (Finds the maximum value of the shape and scales it)
  scaling_factor <- amplitude / max(raw_wave)
  parameterized_wave <- raw_wave * scaling_factor
  
  # Insert into our voltage vector
  voltage[active_mask] <- parameterized_wave
  
  return(voltage)
}

# 1. Define your time scale (e.g., 0 to 15 ms, with fine steps)
time_steps <- seq(0, 15, by = 0.01)

# 2. Generate a custom curve by defining your exact landmarks
my_voltage_1 <- generate_cmap_curve(
  time          = time_steps,
  amplitude     = 2.707,            # Peak height (mV)
  onset         = 3.0,            # Starts exactly at 3.0 ms
  zero_crossing = 7.2,            # Crosses 0 line exactly at 7.2 ms
  decay         = 0.4            # Controls how fast the positive tail fades out
)

my_voltage_2 <- generate_cmap_curve(
  time          = time_steps,
  amplitude     = 5.545,            # Peak height (mV)
  onset         = 3.0,            # Starts exactly at 3.0 ms
  zero_crossing = 7.2,            # Crosses 0 line exactly at 7.2 ms
  decay         = 0.4            # Controls how fast the positive tail fades out
)

my_voltage_3 <- generate_cmap_curve(
  time          = time_steps,
  amplitude     = 8,            # Peak height (mV)
  onset         = 3.0,            # Starts exactly at 3.0 ms
  zero_crossing = 7.2,            # Crosses 0 line exactly at 7.2 ms
  decay         = 0.4            # Controls how fast the positive tail fades out
)

df_long <- data.frame(
  time    = rep(time_steps, 3),
  voltage = c(my_voltage_1, my_voltage_2, my_voltage_3),
  trace   = rep(c("Trace_1", "Trace_2", "Trace_3"), each = length(time_steps))
)

df_points = data.frame(time = rep(4.46, times = 3), peak = c(2.707,5.545,8), trace = c("Trace_1", "Trace_2", "Trace_3"))

# 3. Quick test plot (Base R)
ggplot(df_long)+geom_line(aes(x=time,y = voltage, col = as.factor(trace)), linewidth = 1.2)+theme_light(base_size = 22)+labs(x= 'Time (ms)', y= 'Potential (mV)')+
  theme(legend.position = "none")+geom_hline(yintercept = 0, alpha = 0.5)+
  scale_colour_manual(values = c(
    "Trace_1" = "#FFD92F",  # Vibrant teal
    "Trace_2" = "#00A65A",  # Bright blue
    "Trace_3" = "#4C78D0"   # Bright golden yellow
  ))+
  geom_point(data = df_points, aes(x= time, y = peak, col = as.factor(trace)), size = 4.5)+scale_x_continuous(limits = c(3,15))