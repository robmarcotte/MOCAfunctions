# Dev_DO_Viz
#
# Function to visualize device data with direct observation data
#
# Library dependencies: ggplot2, dplyr, tidyr, stringr

Dev_DO_Viz = function(rds_filepaths, output_filepath, overwrite = T, samp_freq = 80){

  plot_folder_exist = dir.exists(paste(output_filepath, '/Visual Inspection Plots', sep = ''))

  if(plot_folder_exist == T & overwrite == F){

    stop('Found an existing folder with Visual Inspection Plots in the designated output_filepath location. \nTo preserve prior plots, rename the existing folder.')

  } else {

    if(plot_folder_exist == T){
      warning('Found an existing folder with Visual Inspection Plots in the designated output_filepath location. \nTo preserve prior plots, rename the existing folder.\nOverwriting plot contents by default...')
    }

    dir.create(paste(output_filepath, '/Visual Inspection Plots', sep = ''), showWarnings = F)

    for(dddd in 1:length(rds_filepaths)){

      dev_data = readRDS(rds_filepaths[dddd])

      filename = str_replace(rds_filepaths[dddd], pattern = '.rds', '')

      dev_data$seconds = floor(seq(0,nrow(dev_data)/samp_freq, by = 1/samp_freq)[1:nrow(dev_data)])

      dev_data = dev_data %>% group_by(seconds) %>% dplyr::summarise(AxisX = mean(AxisX, na.rm = T),
                                                                   AxisY = mean(AxisY, na.rm = T),
                                                                   AxisZ = mean(AxisZ, na.rm = T),
                                                                   VM = mean(VM, na.rm = T),
                                                                   Behavior = first(Behavior),
                                                                   Modifier_1 = first(Modifier_1),
                                                                   Modifier_2 = first(Modifier_2))

      dev_data = dev_data %>% gather(AxisX:VM, key = 'Signal', value = 'Acceleration')

      if(length(unique(dev_data$Behavior)) >1){

        plot_data = ggplot(data = dev_data, aes(x = seconds, y = Acceleration, color = Signal)) +
          geom_rect(inherit.aes = F, aes(xmin = seconds, xmax = seconds+1, ymin = min(dev_data$Acceleration), ymax = max(dev_data$Acceleration), fill = Behavior), alpha = 0.2) +
          geom_line(alpha = 0.6) + scale_color_manual(values = c('red','blue','green','black')) + theme_minimal() +
          labs(title = filename, x = 'Time (secs)',y = 'Acceleration (average g/1-sec)')

        ggsave(plot = plot_data, filename = paste(output_filepath, '/Visual Inspection Plots',  '/', filename,'.png', sep = ''), device = 'png',height = 8, width = 10.5)
      }
      else {
        behavior = unique(dev_data$Behavior)
        plot_data = ggplot(data = dev_data, aes(x = seconds, y = Acceleration, color = Signal)) +
          geom_rect(inherit.aes = F, aes(xmin = seconds, xmax = seconds+1, ymin = min(dev_data$Acceleration), ymax = max(dev_data$Acceleration), fill = Modifier_2), alpha = 0.2) +
          geom_line(alpha = 0.6) + scale_color_manual(values = c('red','blue','green','black')) + theme_minimal() +
          labs(title = str_c(filename, ' (Behavior: ', behavior, ')', sep = ''), x = 'Time (secs)',y = 'Acceleration (average g/1-sec)')

        ggsave(plot = plot_data, filename = paste(output_filepath, '/Visual Inspection Plots',  '/', filename,'.png', sep = ''), device = 'png',height = 8, width = 10.5)

      }
    }
  }
}
