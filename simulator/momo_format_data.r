###########################################
# momo_format_data.r
# 
# Format MoMo test data.
# 
# Adrian Albert
# Last modified: June 2013.
###########################################

rm(list = ls())

library(lubridate)

format_data = function(data, ID) {
  data$SMS.Received = NULL
  intervals  = c(which(data$Time.stamp != '')) 
  timestamps = data$Time.stamp[intervals]
  intervals  = c(intervals, nrow(data)) 
  data$SMS.ID = NA
  for (i in 1:(length(intervals)-1)) {
    data$Time.stamp[intervals[i]:(intervals[i+1]-1)] = timestamps[i]
    data$SMS.ID[intervals[i]:(intervals[i+1]-1)] = paste('Sens', ID, 'SMS', i, sep = '.')
  }
  data$Time.stamp[nrow(data)] = timestamps[length(timestamps)]
  data$SMS.ID[nrow(data)]     = paste('Sens', ID, 'SMS', length(timestamps), sep = '.')
  data$X = NULL
  data = cbind(ID = ID, data)
  data = droplevels(data)
  return(data)
}

# read data from XLS file
master = read.csv('~/Dropbox/WellDone/Test/test_data/MoMo pilot data - Master.csv')
sens.1 = read.csv('~/Dropbox/WellDone/Test/test_data/MoMo pilot data - MoMo 1 - below capacity.csv')
sens.2 = read.csv('~/Dropbox/WellDone/Test/test_data/MoMo pilot data - MoMo 2 - functional.csv')
sens.3 = read.csv('~/Dropbox/WellDone/Test/test_data/MoMo pilot data - MoMo 3 - rainfall.csv')
sens.4 = read.csv('~/Dropbox/WellDone/Test/test_data/MoMo pilot data - MoMo 4 - broken.csv')
status = c('Below Capacity', 'Functional', 'Rainfall', 'Broken')

# format data for visualization
sens.1 = format_data(sens.1, 1)
sens.2 = format_data(sens.2, 2)
sens.3 = format_data(sens.3, 3)
sens.4 = format_data(sens.4, 4)

# form data frame of full data
sens   = rbind(sens.1, sens.2, sens.3, sens.4)

# replace NA with 0
sens[is.na(sens)] = 0

# convert time stamp to standard format
sens$Time.stamp = as.character(paste(as.character(sens$Time.stamp), '2012', sep = '/'))
sens$Time.stamp = as.character(strptime(sens$Time.stamp, '%H:%M%p %m/%d/%Y'), origin = '1970-01-01')

# save data to file / MySQL
write.csv(file = '~/Dropbox/WellDone/Test/test_data/MoMo_test_data.csv', sens, row.names = F)
save(file =  '~/Dropbox/WellDone/Test/test_data/MoMo_test_data.RData',
     list = c('sens', 'master'))
