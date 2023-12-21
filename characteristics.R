# Extract vital streamflow indicators

rm(list=ls())

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

library(dplyr)
library(readxl)
library(lubridate)
library(EflowStats)
library(sf)
library(ggplot2)
library(scales)
library(foreign)


yearday = function(yr) {
  flag = leap_year(as.Date(paste0(yr,'-01-01')))
  flag[flag=='FALSE'] = 365
  flag[flag==1] = 366
  return(flag)
}

dateBegin = '1951-01-01'
dateEnd = '2021-12-31'

# USGS station info
usgsSitesXl = read_excel("USGS_stations.xlsx")

for (i in 2:length(usgsSitesXl$site_no)) {
  print(i)
  
  # Read streamflow data
  usgsSiteSlt = usgsSitesXl[i,]
  streamflowFile = '***************put the path of streamflow files here***************'
  if (!file.exists(streamflowFile)) next
  streamflow = read.csv(streamflowFile)
  if (nrow(streamflow)==0) next
  streamflow = streamflow %>% filter(Date>=dateBegin) %>% filter(Date<=dateEnd) %>%
    filter(.[[ncol(streamflow)]]%in%unique(streamflow[,ncol(streamflow)])[grepl('A',unique(streamflow[,ncol(streamflow)]))])
  
  # Prepare data to calculate change point
  streamflowDF = streamflow %>%
    select(colnames(streamflow)[4],colnames(streamflow)[5]) %>%
    mutate_at(vars(colnames(streamflow)[4]), list(ymd)) %>%
    mutate(Year = format(Date, format = "%Y")) %>%
    na.omit()
  if (nrow(streamflowDF)==0) next
  obsCount = streamflowDF %>% group_by(Year) %>% summarise(n = n()) %>%
    mutate(daysYear = yearday(unique(streamflowDF$Year))) %>%
    filter(n==daysYear)
  streamflowDF = streamflowDF %>% filter(Year %in% as.character(obsCount[,1][[1]])) %>%
    select(1,2)
  if (nrow(streamflowDF)<365) next
  if (TRUE %in% is.nan(log10(streamflowDF[,2]))) next
  if (TRUE %in% is.na(log10(streamflowDF[,2]))) next
  
  if (i==2) {
    indice_hit = calc_allHIT(streamflowDF, yearType = 'calendar') %>%
      mutate(siteNo = usgsSiteSlt$site_no)
    lowHigh = as.numeric(quantile(streamflowDF[,2], c(0.05,0.95)))
    indice_quantile = data.frame(siteNo = usgsSiteSlt$site_no, low = lowHigh[1], high = lowHigh[2])
    indice_mag7 = calc_magnifSeven(streamflowDF, yearType = 'calendar') %>%
      mutate(siteNo = usgsSiteSlt$site_no)
  } else {
    indice_hit = rbind(indice_hit,
                       calc_allHIT(streamflowDF, yearType = 'calendar') %>%
                         mutate(siteNo = usgsSiteSlt$site_no))
    lowHigh = as.numeric(quantile(streamflowDF[,2], c(0.05,0.95)))
    indice_quantile = rbind(indice_quantile,
                          data.frame(siteNo = usgsSiteSlt$site_no, low = lowHigh[1], high = lowHigh[2]))
    indice_mag7 = rbind(indice_mag7,
                        calc_magnifSeven(streamflowDF, yearType = 'calendar') %>%
                          mutate(siteNo = usgsSiteSlt$site_no))
  }
}
write.csv(indice_hit, file = './streamflow_hit.csv')
write.csv(indice_quantile, file = './streamflow_quantile.csv')
write.csv(indice_mag7, file = './streamflow_MAG7.csv')