# Set environment ---------------------------------------------------------
rm(list = ls())
Sys.setenv('LANGUAGE' = 'En')
Sys.setlocale('LC_ALL', 'English')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, 'Codes', sep = '/')
DataDir       <- paste(BaseDir, 'Data', sep = '/')
FiguresDir    <- paste(BaseDir, 'Figures', sep = '/')
ResultsDir    <- paste(BaseDir, 'Results', sep = '/')
SheetsDir     <- paste(BaseDir, 'Sheets', sep = '/')

# load Packages
setwd(CodesDir)
source('checkpackages.R')
source('eemd_pred.R')
source('single_pred.R')

packages <- c('dplyr', 'tidyverse', 'magrittr', 'caret', 'reshape2', 
              'gghighlight', 'TTR', 'forecast', 'Metrics', 'e1071', 'cowplot', 
              'elmNNRcpp', 'hht', 'grid', 'tcltk', 'foreach','lmtest', 
              'extrafont', 'Cairo')

sapply(packages,packs)

rm(packages)

library(extrafont)
# font_import(pattern = 'CM')
library(ggplot2)

# Data treatment ----------------------------------------------------------
# set working directory
setwd(DataDir)

# load data
rawdata <- read.csv('train.csv')

rawdata$Date <- rawdata$Date %>% as.Date() # format date column

# randomly choose 1 store
set.seed(123)
store <- sample(1:max(rawdata$Store), 1, replace = F)

# replace character column values
rawdata$StateHoliday[rawdata$StateHoliday=='a'] <- 1
rawdata$StateHoliday[rawdata$StateHoliday=='b'] <- 2
rawdata$StateHoliday[rawdata$StateHoliday=='c'] <- 3
rawdata$StateHoliday <- rawdata$StateHoliday %>% as.numeric()

# filter the store sales by the selected store
StoreSales <- rawdata %>%
  filter(Store == store & Date >= "2015-01-01" & Open == 1) %>%
  subset(select = c(Date, Sales, Customers)) %>%
  janitor::clean_names()

StoreSales <- StoreSales[order(StoreSales$date),] # order by date

# set working directory
setwd(ResultsDir)

# list of models
model_list <- c(
  'brnn',
  'cubist',
  'svmRadial'
) %>% sort()

# horizons
horizon <- c(1,7,14)

# training phase
results <- list()

results[['EEMD']] <- eemd_pred(StoreSales, model_list, horizon)
results[['single']] <- single_pred(StoreSales, model_list, horizon)

# save results
saveRDS(
  object = results[['EEMD']],
  file = paste0('results_eemd.rds')
)

saveRDS(
  object = results[['single']],
  file = paste0('results_single.rds')
)

# Save Sheets -------------------------------------------------------------
# set working directory
setwd(SheetsDir)

# loop to save metrics results
FH <- paste0(horizon, '-steps') # aux to create forecasting horizon column

filename_eemd <- paste0('metrics_eemd.csv') # eemd file name
filename_single <- paste0('metrics_single.csv') # single file name

file.create(filename_eemd) # create file eemd
file.create(filename_single) # create file single

# append header in csv files
data.frame('model','FH','MAE','MAPE','RMSE') %>%
  write.table(file = filename_eemd,
              append = TRUE,
              sep = ',',
              col.names = FALSE,
              row.names = FALSE)
data.frame('model','FH','MAE','MAPE','RMSE') %>%
  write.table(file = filename_single,
              append = TRUE,
              sep = ',',
              col.names = FALSE,
              row.names = FALSE)

for (metric in seq(results[['EEMD']]$Decomp_Metrics)) {
  # save eemd metrics in csv
  data.frame(
    FH = rep(FH[metric]),
    results[['EEMD']]$Decomp_Metrics[[metric]][,-1]
  ) %>%
    write.table(file = filename_eemd,
                append = TRUE,
                sep = ',',
                col.names = FALSE,
                row.names = TRUE)
  
  # save single metrics in csv
  data.frame(
    FH = rep(FH[metric]),
    results[['single']]$Metrics[[metric]][,-1]
  ) %>%
    write.table(file = filename_single,
                append = TRUE,
                sep = ',',
                col.names = FALSE,
                row.names = TRUE)
}

