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
              'extrafont', 'Cairo', 'MLmetrics')

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
data.frame('model','FH','MAE','MAPE','RMSPE') %>%
  write.table(file = filename_eemd,
              append = TRUE,
              sep = ',',
              col.names = FALSE,
              row.names = FALSE)
data.frame('model','FH','MAE','MAPE','RMSPE') %>%
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

# Plot results ------------------------------------------------------------

## Plot Predict x Observed
setwd(FiguresDir)

datasets <- data.frame(
  'Observed'         = results$EEMD$Predictions$`1-step`[,'Obs'],
  'OSA.EEMD.BRNN'    = results$EEMD$Predictions$`1-step`[,'brnn'],
  'Observed'         = results$EEMD$Predictions$`7-step`[,'Obs'],
  'SSA.EEMD.CUBIST'  = results$EEMD$Predictions$`7-step`[,'cubist'],
  'Observed'         = results$EEMD$Predictions$`14-step`[,'Obs'],
  'FSA.EEMD.CUBIST'  = results$EEMD$Predictions$`14-step`[,'cubist']
) %>% melt() %>% data.frame(
  .,
  rep(c('Observed','Predicted'), each = nrow(results$EEMD$Predictions$`1-step`)),
  rep(c("One-day","Seven-days","Fourteen-days"), each = 2*nrow(results$EEMD$Predictions$`1-step`)),
  rep(tail(StoreSales$date, nrow(results$EEMD$Predictions$`1-step`)))
)

datasets$variable <- NULL
colnames(datasets) <- c('value', 'type', 'FH', 'date')

datasets$FH <- datasets$FH %>% factor(levels = c("One-day","Seven-days","Fourteen-days"))

PredObsPlot <- datasets %>% 
  ggplot(aes(x = date, y = value, colour = type)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.background = element_blank(),
        legend.text = element_text(size = 20),
        text = element_text(family = "CM Roman", size = 20),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
  ) +
  ylab('Sales') + xlab('Daily samples') +
  facet_grid(rows = vars(FH)) +
  scale_y_continuous(breaks = c(3000, 7500, 12000)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  scale_color_manual(values = c('#377EB8','#E41A1C')) +
  geom_vline(xintercept = StoreSales$date[120], color = 'black', size = 0.5) +
  annotate(geom = 'text', x = StoreSales$date[10], y = min(datasets$value), hjust = .2, vjust = -.4, 
           label = 'Training', color = 'black', family = 'CM Roman', size = 6) +
  annotate(geom = 'text', x = StoreSales$date[140], min(datasets$value), hjust = .2, vjust = -.4,
           label = 'Test', color = 'black', family = 'CM Roman', size = 6)

PredObsPlot %>%
  ggsave(
    filename = 'PO_dataset.pdf',
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  )

## Plot IMFs
setwd(FiguresDir)

IMFs <- results$EEMD$IMF
IMFs$n <- seq(nrow(results$EEMD$IMF))
IMFs <- IMFs %>% melt(id.vars = c('n'))

imf_labels <- c(
  expression(paste(IMF[1])),
  expression(paste(IMF[2])),
  expression(paste(IMF[3])),
  expression(paste(IMF[4])),
  expression(paste(IMF[5])),
  'Residual'
)

IMFs$variable <- IMFs$variable %>% 
  factor(
    levels = c('Obs', paste0('IMF',seq(5)), 'Residual'),
    labels = c('Obs', imf_labels)
  )

imf_plot <- IMFs %>% 
  filter(variable != 'Obs') %>%
  ggplot(aes(x = n, y = value, colour = variable)) +
  geom_line(size = 1, colour = '#377EB8') +
  theme_bw() +
  theme(
    text = element_text(family = "CM Roman", size = 16),
    axis.title.x = element_text(family = "CM Roman", size = 20),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(family = "CM Roman", size = 20),
  ) +
  ylab('') + xlab('Daily samples') +
  facet_grid(
    rows = vars(variable),
    scales = 'free',
    switch = 'y',
    labeller = "label_parsed",
  ) +
  scale_x_continuous(breaks = seq(0,max(IMFs$n),44))

imf_plot %>% 
  ggsave(
    filename = 'imf_plot.pdf',
    device = 'pdf',
    width = 6,
    height = 8,
    units = "in",
    dpi = 1200
  )

## Dataset plot
setwd(FiguresDir)

Observed <- StoreSales[,-3]

datasetplot <- Observed %>%
  ggplot(aes(x = date, y = sales)) +
  geom_line(size = 1, colour = '#377EB8') +
  theme_bw() +
  theme(
    text = element_text(family = "CM Roman", size = 20),
    axis.text.y = element_text(angle = 90),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  ylab('Sales') + xlab('Daily samples') +
  scale_y_continuous(breaks = c(3000, 7500, 12000)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b')
  
datasetplot %>%
  ggsave(
    filename = 'dataset_plot.pdf',
    device = 'pdf',
    width = 6,
    height = 6,
    units = "in",
    dpi = 1200
  )
