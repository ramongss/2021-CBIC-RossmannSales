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
rawdata <- readr::read_csv('train.csv')

# randomly choose 4 store
set.seed(123)
stores <- sample(1:max(rawdata$Store), 4, replace = F)

# filter the store sales by the selected store
StoreSales <- rawdata %>%
  janitor::clean_names() %>% 
  filter(store %in% stores & open == 1) %>%
  subset(select = c(store, date, sales, customers)) %>%
  group_by(store) %>%
  arrange(date, .by_group = TRUE) %>% 
  split(., .$store)

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
count <- 1
for (store in paste0("store-",sort(stores))) {
  print(store)
  results[[store]] <- list()
  results[[store]][["EEMD"]] <- eemd_pred(StoreSales[[count]], model_list, horizon)
  results[[store]][["single"]] <- single_pred(StoreSales[[count]], model_list, horizon)
  count <- count + 1
}

beepr::beep(8)

# save results
saveRDS(object = results, file = 'results.rds')

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

# Load data ---------------------------------------------------------------
setwd(ResultsDir)

results <- list()

results[['EEMD']] <- readRDS('results_eemd.rds')
results[['single']] <- readRDS('results_single.rds')

# Plot results ------------------------------------------------------------
  ## Plot Predict x Observed ----
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
    rep(c("One day","Seven days","Fourteen days"), each = 2*nrow(results$EEMD$Predictions$`1-step`)),
    rep(tail(StoreSales$date, nrow(results$EEMD$Predictions$`1-step`)))
  )
  
  datasets$variable <- NULL
  colnames(datasets) <- c('value', 'type', 'FH', 'date')
  
  datasets$FH <- datasets$FH %>% factor(levels = c("One day","Seven days","Fourteen days"))
  
  PredObsPlot <- datasets %>% 
    ggplot(aes(x = date, y = value, colour = type)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          legend.background = element_blank(),
          legend.text = element_text(size = 35),
          text = element_text(family = "CM Roman", size = 35),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
    ) +
    ylab('Sales') + xlab('Day') +
    facet_grid(rows = vars(FH)) +
    scale_y_continuous(breaks = c(3000, 7500, 12000)) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b') +
    scale_color_manual(values = c('#377EB8','#E41A1C')) +
    geom_vline(xintercept = StoreSales$date[120], color = 'black', size = 0.5) +
    annotate(geom = 'text', x = StoreSales$date[10], y = min(datasets$value), hjust = .2, vjust = -.4, 
             label = 'Training', color = 'black', family = 'CM Roman', size = 12) +
    annotate(geom = 'text', x = StoreSales$date[170], min(datasets$value), hjust = .2, vjust = -.4,
             label = 'Test', color = 'black', family = 'CM Roman', size = 12)
  
  PredObsPlot %>%
    ggsave(
      filename = 'PO_dataset.pdf',
      device = 'pdf',
      width = 12,
      height = 12,
      units = "in",
      dpi = 1200
    )
  
  ## Plot IMFs ----
  setwd(FiguresDir)
  
  IMFs <- results$EEMD$IMF
  IMFs$n <- StoreSales$date
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
    geom_line(size = 0.5, colour = '#377EB8') +
    theme_bw() +
    theme(
      text = element_text(family = "CM Roman", size = 14),
      # axis.title.x = element_text(family = "CM Roman", size = 16),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      # strip.text = element_text(family = "CM Roman", size = 16),
    ) +
    ylab('') + xlab('Day') +
    facet_grid(
      rows = vars(variable),
      scales = 'free',
      switch = 'y',
      labeller = "label_parsed",
    ) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b')
  
  imf_plot %>% 
    ggsave(
      filename = 'imf_plot.pdf',
      device = 'pdf',
      width = 4.5,
      height = 8,
      units = "in",
      dpi = 1200
    )
  
  ## Dataset plot ----
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
      width = 8,
      height = 4.5,
      units = "in",
      dpi = 1200
    )
  
  ## Customer plot ----
  setwd(FiguresDir)
  
  customer <- StoreSales[,-2]
  
  customerplot <- customer %>%
    ggplot(aes(x = date, y = customers)) +
    geom_line(size = 1, colour = '#E41A1C') +
    theme_bw() +
    theme(
      text = element_text(family = "CM Roman", size = 20),
      axis.text.y = element_text(angle = 90),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
    ) +
    ylab('Number of customers') + xlab('Daily samples') +
    scale_x_date(date_breaks = '1 month', date_labels = '%b')
  
  customerplot %>%
    ggsave(
      filename = 'customer_plot.pdf',
      device = 'pdf',
      width = 8,
      height = 4.5,
      units = "in",
      dpi = 1200
    )
  
  ## Facet plot ----
  setwd(FiguresDir)
  
  facet <- StoreSales %>% 
    melt(id.vars = c('date'))
  
  facet$variable <- facet$variable %>% 
    factor(
      levels = c('sales', 'customers'),
      labels = c('Sales', 'No. of customers')
    )
  
  facetplot <- facet %>%
    ggplot(aes(x = date, y = value, colour = variable)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(
      text = element_text(family = "CM Roman", size = 20),
      axis.text.y = element_text(angle = 90),
      axis.title.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
    ) +
    facet_grid(
      rows = vars(variable),
      scales = 'free',
      switch = 'y',
    ) +
    xlab('Day') +
    scale_color_manual(values = c('#377EB8', '#E41A1C')) +
    scale_y_continuous(breaks = c(400, 600, 800, 3000, 7500, 12000)) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b')
  
  facetplot %>%
    ggsave(
      filename = 'facet_plot.pdf',
      device = 'pdf',
      width = 8,
      height = 9,
      units = "in",
      dpi = 1200
    )
  
  ## BoxPlot ----
  setwd(FiguresDir)
  
  boxplot_data <- rawdata %>%
    filter(Store == store & Date >= "2015-01-01" & Open == 1) %>%
    subset(select = c(DayOfWeek, Sales, Customers)) %>%
    janitor::clean_names() %>% 
    melt(id.vars = c('day_of_week'))
  
  boxplot_data$day_of_week <- boxplot_data$day_of_week %>% 
    factor(
      levels = c(1:6),
      labels = c('Mon.', 'Tue.', 'Wed.', 'Thu.', 'Fri.', 'Sat.')
    )
  
  boxplot_data$variable <- boxplot_data$variable %>% 
    factor(
      levels = c('sales', 'customers'),
      labels = c('Sales', 'No. of customers')
    )
  
  boxplot <- boxplot_data %>%  
    ggplot(aes(x = day_of_week, y = value, fill = variable)) +
    geom_boxplot() +
    labs(x = "Day of the week") + 
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      text = element_text(family = "CM Roman", size = 35),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'None',
    ) +
    facet_wrap(~variable, ncol = 2, scales = "free", strip.position="left") +
    scale_fill_manual(values = alpha(c('#377EB8', '#E41A1C'), 0.7))
  
  boxplot %>%
    ggsave(
      filename = 'boxplot.pdf',
      device = 'pdf',
      width = 16,
      height = 9,
      units = "in",
      dpi = 1200
    )
  
# Summary table -----------------------------------------------------------
summaries_table <- data.frame(
  'Variable' = rep(names(StoreSales)[-1], times = 3),
  'Samples' = rep(c('Whole', 'Training', 'Test'), each = ncol(StoreSales[-1]))
)

#Descriptives
n <- nrow(StoreSales)
cut <- n - 51

#Whole
Whole <- t(apply(StoreSales[,-1],2,function(x){c(mean(x),sd(x),min(x),max(x))}))
colnames(Whole) <- c('Mean', 'Std', 'Min', 'Max')
#Train Descriptives
Train <- t(apply(StoreSales[1:cut,-1],2,function(x){c(mean(x),sd(x),min(x),max(x))}))
colnames(Train) <- names(Whole)
#Test Descriptives
Test <- t(apply(tail(StoreSales[,-1],n - cut),2,function(x){c(mean(x),sd(x),min(x),max(x))}))
colnames(Test) <- names(Whole)

#Merge
summaries_table <- cbind(summaries_table, rbind(Whole, Train, Test))
row.names(summaries_table) <- NULL # reset row index

# Reorder rows
summaries_table <- summaries_table %>% 
  arrange(factor(Variable, levels = names(StoreSales[-1])))

print(xtable::xtable(summaries_table, digits = 2), include.rownames = FALSE)

# Diebold-Mariano test ----------------------------------------------------
error <- list()
DM_tvalue <- list()
DM_pvalue <- list()
DM_presult <- list()
n <- nrow(results$EEMD$Errors$`1-step`)
cut <- round(n*0.7)
for (horizon in seq(results$EEMD$Errors)) {
  # taking errors from ceemd_stack
  error[[horizon]] <- tail(results$EEMD$Errors[[horizon]], n-cut)
  # cbind the errors from ceemd_stack with stack
  error[[horizon]] <- cbind(
    error[[horizon]], 
    tail(results$single$Errors[[horizon]], n-cut)
  )
  # rename columns
  colnames(error[[horizon]]) <- LETTERS[1:ncol(error[[horizon]])]
  
  # DM test
  DM_tvalue[[horizon]] <- matrix(
    nrow = ncol(error[[horizon]]),
    ncol = ncol(error[[horizon]])
  )
  colnames(DM_tvalue[[horizon]]) <- LETTERS[1:ncol(error[[horizon]])]
  rownames(DM_tvalue[[horizon]]) <- LETTERS[1:ncol(error[[horizon]])]
  DM_pvalue[[horizon]] <- DM_tvalue[[horizon]]
  DM_presult[[horizon]] <- DM_tvalue[[horizon]]
  
  for (col in seq(nrow(DM_tvalue[[horizon]]))) {
    for (row in seq(nrow(DM_tvalue[[horizon]]))) {
      if (col == row) {
        DM_tvalue[[horizon]][row, col] <- NA
        DM_pvalue[[horizon]][row, col] <- NA
      } else if (error[[horizon]][,col] == error[[horizon]][,row]) {
        DM_tvalue[[horizon]][row, col] <- NA
        DM_pvalue[[horizon]][row, col] <- NA
      } else {
        DMtest <- dm.test(
          error[[horizon]][,col],
          error[[horizon]][,row],
          h = horizon, power = 1
        )
        DM_tvalue[[horizon]][row,col] <- DMtest$statistic
        DM_pvalue[[horizon]][row,col] <- DMtest$p.value
        
        if (DM_pvalue[[horizon]][row,col] <= 0.01) {
          DM_presult[[horizon]][row,col] <- '*'
        } else if (DM_pvalue[[horizon]][row,col] <= 0.05 && DM_pvalue[[horizon]][row,col] > 0.01){
          DM_presult[[horizon]][row,col] <- '**'
        } else if (DM_pvalue[[horizon]][row,col] > 0.05){
          DM_presult[[horizon]][row,col] <- NA
        }
      }
    }
  }
  
}
names(error) <- paste0(c(1,7,14),'-step')
names(DM_tvalue) <- names(error)
names(DM_pvalue) <- names(error)
names(DM_presult) <- names(error)