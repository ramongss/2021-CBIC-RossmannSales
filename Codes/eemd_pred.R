eemd_pred <- function(data, model_list, horizon){
  cat('\n\n######### Decomposition Prediction #########\n\n')
  # Data preprocessing ------------------------------------------------------
  # set random seed
  set.seed(1234)
  
  # decomposition eemd
  EEMD(
    sig = data$sales,
    tt = seq(nrow(data)),
    trials = 100,
    noise.amp = sd(data$sales)*.25,
    nimf = 5,
    verbose = F
  )
  
  eemd <- EEMDCompile(
    trials.dir = 'trials',
    trials = 100,
    nimf = 5
  )
  
  imfs <- data.frame(
    eemd$averaged.imfs,
    eemd$averaged.residue
  )
  
  colnames(imfs) <- c(paste0('IMF',seq(5)), 'Residual')
  
  # create dataframes
  lag <- 5 # number of lags

  input <- lags(data[,-c(1,2)], lag)[,6] # create lagged input
  # colnames(input) <- colnames(data)[-c(1,2)]
  
  IMF <- list() # list to save dfs
  
  # create df for each imf with lags and inputs
  for (ii in seq(ncol(imfs))) {
    IMF[[ii]] <- data.frame(
      lags(imfs[,ii], lag),
      input
    )
    
    # rename lagged columns
    names(IMF[[ii]]) <- c(
      'y',
      paste0('lag',seq(lag)),
      colnames(data)[-c(1,2)]
    )
  }
  
  # rename the list
  names(IMF) <- names(imfs)
  
  # create training and test sets
  IMF_train  <- list()
  IMF_test   <- list()
  IMF_xtrain <- list()
  IMF_ytrain <- list()
  IMF_xtest  <- list()
  IMF_ytest  <- list()
  
  for (i in seq(IMF)) {
    n <- nrow(IMF[[i]])
    cut <- round(n*0.7)
    
    IMF_train[[i]] <- IMF[[i]][1:cut,]
    IMF_test[[i]]  <- tail(IMF[[i]],n-cut)
    
    IMF_xtrain[[i]] <- IMF_train[[i]][,-1]
    IMF_ytrain[[i]] <- IMF_train[[i]][,1]
    
    IMF_xtest[[i]] <- IMF_test[[i]][,-1]
    IMF_ytest[[i]] <- IMF_test[[i]][,1]
  }
  
  # create Observed dfs
  Obs <- lags(data$sales, lag)[,1]
  Obs_train <- Obs[1:cut]
  Obs_test  <- tail(Obs,n-cut)
  
  IMF_df <- data.frame(data$sales, imfs)
  colnames(IMF_df) <- c('Obs', names(imfs))
  
  # Training phase ----------------------------------------------------------
  # set random seed
  set.seed(1234)

  # set traincontrol
  control <- trainControl(
    method = "cv",
    number = 5
  )
  
  # list of training models
  model_list <- model_list
  
  # define objects
  IMF_model <- list()
  IMF_pred <- list()
  Params <- list()
  Importance <- list()
  k <- 1 # aux
  
  # training and predicting each IMF with each model
  for (imf in seq(IMF)) {
    Importance[[imf]] <- matrix(nrow = ncol(IMF_xtrain[[imf]]), ncol = length(model_list))
    colnames(Importance[[imf]]) <- model_list
    rownames(Importance[[imf]]) <- colnames(IMF_xtrain[[imf]])
    for (model in seq(model_list)) {
      # fitting
      IMF_model[[k]] <- train(
        y~., data = IMF_train[[imf]],
        method = model_list[model],
        trControl = control,
        preProcess = c('center','scale')
      )
      
      # save hyperparameters
      Params[[k]] <- IMF_model[[k]]$bestTune
      
      # save variables importance
      Importance[[imf]][,model] <- varImp(IMF_model[[k]], 
                                          scale = FALSE)$importance$Overall
      
      # prediction
      IMF_train_pred <- predict(IMF_model[[k]], IMF_train[[imf]])
      IMF_test_pred <- predict(IMF_model[[k]], IMF_test[[imf]])
      IMF_pred[[k]] <- data.frame(c(IMF_train_pred, IMF_test_pred))
      
      # print steps
      cat("\nModel: ", model_list[model], "\tIMF", imf, "\t",
          as.character(format.Date(Sys.time(), '%H:%M:%S')),
          sep = '')
      
      # update k aux
      k <- k + 1
    }
  }
  
  names(Importance) <- names(IMF)
  
  # create a matrix combination
  combs <- matrix(nrow = length(model_list), ncol = length(IMF))
  colnames(combs) <- names(IMF)
  
  for (i in seq(model_list)) {
    for (j in seq(IMF)) {
      combs[i,j] <- i
    }
  }
  
  # Multi-step predictions ----
  
  # save IMF_model into a matrix
  model_matrix <- data.frame(matrix(IMF_model, ncol=length(IMF), byrow = FALSE))
  names(model_matrix) <- names(IMF)
  
  ## Recursive prediction
  
  # define objects
  step_eemd_pred <- list()
  metrics_eemd_train <- list()
  metrics_eemd_test <- list()
  predictions <- list()
  errors <- list()
  horizon <- horizon
  for (h in seq(length(horizon))) {
    {
      hrz <- horizon[h]
      PTRmo <- list()
      PTEmo <- list()
      Comp_train <- list()
      Comp_test <- list()
      k <- 1
    }
    
    cat('\nHorizon: ', hrz, 'steps\n')
    
    for (m in 1:length(model_list)) {
      
      Comp_train[[m]] <- IMF_xtrain
      
      Comp_test[[m]] <- IMF_xtest
      
      PTRmo[[m]] <- matrix(ncol = length(IMF), nrow = nrow(IMF_xtrain[[1]]))
      PTEmo[[m]] <- matrix(ncol = length(IMF), nrow = nrow(IMF_xtest[[1]]))
      
      for (c in 1:length(Comp_train[[m]])) {
        if (hrz == 1) {
          # train
          PTRmo[[m]][,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]]))
          
          # test
          PTEmo[[m]][,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]]))
        } else {
          # train
          for(p in 1:cut) {
            if(p%%hrz !=1) {
              PTRmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(hrz-1)) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              }
            } else {
              Comp_train[[m]][[c]][p:cut,] <- IMF_xtrain[[c]][p:cut,]
              PTRmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(hrz-1)) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              }
            }
          }
          
          # test
          for(p in 1:(n-cut)) {
            if(p%%hrz !=1) {
              PTEmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(hrz-1)) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              }
            } else {
              Comp_test[[m]][[c]][p:(n-cut),] <- IMF_xtest[[c]][p:(n-cut),]
              PTEmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(hrz-1)) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              }
            }
          }
        }
        
        cat("Model: ", model_list[m], "\tIMF: ", c , "\t", 
            (k/(length(model_list)*length(Comp_train[[m]])))*100,"%\n", sep = "")
        
        k <- k + 1
      }
    }
    
    {
      metrics_eemd_train[[h]] <- matrix(nrow = dim(combs)[1],ncol = 4)
      metrics_eemd_test[[h]] <- matrix(nrow = dim(combs)[1],ncol = 4)
      colnames(metrics_eemd_train[[h]]) <- c("i","MAE","MAPE","RMSE")
      colnames(metrics_eemd_test[[h]]) <- colnames(metrics_eemd_train[[h]])
      rownames(metrics_eemd_train[[h]]) <- model_list
      rownames(metrics_eemd_test[[h]]) <- rownames(metrics_eemd_train[[h]])
      
      step_eemd_pred_train <- matrix(nrow = dim(IMF_xtrain[[1]])[1], ncol = dim(combs)[1])
      step_eemd_pred_test <- matrix(nrow = dim(IMF_xtest[[1]])[1], ncol = dim(combs)[1])
      step_eemd_pred[[h]] <- matrix(nrow = n, ncol = dim(combs)[1])
    }
    
    for (i in 1:dim(combs)[1]) {
      step_eemd_pred_train[,i] <- rowSums(PTRmo[[i]])
      step_eemd_pred_test[,i] <- rowSums(PTEmo[[i]])
      
      ### Avoiding negative values
      for (j in 1:dim(step_eemd_pred_train)[1]) {
        if (step_eemd_pred_train[j,i] < 0) {
          step_eemd_pred_train[j,i] <- 0
        }
      }
      for (j in 1:dim(step_eemd_pred_test)[1]) {
        if (step_eemd_pred_test[j,i] < 0) {
          step_eemd_pred_test[j,i] <- 0
        }
      }
      
      step_eemd_pred[[h]][,i] <- c(step_eemd_pred_train[,i],
                                  step_eemd_pred_test[,i])
      
      # metrics
      eemd_step_mae_train <- MAE(step_eemd_pred_train[,i], Obs_train)
      eemd_step_mape_train <- mape(step_eemd_pred_train[,i], Obs_train)
      eemd_step_rmse_train <- RMSE(step_eemd_pred_train[,i], Obs_train)
      
      eemd_step_mae_test <- MAE(step_eemd_pred_test[,i], Obs_test)
      eemd_step_mape_test <- mape(step_eemd_pred_test[,i], Obs_test)
      eemd_step_rmse_test <- RMSE(step_eemd_pred_test[,i], Obs_test)
      
      metrics_eemd_train[[h]][i,] <- c(i,
                                      eemd_step_mae_train,
                                      eemd_step_mape_train,
                                      eemd_step_rmse_train)
      metrics_eemd_test[[h]][i,] <- c(i,
                                     eemd_step_mae_test,
                                     eemd_step_mape_test,
                                     eemd_step_rmse_test)
    }
    step_eemd_pred[[h]] <- cbind(Obs, step_eemd_pred[[h]]) 
    colnames(step_eemd_pred[[h]]) <- c('Obs',model_list)
  
    predictions[[h]] <- cbind(step_eemd_pred[[h]])
    
    errors[[h]] <- matrix(ncol = ncol(predictions[[h]])-1, nrow = n)
    colnames(errors[[h]]) <- colnames(predictions[[h]][,-1])
    
    for (error in seq(ncol(errors[[h]]))) {
      errors[[h]][,error] <- (predictions[[h]][,1] - predictions[[h]][,1+error])
    }
    
  }
  names(step_eemd_pred) <- paste0(horizon,'-step')
  names(metrics_eemd_train) <- names(step_eemd_pred)
  names(metrics_eemd_test) <- names(step_eemd_pred)
  names(predictions) <- names(step_eemd_pred)
  names(errors) <- names(step_eemd_pred)
  
  results <- list(Predictions = predictions,
                  Decomp_Metrics = metrics_eemd_test,
                  Hyperparameters = Params,
                  Var_Importance = Importance,
                  Errors = errors,
                  IMF = IMF_df)
  
  return(results)
}