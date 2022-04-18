#temp file for NELSON dev




run_ml_model_1 = function(population
                          , n
                          , train_prop = 0.80
                          , cv_folds = 5
                          , cv_reps = 5
                          , seed){
  
  seed = 12
  set.seed(seed)
  data_ml = sample_from_population(population = population
                                   , n = n
                                   , seed = (seed*42))
  
  data_ml$y = as.factor(make.names(data_ml$y))
  data_ml = as.data.frame(data_ml)
  
  train_idx = createDataPartition(y = data_ml$y
                                  , p = train_prop
                                  , list = FALSE)
  
  train_data = data_ml[train_idx, ]
  test_data = data_ml[-train_idx,]
  
  train_ctrl = trainControl(method="repeatedcv"
                            , number = cv_folds
                            , repeats = cv_reps
                            , selectionFunction = "oneSE"
                            , classProbs = T
                            , summaryFunction = twoClassSummary
                            , savePredictions = F)
  
  train_model = train(y ~ .
                      , data = train_data
                      , method = "rf"
                      , trControl = train_ctrl
                      #, preProcess = c('nzv')
  )
  
  pred = predict(train_model, test_data[, -c(1)])
  probs = predict(train_model, test_data[, -c(1)], type='prob')[,1]
  
  confusion_matrix = confusionMatrix(pred, test_data$y)
  
  return(confusion_matrix)
  
}
