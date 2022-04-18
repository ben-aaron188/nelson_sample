###############################################################################
###############################################################################
### NELSON sample size estimator for supervised machine learning problems
###
###
###############################################################################
rm(list = ls())

source('/Users/bennettkleinberg/GitHub/nelson_sample/nelson_utils.R')



#TODO:
# feature strength via log-odds reversion DONE
# feature strength prior information DONE
# print output statement
# evaluate simulation-to-empirical match DONE
# add slots to class with meta var
# add weight to small values in sampling (lognormal?)

###############################################################################
###############################################################################
###############################################################################




# ML model 1
require(caret)

run_ml_model_gen = function(population
                          , n
                          , model = 'rf'
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
                      , method = model
                      , trControl = train_ctrl
                      #, preProcess = c('nzv')
  )
  
  pred = predict(train_model, test_data[, -c(1)])
  probs = predict(train_model, test_data[, -c(1)], type='prob')[,1]
  
  confusion_matrix = confusionMatrix(pred, test_data$y)
  
  return(confusion_matrix)
  
}




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

cmat_1 = run_ml_model_1(population = p1
                        , n = 800
                        , train_prop = 0.8
                        , cv_folds = 4
                        , cv_reps = 2
                        , seed = 3)

# retrieve values from confusion-matrix
get_metric_from_confmat = function(cmat
                                   , metric = 'acc'){
  
  if(metric == 'acc'){
    m_out = cmat$overall[1]
  } else if(metric){
    #...
  }
  
  return(unname(m_out))
  
}

get_metric_from_confmat(cmat_1)



# Sample from pseudo population, run ML model





# META FUNCTION
## params
### - nfeatures
### - strengths: weak, moderate, strong
### - distr.: uniform, normal
### model
### balance
### split
### metric

##### STEPS within
# 1. simulate pop
# 2. obtain baseline performance (metric!)
# 3. downsample
# 4. assessment with corridor


# VERSIONS
## user: simply user-based input
## dev: full control