###############################################################################
###############################################################################
### NELSON sample size estimator for supervised machine learning problems
###
###
###############################################################################
rm(list = ls())

source('/Users/bennettkleinberg/GitHub/nelson_sample/nelson_sim_utils.R')
source('/Users/bennettkleinberg/GitHub/nelson_sample/nelson_learning_utils.R')



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


p1 = make_pseudo_population(feat_n = 2
                            , population_size = 1e4
                            , feat_dist = 'uniform'
                            , feat_strength_unit = 'prop'
                            , min = 0.01
                            , max = 0.60
                            , seed = 123)


run_ml_model_gen = function(population
                          , n
                          , model = 'rf'
                          , train_prop = 0.80
                          , cv_folds = 5
                          , cv_reps = 5
                          , seed){
  
  #seed = 12
  set.seed(seed)
  if(n == 'baseline'){
    data_ml = sample_from_population(population = population
                                     , n = nrow(population)
                                     , seed = (seed*42))
  } else {
    data_ml = sample_from_population(population = population
                                     , n = n
                                     , seed = (seed*42))
  }
  
  
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
                      , metric="ROC"
                      , trControl = train_ctrl
                      #, preProcess = c('nzv')
  )
  
  pred = predict(train_model, test_data[, -c(1)])
  probs = predict(train_model, test_data[, -c(1)], type='prob')[,1]
  
  confusion_matrix = confusionMatrix(pred, test_data$y)
  
  return(confusion_matrix)
  
}



cmat_1 = run_ml_model_gen(population = p1
                        , n = 'baseline'
                        , train_prop = 0.8
                        , cv_folds = 4
                        , cv_reps = 2
                        , seed = 3)
get_metric_from_confmat(cmat_1)

iter_max = 10
empty_list_meta = list()
steps = seq(50, 1000, 50)
for (i in 1:length(steps)){
  
  print(i)
  
  empty_list_cmats = list()
  for(j in 1:iter_max){
    print(j)
    
    temp_cmat = run_ml_model_gen(population = p1
                                 , n = steps[i]
                                 , train_prop = 0.8
                                 , cv_folds = 4
                                 , cv_reps = 2
                                 , seed = sample(1:1e6, 1))
    
    metric = get_metric_from_confmat(temp_cmat)
    df_ = data.frame(metric = metric
                     , n = steps[i]
                     , iter = j)
    
    empty_list_cmats[[j]] = df_
  }
  
  list_to_df = rbindlist(empty_list_cmats)
  empty_list_meta[[i]] = list_to_df
  
    
}

results_dt = rbindlist(empty_list_meta)
results_dt[, metric_delta := (0.8865 - metric)]
results_mean = results_dt[, mean(metric_delta), n]

plot(V1 ~ n, data = results_mean)

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
# 1. simulate pop DONE
# 2. obtain baseline performance (metric!)  ONCE!
# 3. increase n successively (store cmat)
# 3a. performance improvement: stop once criterion is met
# 4. assessment with corridor (from cmats)


# VERSIONS
## user: simply user-based input
## dev: full control