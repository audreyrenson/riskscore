library(pensim)
library(caret)
library(glmnet)
library(pROC)

# Receive a model object (can be glm or glmnet)
cv.riskScore <- function(dat, k, outcome,
                         tuneGrid=expand.grid(list(fun=c("floor","ceiling","round"), tau=seq(0.25,3.5,0.25)))) {

  #check that data are all categorical
  if(!.allCat(dat)) stop("All columns of data frame must be factors.")

  #create k folds
  list_folds <- .createKfolds(k, dat, outcome)

  #get range of lambdas
  lambdas <- .getDefaultLambdaValues(dat, outcome)

  #get models for all lambda values
  list_list_models <- .getAllModelsForAllLambdas(list_folds, dat, outcome, lambdas)

  #get cross-validation predictinos for all lambdas
  df_preds <- .getPredsForAllLambdas(list_list_models, training=dat, outcome=outcome)

}

## Check that data are all categorical
  .allCat <- function(dat) {
    return(all(sapply(dat, is.factor)))
  }

## create k folds
  .createKfolds <- function(k, training, outcome) {
    return(createFolds(training[[outcome]], k=k, list=TRUE))
  }
## get model for fold
  .getModelForFold <- function(fold, training, outcome, lambda) {
    m_train <- training[-fold,]
    m_test  <- training[fold,]

    y=m_train[[outcome]]
    x=model.matrix(as.formula(paste("~.-", outcome)), data=m_train)
    mod=glmnet(x,y,"binomial",lambda=lambda)
    #add fold attribute to model
    attributes(mod)$fold <- fold
    return(mod)
  }

## get list of models for all folds
  .getFoldModelsForLambda <- function(folds, training, outcome, lambda) {
    list_models <- lapply(folds, .getModelForFold, training=training,outcome=outcome,lambda=lambda)
    return(list_models)
  }


## get predictions for fold model
  .getPredsForFoldModel <- function(mod, training, outcome) {
    fold <- attributes(mod)$fold
    m_test  <- training[fold,]

    preds = predict(mod, newx=model.matrix(as.formula(paste("~.-", outcome)), data=m_test),type="response")

    return(preds)
  }

## get cross-validated predictions for models forall k folds
  .getPredsForAllKModels <- function(list_models, training, outcome) {
    preds = vector("numeric", nrow(training))
    for(mod in list_models) {
      fold <- attributes(mod)$fold
      preds[fold] <- .getPredsForFoldModel(mod, training, outcome)
    }
    return(preds)
  }


## get cross-validated predictions for all lambdas
  .getPredsForAllLambdas <- function(list_list_models, training, outcome) {
    df_lambdas <- as.data.frame(lapply(list_list_models, .getPredsForAllKModels, training=training, outcome=outcome))
    names(df_lambdas) <- names(list_list_models)
    return(df_lambdas)
  }

## get all models for each lambda
  .getAllModelsForAllLambdas <- function(folds, training, outcome, lambdas) {
    list_list_models <- lapply(lambdas, function(l) .getFoldModelsForLambda(folds, training, outcome, l))
    names(list_list_models) <- lambdas
    return(list_list_models)

  }


#   df_aucs_lambdas = data.frame(lambdas, auc=NA)
#
#   for(l in lambdas) {
#     predictions <- .getPredsForAllK(folds, training, outcome, l)
#
#     aucs_lambdas$auc[aucs_lambdas$lambda == l]  <- auc(response=training[[outcome]],
#                                                        predictor=predictions)
#   }
#   return(df_aucs_lambdas)
# }


## get reasonable range of lambda values
.getDefaultLambdaValues <- function(training, outcome) {
  lambda=glmnet(x=model.matrix(as.formula(paste("~.-", outcome)),
                               data=training),
                y=training[[outcome]], family="binomial")$lambda
  return(lambda)

}






## END



dat <- pensim::create.data(response="binary", nsamples=100, nvars=c(20,20,20,20,20))$data

dat_cutpoints <- sapply(names(dat), function(i) rf.cutPoints(as.formula(paste0("outcome~",i)), dat=dat, ntree=100 ))[-ncol(dat)]



for(i in names(dat)[-which(names(dat)=="outcome")]) dat[[i]] <- cut(dat[[i]],
                                     breaks = rf.cutPoints(as.formula(paste0("outcome~",i)), dat=dat, ntree=100 ), include.lowest = TRUE)

inTrain <- createDataPartition(dat$outcome, p=.8, list=FALSE)
training <- dat[inTrain,]
testing  <- dat[-inTrain,]

folds <- createFolds(training$outcome, k=10, list=TRUE)


lambda=glmnet(x=model.matrix(~.-eval(as.name(outcome)), data=training),
              y=training[[outcome]], family="binomial")$lambda



aucs_lambdas = data.frame(lambda, auc=NA)

for(l in lambda) {
  #get AUC for each lambda value
  predictions = vector("numeric", nrow(training))

  for(k in folds) {
    k_train <- training[-k,]
    k_test  <- training[k,]

    y=k_train$outcome
    x=model.matrix(~.-outcome, data=k_train)
    mod=glmnet(x,y,"binomial",lambda=l)
    predictions[k]=predict(mod,
                       newx=model.matrix(~.-outcome, data=k_test),
                       type="response")
  }

  aucs_lambdas$auc[aucs_lambdas$lambda == l]  <- auc(response=training$outcome,
                                                     predictor=predictions)

}

newlambda=aucs_lambdas$lambda[aucs_lambdas$auc == max(aucs_lambdas$auc)]

#run cross validation again with rounding of the model selected by lasso

tau = seq(0.25, 5, 0.25)
aucs_taus = data.frame(tau, auc=NA)

for(tt in tau) {
  #get AUC for each lambda value
  predictions = vector("numeric", nrow(training))

  for(k in folds) {
    k_train <- training[-k,]
    k_test  <- training[k,]

    y=k_train$outcome
    x=model.matrix(~.-outcome, data=k_train)
    mod=glmnet(x,y,"binomial",lambda=newlambda)

    #need to select unique rownames to remove duplicate (intercept)
    beta=coef(mod)[unique(rownames(coef(mod))),]

    rounded_beta = round(beta * tt)
    x_test = model.matrix(~.-outcome, data=k_test)
    predictions[k]= x_test %*% rounded_beta
  }

  aucs_taus$auc[aucs_taus$tau == tt]  <- auc(response=training$outcome,
                                                     predictor=predictions)
}

#get optimal risk score and validate on the test set
mod_opt = glmnet(x=model.matrix(~.-outcome, data=training),
                 y=training$outcome, family="binomial",
                 lambda=newlambda)
tau_opt = aucs_taus$tau[aucs_taus$auc==max(aucs_taus$auc)]
beta_opt = coef(mod_opt)[unique(rownames(coef(mod_opt))),]
beta_round_opt = beta * tau_opt
train_predictions = model.matrix(~.-outcome, data=training) %*% beta_round_opt
train_roc         = roc(training$outcome, exp(train_predictions)/(1+exp(train_predictions)))
test_predictions  = model.matrix(~.-outcome, data=testing) %*% beta_round_opt
test_roc          = roc(testing$outcome, exp(test_predictions)/(1+exp(test_predictions)))
