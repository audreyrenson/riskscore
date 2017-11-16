dat <- pensim::create.data(response="binary", nsamples=1000, nvars=c(20,20,20,20,20))$data
source("quantileCuts.R")
for(i in names(dat)[-which(names(dat)=="outcome")]) dat[[i]] <- cut(dat[[i]],
                                                                    breaks = rf.cutPoints(as.formula(paste0("outcome~",i)), dat=dat, ntree=100 ), include.lowest = TRUE)

levels(dat$outcome) <- make.names(levels(dat$outcome))

library(caret)
tc <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary,
                   method = 'cv', 
                   number = 10)

mod1 <- train(form=outcome ~ ., data=dat, method="glmnet", trControl=tc)

best <- mod1$bestTune$lambda

training <- dat

tau = seq(0.25, 5, 0.25)
aucs_taus = data.frame(tau, auc=NA)
folds <- createFolds(training$outcome, k=10, list=TRUE)
coefs <- coef(mod1$finalModel, mod1$bestTune$lambda)


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
