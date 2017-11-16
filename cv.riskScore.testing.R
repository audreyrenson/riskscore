#testing
dat <- pensim::create.data(response="binary", nsamples=100, nvars=c(20,20,20,20,20))$data
for(i in names(dat)[-which(names(dat)=="outcome")]) dat[[i]] <- cut(dat[[i]],
                                                                    breaks = rf.cutPoints(as.formula(paste0("outcome~",i)), dat=dat, ntree=100 ), include.lowest = TRUE)

outcome="outcome"
k = 10

.allCat(dat)
list_folds <- .createKfolds(k, dat, outcome)
lambdas <- .getDefaultLambdaValues(dat, outcome)
list_list_models <- .getAllModelsForAllLambdas(list_folds, dat, outcome, lambdas)

df_preds <- .getPredsForAllLambdas(list_list_models, training=dat, outcome=outcome)
boxplot(df_preds)
