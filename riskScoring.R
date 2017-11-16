# In cross-validation step k, a lasso model with lambda l is estimated on 9/10 of the data, and one basis function is applied to the coefficients, so instead of XB, you have X(r(B)), where r is a selected from the library of functions that returns an integer vector from a numeric vector, for instance f(x) = floor(bx). Then the resulting integer model is used to make predictions on the remaining 10th and the predictions are returned. 
# 
# You therefore have 2 tuning parameters instead of 1 for the LASSO -- you have lambda, and you have the scalar, call it tau. You can first select the value of lambda that maximizes AUC in a conventional LASSO cross validation model, and then set that to be lambda and use cross validation to estimate tau with lambda held constant. Now, you can separately apply the LASSO and the risk score to the test set. 

roundCoefs <- function(data, indices=seq_along(data[,1]), fit, mult=2, fun=round, eval.type="auc", y) {
  #get AUC or other validation statistic from a give multiple for a given set of indices
  coefs <- if("glmnet" %in% class(fit)) fit$beta else  coef(fit)
  beta_score <- fun(mult*coefs)
  pred <-  as.numeric(data[indices,] %*% beta_score)
  roc_score <- pROC::roc(y[indices], pred, ci=TRUE)
  if(eval.type=="auc") roc_score$auc
  
}


boot.roundCoefs <- function(data, y, fit, mult.seq=seq(.25, 5, .25), fun=round, eval.type="auc",R=1000) {
  #bootstrap interface for the roundCoefs function
  tbl <- sapply(mult.seq, function(i)
      replicate(n = R, expr = roundCoefs(data, indices=sample(1:nrow(data), size=nrow(data), replace = TRUE),   
                                     fit, mult=i, fun=round, eval.type="auc", y))
  )
  colnames(tbl) = mult.seq
  
  dat <- reshape2::melt(tbl) %>%
    group_by(Var2) %>%
    mutate(median=median(value))
  
  res <- list(boots = tbl, max_median = max(dat$median), max_mult = min(dat$Var2[dat$median==max(dat$median)]))
  
}


plot_score_aucs <- function(boot.dat, ...) {
  # view boxplots of range of AUCs for each choice of scalar multiple
  dat <- reshape2::melt(boot.dat$boots) %>%
    group_by(Var2) %>%
    mutate(median=median(value)) %>%
    ungroup() %>% 
    mutate(max_median=median==max(median))
  
  dat %>%
    ggplot(aes(x=factor(Var2), y=value)) +
    geom_boxplot(...) + 
    theme_light() +
    theme(legend.position = "none") +
    labs(y="AUC",x="Mutiple")
}






