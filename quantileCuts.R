require(rpart)

qCut <- function(x, probs=seq(0,1,.25), labels=NULL) {
  cut(x, breaks=quantile(x, probs=probs, na.rm=TRUE), labels=labels)
}

getOptimalCuts <- function(..., cp=0.01) {
  tr <- rpart::prune(rpart::rpart(...), cp=cp)
  cuts <- c(0, tr$splits[,"index"], Inf)
  cuts
}

getOptimalCuts.boot <- function(formula, data, type="b", cp=0.01, R=1000) {
  cuts <- function(formula, data, indices, cp=cp) {
    d <- data[indices,]
    tr <- rpart::prune(rpart::rpart(formula, data=d), cp=cp)
    cuts <- c(0, tr$splits[,"index"], Inf)
    cuts
  }
  if(type=="h") hist(boot::boot(data, cuts, R=R, formula=formula)) else {
    d <- data[sample(1:nrow(data), size=nrow(data), replace=TRUE),]
    cuts(formula=formula, data=d, indices=, cp=cp)
  }
}

boot.cutpoints <- function(data, formula, R=500, cp=0.02) {
  cuts <- function(formula, data, indices, cp=cp) {
    d <- data[indices,]
    tr <- rpart::prune(rpart::rpart(formula, data=d), cp=cp)
    cuts <- c(0, tr$splits[,"index"], Inf)
    cuts
  }
  boot.cuts <- function(data,formula) {
    d <- data[sample(1:nrow(data), size=nrow(data), replace=TRUE),]
    cuts(formula=formula, data=d, indices=, cp=cp)
  }
  boots <- as.list(replicate(R, boot.cuts(data, formula)))
  lengths <- sapply(boots, length)
  do.call(rbind, boots[lengths==median(lengths)])
}
boot.cutpoints <- function(data, formula, R=500, cp=0.02, fun=mean) {
     cuts <- function(formula, data, indices, cp=cp) {
         d <- data[indices,]
         tr <- rpart::prune(rpart::rpart(formula, data=d), cp=cp)
         cuts <- c(0, tr$splits[,"index"], Inf)
         cuts
       }
     boot.cuts <- function(data,formula) {
         d <- data[sample(1:nrow(data), size=nrow(data), replace=TRUE),]
         cuts(formula=formula, data=d, indices=, cp=cp)
       }
     boots <- replicate(R, boot.cuts(data, formula))
     lengths <- sapply(boots, length)
     cutpoint_mtrx <- if(class(boots)=="matrix") t(boots) else do.call(rbind, boots[lengths==median(lengths)])
     apply(cutpoint_mtrx, 2, fun)
}


rf.cutPoints <- function(formula, data, ntree) {
  require(randomForest)

  rf <- randomForest(formula = formula, data = data, ntree = ntree, mtry = 1)

  return(c(-Inf, mean(sapply(1:ntree, function(tr) getTree(rf, k = tr)[1,"split point"])), Inf))
}


