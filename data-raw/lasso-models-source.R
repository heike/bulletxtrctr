## this code isn't meant to actually run, the data is not included in the package.
## It is here for reference as to how the model objects lasso_simple and lasso_interactions were generated.

#library(tidyr)
#library(plyr)
#library(dplyr)
#library(purrr)
#library(ggplot2)
#library(zoo)
#library(glmnet)
#library(caret)
#library(e1071)


#hamby44 <- hamby44 %>% mutate(ccdata_w_resid = purrr::map(ccdata_w_resid, .f = function(bullet){
#  mx <- max(bullet$x, na.rm = T)
#  diff_mx <- mx/2 - bullet$x
#  median <- bullet$x[which.min(abs(tst_mx))]
#  #median <- median(bullet$x)
#  bullet$side <- "right"
#  bullet$side <- ifelse(bullet$x <= median, "left", bullet$side)
#  bullet$depth <- abs(bullet$x - median)
#  return(bullet)
#}))

#calculate_response <- function(dataset){
#  for(i in 1:nrow(dataset)){
#    left_groove <- dataset$left_groove[i]
#    right_groove <- dataset$right_groove[i]
#    dataset$ccdata_w_resid[[i]]$left_groove <- left_groove
#    dataset$ccdata_w_resid[[i]]$right_groove <- right_groove
#  }
#  return(dataset)
#}

#hamby44 <- calculate_response(hamby44)

#hamby44 <- hamby44 %>% mutate(ccdata_w_resid = purrr::map(ccdata_w_resid, .f = function(bullet){
#  bullet$response <- ifelse(bullet$x <= bullet$left_groove | bullet$x >= bullet$right_groove, 1, 0)
#  return(bullet)
#}))

#hamby44 <- hamby44 %>% mutate(ccdata_w_resid = purrr::map(ccdata_w_resid, .f = function(bullet){
#  ## range20 : range of values in a 20-wide band around each data point.
#  bullet$range_50 <- rollapply(bullet$rlo_resid, width = 50, FUN = function(x){max(x) - min(x)}, partial = TRUE)

  ## xint1 and xint2: the predicted locations that the robust LOESS crosses the x-axis.
#  xint1 <- min(abs(bullet$rlo_pred[(bullet$x < median(bullet$x))]), na.rm = T)
#  xint2 <- min(abs(bullet$rlo_pred[(bullet$x > median(bullet$x))]), na.rm = T)
#  ind1 <- which(bullet$rlo_pred == xint1 | bullet$rlo_pred == -1*xint1)
#  ind2 <- which(bullet$rlo_pred == xint2 | bullet$rlo_pred == -1*xint2)
#  bullet$xint1 <- bullet$x[ind1]
#  bullet$xint2 <- bullet$x[ind2]

#  bullet$ind_edges <- ifelse(bullet$x < bullet$xint1 | bullet$x > bullet$xint2, 1, 0)

  ## ind_2mar: whether the data point is above the 2*MAR cutoff previously used as an ad-hoc cutoff method.
#  mad <- mad(bullet$rlo_resid, na.rm = T)
#  bullet$ind_2mad <- ifelse(bullet$rlo_resid > 2*mad, 1, 0)

  ## numpos_50: how many positive residuals there are in a 50-wide band around each data point.
#  bullet$numpos_50 <- rollapply(bullet$rlo_resid, width = 50, FUN = function(x){sum(x > 0)}, partial = TRUE)

#  bullet$numNA_50 <- rollapply(bullet$rlo_resid, width = 50, FUN = function(x){sum(is.na(x))}, partial = TRUE)

#  return(bullet)
#}))

#hamby44 <- hamby44 %>% mutate(ccdata_w_resid = purrr::map(ccdata_w_resid, .f = function(bullet){
  #proxy <- mad(bullet$rlo_resid, na.rm = T)
#  lower <- quantile(bullet$x, prob = .25)
#  upper <- quantile(bullet$x, prob = .75)
#  proxy_dat <- bullet %>% filter(x < upper & x > lower)
#  proxy <- sd(proxy_dat$rlo_resid, na.rm = T)
#  bullet$rlo_resid_std <- bullet$rlo_resid/proxy
#  bullet$range_50_std <- bullet$range_50/proxy
#
#  xrange <- max(bullet$x) #- min(bullet$x)
#  bullet$depth_std <- bullet$depth/xrange
#  bullet$xint1_std <- bullet$xint1/xrange
#  bullet$xint2_std <- bullet$xint2/xrange
#  return(bullet)
#}))



## LASSO_SIMPLE MODEL:
#hamby44_model <- rbind.fill(hamby44$ccdata_w_resid)
#hamby44_model <- na.omit(hamby44_model)


#X <- model.matrix(response~rlo_resid_std + I(rlo_resid_std^2) + side +
#                    depth_std + side*depth_std + xint1_std +
#                    xint2_std + range_50_std + numNA_50 + ind_2mad +
#                    numpos_50 + ind_edges - 1,
#                  hamby44_model)

#fit <- cv.glmnet(x = X, y = hamby44_model$response, family = 'binomial', type.measure = 'class', alpha = 1)
#lasso_simple <- as.matrix(coef(fit))


## LASSO_INTERACTIONS MODEL:
#hamby44_model <- rbind.fill(hamby44$ccdata_w_resid)
#hamby44_model <- na.omit(hamby44_model)


#X <- model.matrix(response~(rlo_resid_std + I(rlo_resid_std^2) + side +
#                              depth_std + xint1_std +
#                              xint2_std + range_50 + numNA_50 + ind_2mad +
#                              numpos_50 + ind_edges)^2 - 1,
#                  hamby44_model)

#fit_interactions <- cv.glmnet(x = X, y = hamby44_model$response, family = 'binomial', type.measure = 'class', alpha = 1)
#lasso_interactions <- as.matrix(coef(fit_interactions))



devtools::use_data(lasso_simple, lasso_interactions, internal = TRUE)


