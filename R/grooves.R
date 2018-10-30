#' Internal function to plot crosscut + grooves
#'
#' @param land data.frame with columns x and value
#' @param grooves numeric vector of length 2 identifying both grooves.
#'        If only one groove is identified, the other should be NA
#' @import assertthat
#' @import ggplot2
#' @return a ggplot2 object
grooves_plot <- function(land, grooves) {
  assert_that(has_name(land, "x"))
  assert_that(has_name(land, "value"))
  assert_that(is.numeric(grooves), sum(!is.na(grooves)) >= 1)
  x <- value <- NULL

  ggplot(aes(x = x, y = value), data = land) + geom_line(size = .5) +
    theme_bw() +
    geom_vline(xintercept = grooves[1], colour = "blue") +
    geom_vline(xintercept = grooves[2], colour = "blue")
}


#' Find the grooves of a bullet land
#'
#' @param ccdata data frame of the crosscut. Data frame needs location x and
#'          measured values as `value`. If multiple crosscuts are to be
#'          considered, include a variable y and use as a key.
#' @param method method to use for identifying grooves. Defaults to "rollapply"
#' @param smoothfactor The smoothing window to use - XXX the smoothing window
#'          seems to depend on the resolution at which the data has been
#'          collected.
#' @param adjust positive number to adjust the grooves - XXX should be
#'          expressed in microns rather than an index
#'          (not used for method = "middle")
#' @param groove_cutoff The index at which a groove cannot exist past - XXX
#'          this parameter should be expressed in microns rather than as an
#'          index to be able to properly deal with different resolutions
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param return_plot Return plot of grooves?
#' @param ... parameters passed on to specific groove location methods
#' @export
#' @import assertthat
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "rollapply",
#'   adjust = 30, return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "middle",
#'   adjust = 30, return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "quadratic",
#'   adjust = 30, return_plot = T
#' )
#' }
cc_locate_grooves <- function(ccdata, method = "rollapply", smoothfactor = 15,
                              adjust = 10, groove_cutoff = 400,
                              mean_left = NULL, mean_right = NULL,
                              mean_window = 100,
                              return_plot = F, ...) {
  # TODO add documentation for different groove options, either here or by exporting the individual functions and documenting the parameters there.

  x <- y <- value <- NULL
  land <- ccdata
  if (is.null(ccdata) || nrow(ccdata) == 0) return(NULL)

  check_ccdata(ccdata)

  assert_that(method %in% c("quadratic", "rollapply", "middle", "logisticlegacy", "lassobasic", "lassofull")) ## too strict
  # TODO: expand cc_locate_groove to accept user defined get_grooves_XXX function
  assert_that(
    is.numeric(smoothfactor), is.numeric(adjust), is.numeric(groove_cutoff),
    is.logical(return_plot)
  )

  if (method == "logisticlegacy") {
    grooves <- get_grooves_logisticlegacy(
      x = land$x, value = land$value,
      adjust = adjust, return_plot = return_plot
    )
  }
  if (method == "lassobasic") {
    grooves <- get_grooves_lasso(
      x = land$x, value = land$value, lasso_method = 'basic',
      return_plot = return_plot, ...
    )
  }
  if (method == "lassofull") {
    grooves <- get_grooves_lasso(
      x = land$x, value = land$value, lasso_method = 'full',
      return_plot = return_plot, ...
    )
  }
  if (method == "quadratic") {
    # grooves <- get_grooves_quadratic(bullet = bullet, adjust = adjust)
    grooves <- get_grooves_quadratic(
      x = land$x, value = land$value,
      adjust = adjust, return_plot = return_plot
    )
  }
  if (method == "rollapply") {
    # make sure there is only one x
    if (length(unique(land$y)) > 1) {
      message(sprintf(
        "summarizing %d profiles by averaging across values\n",
        length(unique(land$x))
      ))
      land <- land %>% group_by(x) %>% summarize(
        y = mean(y, na.rm = TRUE),
        value = mean(value, na.rm = TRUE)
      )
    }
    grooves <- get_grooves_rollapply(
      x = land$x,
      value = land$value,
      smoothfactor = smoothfactor,
      adjust = adjust,
      groove_cutoff = groove_cutoff,
      mean_left = mean_left,
      mean_right = mean_right,
      mean_window = mean_window,
      second_smooth = TRUE,
      return_plot = return_plot,
      ...
    )
  }
  if (method == "middle") {
    middle <- 75
    if ("middle" %in% names(list(...))) {
      middle <- list(...)$middle
    }
    grooves <- get_grooves_middle(
      x = land$x, value = land$value,
      middle = middle,
      return_plot = return_plot
    )
  }

  return(grooves)
}


#' Use the center of a crosscut
#'
#' @param x numeric vector of locations in microns
#' @param value numeric vector of surface measurements in microns
#' @param middle middle percent to use for the identification
#' @param return_plot return plot?
#' @return list of groove vector and plot of crosscut, if return_plot is true
#' @import assertthat
#' @export
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' get_grooves_middle(example_data$ccdata[[1]]$x,
#'   example_data$ccdata[[1]]$value,
#'   return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "middle",
#'   return_plot = T
#' )
#' }
get_grooves_middle <- function(x, value, middle = 75, return_plot = F) {
  assert_that(
    is.numeric(x), is.numeric(value), is.numeric(middle),
    is.logical(return_plot)
  )
  assert_that(middle >= 0, middle <= 100)

  land <- data.frame(x = x, value = value)
  # summarize values for each x:
  ns <- land %>% count(x)
  if (max(ns$n) > 1) message(sprintf("summarizing across %d profiles ...", max(ns$n)))

  land <- land %>% group_by(x) %>% summarize(value = mean(value, na.rm = TRUE))
  groove <- quantile(land$x,
    probs = c((100 - middle) / 200, (100 + middle) / 200)
  )

  if (return_plot) {
    return(list(
      groove = groove,
      plot = grooves_plot(land = land, grooves = groove)
    ))
  } else {
    return(list(groove = groove))
  }
}

#' Use logistic model to identify groove locations
#'
#' @inheritParams get_grooves_quadratic
#' @importFrom locfit locfit.robust
#' @importFrom locfit locfit
#' @importFrom stats model.matrix
#' @export
get_grooves_logisticlegacy <- function(x, value, adjust = 10, # smoothfactor = 15,
                                       # groove_cutoff = 400,
                                       return_plot = F) {
  land <- data.frame(x = x, value = value)
  original_land <- land

  ## generate additional variables

  check_min <- min(land$value[!is.na(land$value)])
  land <- mutate(land, value_std = value - check_min)
  # install.packages("locfit")
  # library(locfit)
  robust_loess_fit <- locfit.robust(value_std ~ x, data = land, alpha = 1, kern = "tcub")
  land$rlo_pred <- predict(robust_loess_fit, newdata = land)

  land$rlo_absresid <- with(land, abs(value_std - rlo_pred))
  land$rlo_resid <- with(land, value_std - rlo_pred)


  median <- median(land$x)
  land$side <- "right"
  land$side <- ifelse(land$x <= median, "left", land$side)
  land$depth <- abs(land$x - median)

  ## range20 : range of values in a 20-wide band around each data point.
  land$range_50 <- rollapply(land$rlo_resid, width = 50, FUN = function(x) {
    max(x) - min(x)
  }, partial = TRUE)

  ## xint1 and xint2: the predicted locations that the robust LOESS crosses the x-axis.
  xint1 <- min(abs(land$rlo_pred[(land$x < median(land$x))]))
  xint2 <- min(abs(land$rlo_pred[(land$x > median(land$x))]))
  ind1 <- which(land$rlo_pred == xint1 | land$rlo_pred == -1 * xint1)
  ind2 <- which(land$rlo_pred == xint2 | land$rlo_pred == -1 * xint2)
  land$xint1 <- land$x[ind1]
  land$xint2 <- land$x[ind2]

  ## ind_2mad: whether the data point is above the 2*MAR cutoff previously used as an ad-hoc method.
  mar <- median(land$rlo_absresid, na.rm = T)
  land$ind_2mad <- ifelse(land$rlo_absresid > 2 * mar, 1, 0)

  ## numpos_50: how many positive residuals there are in a 50-wide band around each data point.
  land$numpos_50 <- rollapply(land$rlo_resid, width = 50, FUN = function(x) {
    sum(x > 0)
  }, partial = TRUE)

  land$numNA_50 <- rollapply(land$rlo_resid, width = 50, FUN = function(x) {
    sum(is.na(x))
  }, partial = TRUE)
  lower <- quantile(land$x, prob = .25)
  upper <- quantile(land$x, prob = .75)
  proxy_dat <- land %>% filter(x < upper & x > lower)
  proxy <- sd(proxy_dat$rlo_resid, na.rm = T)
  land$rlo_resid_std <- land$rlo_resid / proxy
  land$range_50_std <- land$range_50 / proxy

  xrange <- max(land$x) - min(land$x)
  land$depth_std <- land$depth / xrange
  land$xint1_std <- land$xint1 / xrange
  land$xint2_std <- land$xint2 / xrange

  ## now get logistic predictions
  model_all4 <- as.matrix(c(-26.7166509, 0.1727030, 0, -0.1815079, 0, 39.7340095, -1.0473396, 7.0916175, 0.2428548, 0, 1.6039295, 0, 0))


  land <- na.omit(land)
  X <- cbind(1, model.matrix(
    ~rlo_resid_std + I(rlo_resid_std^2) + side +
      depth_std + side * depth_std + xint1_std +
      xint2_std + range_50 + numNA_50 + ind_2mad +
      numpos_50 - 1,
    land
  ))
  ymean <- X %*% model_all4
  yhat <- exp(ymean) / (1 + exp(ymean))
  land$pred_val <- yhat
  land$pred_class <- ifelse(land$pred_val < .25, "LEA", "GEA")

  groove <- range(land$x[land$pred_class == "LEA"])

  if (return_plot) {
    return(list(
      groove = groove,
      plot = grooves_plot(land = original_land, grooves = groove)
    ))
  } else {
    return(list(groove = groove))
  }
}




#' Calculate predicted values and residuals
#'
#' @param cc crosscut data, including columns value_std and x
#' @param fit fitted loess object from robust_loess_fit
#' @importFrom stats predict
#' @importFrom assertthat assert_that
#' @importFrom assertthat has_name
resid_calc <- function(cc, fit) {
  assert_that(has_name(cc, "x"), has_name(cc, "value_std"))
  cc$rlo_pred <- predict(fit, newdata = cc)
  cc$rlo_resid <- cc$value_std - cc$rlo_pred
  cc$rlo_absresid <- abs(cc$rlo_resid)
  return(cc)
}


#' Fit a robust loess regression
#'
#' Internal function called by get_grooves_lassobasic and get_grooves_lassofull
#' @param cc data frame with columns x and value_std, representing the crosscut
#' @param iter number of iterations
#' @importFrom stats loess
#' @importFrom assertthat assert_that
#' @importFrom assertthat has_name
robust_loess_fit <- function(cc, iter) {
  assert_that(has_name(cc, "x"), has_name(cc, "value_std"))
  n <- nrow(cc)
  weights <- rep(1, n)
  fit <- loess(value_std ~ x, data = cc, span = 1)
  cc$fit <- predict(fit, newdata = cc)
  cc$resid <- cc$value_std - cc$fit
  i <- 1
  while (i < iter) {
    mar <- median(abs(cc$resid), na.rm = T)
    cc$bisq <- pmax(1 - (cc$resid / (6 * mar))^2, 0)^2
    weights <- ifelse(cc$resid > 0, cc$bisq, 1)
    fit <- loess(value_std ~ x, data = cc, span = 1, weights = weights)
    cc$fit <- predict(fit, newdata = cc)
    cc$resid <- cc$value_std - cc$fit
    i <- i + 1
  }
  return(fit)
}

#' Use logistic model to identify groove locations
#'
#' @param x numeric vector of locations (in microns)
#' @param value numeric values of surface measurements in microns
#' @param lasso_method use the 'basic' model or the 'full' model with interaction terms?
#' @param pred_cutoff equal error rate cutoff for classification into GEA or LEA, trained on Hamby set 44
#' @param return_plot return plot of grooves?
#' @importFrom locfit locfit.robust
#' @importFrom locfit locfit
#' @importFrom stats model.matrix
#' @importFrom assertthat assert_that
#' @importFrom assertthat has_name
#' @export
get_grooves_lasso <- function(x, value, lasso_method = 'basic', pred_cutoff = ifelse(lasso_method == 'basic', .07, 0.06), return_plot = F) {
  land <- data.frame(x = x, value = value)
  original_land <- land

  assert_that(has_name(land, "x"), has_name(land, "value"),
              is.numeric(land$x), is.numeric(land$value))

  assert_that(lasso_method %in% c("basic", "full"))
  assert_that(pred_cutoff > 0, pred_cutoff < 1)

  ## generate additional variables
  check_min <- min(land$value[!is.na(land$value)])
  land <- land %>% mutate(value_std = value - check_min)

  ## fit robust loess, calculate residuals
  rlo_fit <- robust_loess_fit(cc = land, iter = 20)
  land <- resid_calc(cc = land, fit = rlo_fit)

  mx <- max(land$x, na.rm = T)
  diff_mx <- mx / 2 - land$x
  ## Use this method because some data may have shifted x values
  median <- land$x[which.min(abs(diff_mx))] # changed abs(tst_mx) to abs(diff_mx) - tst_mx not defined
  #median <- median(land$x) # some of the houston data appears to have a shifted x
  land$side <- "right"
  land$side <- ifelse(land$x <= median, "left", land$side)
  land$depth <- abs(land$x - median)

  ## range50 : range of values in a 50-wide band around each data point.
  land$range_50 <- rollapply(land$rlo_resid, width = 50, FUN = function(x) {
    max(x) - min(x)
  }, partial = TRUE)

  ## xint1 and xint2: the predicted locations that the robust LOESS crosses the x-axis.
  xint1 <- min(abs(land$rlo_pred[(land$x < median(land$x))]), na.rm = T)
  xint2 <- min(abs(land$rlo_pred[(land$x > median(land$x))]), na.rm = T)
  ind1 <- which(land$rlo_pred == xint1 | land$rlo_pred == -1 * xint1)
  ind2 <- which(land$rlo_pred == xint2 | land$rlo_pred == -1 * xint2)
  land$xint1 <- land$x[ind1]
  land$xint2 <- land$x[ind2]

  land$ind_edges <- ifelse(land$x < land$xint1 | land$x > land$xint2, 1, 0)

  ## ind_2mad: whether the data point is above the 2*MAR cutoff previously used as an ad-hoc method.
  mad <- mad(land$rlo_resid, na.rm = T)
  land$ind_2mad <- ifelse(land$rlo_resid > 2 * mad, 1, 0)

  ## numpos_50: how many positive residuals there are in a 50-wide band around each data point.
  land$numpos_50 <- rollapply(land$rlo_resid, width = 50, FUN = function(x) {
    sum(x > 0)
  }, partial = TRUE)

  land$numNA_50 <- rollapply(land$rlo_resid, width = 50, FUN = function(x) {
    sum(is.na(x))
  }, partial = TRUE)
  lower <- quantile(land$x, prob = .25)
  upper <- quantile(land$x, prob = .75)
  proxy_dat <- land %>% filter(x < upper & x > lower)
  proxy <- sd(proxy_dat$rlo_resid, na.rm = T)
  land$rlo_resid_std <- land$rlo_resid / proxy
  land$range_50_std <- land$range_50 / proxy

  xrange <- max(land$x) #- min(land$x) # again correcting for shifted houston persistence data
  land$depth_std <- land$depth / xrange
  land$xint1_std <- land$xint1 / xrange
  land$xint2_std <- land$xint2 / xrange

  ## now get logistic predictions
  land <- na.omit(land)

  if (lasso_method == 'basic') {
    X <- cbind(1, model.matrix(
      ~rlo_resid_std + I(rlo_resid_std^2) + side +
        depth_std + side * depth_std + xint1_std +
        xint2_std + range_50 + numNA_50 + ind_2mad +
        numpos_50 + ind_edges - 1,
      land
    ))
    ymean <- X %*% lasso_simple
  } else if (lasso_method == 'full') {
    X <- cbind(1, model.matrix(
      ~(rlo_resid_std + I(rlo_resid_std^2) + side +
          depth_std + xint1_std +
          xint2_std + range_50 + numNA_50 + ind_2mad +
          numpos_50 + ind_edges)^2 - 1,
      land
    ))
    ymean <- X %*% lasso_interactions
  } else {
    stop("invalid lasso_method argument.")
  }

  yhat <- as.vector(exp(ymean) / (1 + exp(ymean)))
  land$pred_val <- yhat
  land$pred_class <- ifelse(land$pred_val < pred_cutoff, "LEA", "GEA")

  groove <- range(land$x[land$pred_class == "LEA"])

  if (return_plot) {
    return(list(
      groove = groove,
      plot = grooves_plot(land = original_land, grooves = groove)
    ))
  } else {
    return(list(groove = groove))
  }
}



#' Quadratic fit to find groove locations
#'
#' Use a robust fit of a quadratic curve to find groove locations
#' @param x numeric vector of locations (in microns)
#' @param value numeric values of surface measurements in microns
#' @param adjust positive number to adjust the grooves
#' @param return_plot return plot of grooves?
#' @return list of groove vector and plot of crosscut with shoulder locations
#' @importFrom MASS rlm
#' @export
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' get_grooves_quadratic(example_data$ccdata[[1]]$x,
#'   example_data$ccdata[[1]]$value,
#'   adjust = 30, return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "quadratic",
#'   adjust = 30, return_plot = T
#' )
#' }
get_grooves_quadratic <- function(x, value, adjust, return_plot = F) {
  assert_that(
    is.numeric(x), is.numeric(value), is.numeric(adjust),
    is.logical(return_plot)
  )

  land <- data.frame(x = x, value = value)

  lm0 <- rlm(value ~ poly(x, 2), data = land, maxit = 100)
  land$pred <- predict(lm0, newdata = land)

  land$absresid <- with(land, abs(value - pred))
  absresid90 <- NULL
  land$absresid90 <- with(
    land, absresid > 4 * median(land$absresid, na.rm = TRUE)
  )

  groove <- range(filter(land, !absresid90)$x) + c(adjust, -adjust)

  if (return_plot) {
    return(list(
      groove = groove,
      plot = grooves_plot(land = land, grooves = groove)
    ))
  } else {
    return(list(groove = groove))
  }
}

#' Using rollapply to find grooves in a crosscut
#'
#' @param x numeric vector of locations (in microns)
#' @param value numeric values of surface measurements in microns
#' @param smoothfactor The smoothing window to use
#' @param adjust positive number to adjust the grooves
#' @param groove_cutoff The index at which a groove cannot exist past
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param second_smooth Whether or not to smooth a second time
#' @param which_fun Which function to use in the rollapply statement
#' @param return_plot return plot of grooves?
#' @export
#' @importFrom assertthat assert_that
#' @importFrom zoo rollapply na.fill
#' @importFrom utils head tail
#' @examples
#' \dontrun{
#' # Set the data up to be read in, cleaned, etc.
#' library(bulletxtrctr)
#' library(x3ptools)
#'
#' example_data <- bullet_pipeline(
#'   location = list(Bullet1 = c(hamby252demo$bullet1[3])),
#'   stop_at_step = "crosscut",
#'   x3p_clean = function(x) x %>%
#'       x3pheader_to_microns() %>%
#'       rotate_x3p(angle = -90) %>%
#'       y_flip_x3p()
#' )
#'
#' get_grooves_rollapply(example_data$ccdata[[1]]$x,
#'   example_data$ccdata[[1]]$value,
#'   adjust = 30, return_plot = T
#' )
#' cc_locate_grooves(example_data$ccdata[[1]],
#'   method = "rollapply",
#'   adjust = 30, return_plot = T
#' )
#' }
get_grooves_rollapply <- function(x, value, smoothfactor = 15, adjust = 10,
                                  groove_cutoff = 400, mean_left = NULL,
                                  mean_right = NULL, mean_window = 100,
                                  second_smooth = T, which_fun = mean,
                                  return_plot = F) {
  . <- NULL

  assert_that(
    is.numeric(x), is.numeric(value), is.numeric(adjust),
    is.numeric(smoothfactor), is.numeric(groove_cutoff),
    is.logical(second_smooth), is.logical(return_plot),
    "function" %in% class(which_fun)
  )

  land <- data.frame(x = x, value = value)
  original_land <- land

  if (!is.null(mean_left) && !is.null(mean_right)) {
    mean.left.ind <- which.min(abs(land$x - mean_left))
    mean.right.ind <- which.min(abs(land$x - mean_right))

    window.left.left <- max(1, mean.left.ind - mean_window)
    window.left.right <- mean.left.ind + mean_window

    window.right.left <- mean.right.ind - mean_window
    window.right.right <- min(length(land$x), mean.right.ind + mean_window)

    land <- land[c(
      window.left.left:window.left.right,
      window.right.left:window.right.right
    ), ]

    groove_cutoff <- Inf
  }

  value_filled <- zoo::na.fill(land$value, "extend")
  smoothed <- rollapply(value_filled, smoothfactor, function(x) which_fun(x))

  # Add in an if statement, to only do the first smoothing if the second_smooth
  # parameter is equal to FALSE
  if (second_smooth) {
    smoothed_truefalse <- rollapply(smoothed, smoothfactor,
      function(x) which_fun(x),
      partial = TRUE
    )
  } else {
    smoothed_truefalse <- smoothed
  }

  lengthdiff <- length(land$value) - length(smoothed_truefalse)

  peak_ind_smoothed <- rollapply(
    smoothed_truefalse, 3,
    function(x) which.max(x) == 2
  ) %>%
    which() %>%
    head(n = 1)
  peak_ind <- peak_ind_smoothed + floor(lengthdiff / 2)
  if (length(peak_ind) == 0) {
    groove_ind <- peak_ind
  } else {
    groove_ind <- tail(smoothed_truefalse, n = -peak_ind_smoothed) %>%
      rollapply(., 3, function(x) which.min(x) == 2) %>%
      which() %>%
      head(n = 1)
    groove_ind <- groove_ind + peak_ind
  }

  peak_ind2_smoothed_temp <- smoothed_truefalse %>%
    rev() %>%
    rollapply(., 3, function(x) which.max(x) == 2) %>%
    which() %>%
    head(n = 1)

  peak_ind2_temp <- peak_ind2_smoothed_temp + floor(lengthdiff / 2)
  if (length(peak_ind2_temp) == 0) {
    groove_ind2_temp <- peak_ind2_temp
  } else {
    groove_ind2_temp <- rev(smoothed_truefalse) %>%
      tail(., n = -peak_ind2_smoothed_temp) %>%
      rollapply(., 3, function(x) which.min(x) == 2) %>%
      which() %>%
      head(n = 1)
    groove_ind2_temp <- groove_ind2_temp + peak_ind2_temp
  }

  # peak_ind2 <- length(land$value) - peak_ind2_temp + 1
  groove_ind2 <- length(land$value) - groove_ind2_temp + 1

  ## Check that it actually FOUND a groove...
  if (length(groove_ind) == 0 || groove_ind > groove_cutoff) {
    groove_ind <- 1
  }
  if (length(groove_ind2) == 0 ||
    groove_ind2 < length(land$value) - groove_cutoff) {
    groove_ind2 <- length(land$value)
  }

  xvals <- original_land$x
  # yvals <- original_land$value

  # plot_peak_ind <- which(original_land$x == land$x[peak_ind])
  plot_groove_ind <- which(original_land$x == land$x[groove_ind])
  # plot_peak_ind2 <- which(original_land$x == land$x[peak_ind2])
  plot_groove_ind2 <- which(original_land$x == land$x[groove_ind2])

  center <- which.min(abs(xvals - mean(xvals)))

  # I can't figure out how to test this if statement...
  if (plot_groove_ind > center) {
    plot_groove_ind2 <- plot_groove_ind
    plot_groove_ind <- 0
  }

  if (plot_groove_ind2 < center) {
    plot_groove_ind <- plot_groove_ind2
    plot_groove_ind2 <- length(xvals)
  }

  # smoothed_diff <- floor(lengthdiff/2)

  groove <- c(
    original_land$x[plot_groove_ind + adjust],
    original_land$x[plot_groove_ind2 - adjust]
  )

  if (return_plot) {
    return(list(
      groove = groove,
      plot = grooves_plot(land = original_land, grooves = groove)
    ))
  } else {
    return(list(groove = groove))
  }
}

#' Check grooves for correctness
#'
#' @param x output from cc_locate_grooves
#' @return TRUE if ok, error otherwise
#' @importFrom assertthat assert_that
check_grooves <- function(x) {
  assert_that(has_name(x, "groove"))
  assert_that(is.numeric(x$groove))
  assert_that(length(x$groove) <= 2)
  return(TRUE)
}
