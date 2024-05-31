#' Bullet Phase test
#'
#' Determine a p-value for the strength of similarity between two bullets based on scores for each land-to-land comparison.
#' @param land1 vector with land ids of the LEAs in bullet 1
#' @param land2 vector with land ids of the LEAs in bullet 2
#' @param score real-valued vector consisting of land-to-land comparisons. Higher values are assumed to describe higher similarity.
#' @param sigma0 real valued, standard deviation of the different source distribution. If `NA` (default), the standard deviation of the scores
#' corresponding to non-matches (after selecting the highest phase) is taken. Real-values are interpreted as fixed standard deviation.
#' The user can provide their own function of the form `function (data)`, where `data` is a data frame with `land1, land2, score,` and `phase`.
#' The `phase` vector consists of integer values 1, ..., `k`, where `k` is the maximum of the number of unique lands in land1 and land2. The values are
#' ordered such that the highest value `k` of phase corresponds to the elements in score with the highest average score.
#' @return phase.test object. List of estimate=est1-est2,estimate1=est1, estimate2=est2, statistic= test.statistic,
#' p.value=pvalue, parameter=sigma_0, data = dframe
#' @export
phase_test <- function(land1, land2, score, sigma_0 = NA) {
  # data frame with structure land 1, land 2, score
  # returns estimate 1, estimate 2, test statistic (difference), sigma (of reference distribution),
  # and p-value
  if (is.numeric(sigma_0)) stopifnot(sigma_0 > 0) # only positive values for sigma_0 are allowed

  dframe <- data.frame(land1, land2, score)
  dframe <- dframe %>%
    arrange(land1, land2) %>% # provides row-first order (land2 is slower index)
    mutate(
      phase = get_phases(land1, land2)
    ) %>%
    arrange(land1, land2)
  n <- max(dframe$phase)
  avgs <- dframe %>% group_by(phase) %>%
    summarize(
      means = mean(score, na.rm = TRUE)
    ) %>% ungroup() %>%
    mutate(
      ordered = (1:n)[order(means)]
    ) %>% arrange(means)
  dframe <- dframe %>% mutate(
    phase = avgs$ordered[phase]
  )


  est1 <- avgs$means[n]
  est2 <- avgs$means[n/2]

  if (is.na(sigma_0)) sigma_0 <- sd(dframe %>% filter(phase!= n) %>% purrr::pluck("score"))/sqrt(n)
  if (is.function(sigma_0)) { sigma_0 = sigma_0(dframe)}
  test.statistic <- (est1-est2)
  pvalue <- F_T(test.statistic, sigma = sigma_0, n = n, lower.tail = FALSE)
  res <- list(estimate=est1-est2,estimate1=est1, estimate2=est2, statistic= test.statistic,
              p.value=pvalue, parameter=sigma_0, data = dframe)
  class(res) <- c("phase.test", "list")
  res
}

#' Extract results from test in tidy form
#'
#' `tidy.phase.test` expands the tidy method for test.phase objects. It gives a summary of the relevant
#' parameters and estimates.
#' @param x phase.test object as returned from `phase_test`
#' @param ... ignored
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' print(logo)
#' @method tidy phase.test
tidy.phase.test <- function (x, ...) {
  with(x, tibble(estimate, estimate1, estimate2, statistic, p.value, parameter))
}

#' Print information of a phase test
#'
#' `print.phase.test` expands the generic print method for x3p objects. It gives a summary of the most relevant x3p meta information and returns the object invisibly.
#' @param x phase.test object
#' @param ... ignored
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' print(logo)
#' @method print phase.test
print.phase.test <- function(x, ...) {
  cat(sprintf("
                 Phase Test

  alternative hypothesis:
  true difference in means between group SS and group DS is greater than 0 (indicating same-source)

  t = %f, sigma_0 = %f, p-value = %f

  sample estimates:\n", x$statistic, x$parameter, x$p.value))
  means <- c(x$estimate1, x$estimate2)
  names(means) <- c("mean in group SS", "mean in group DS")
  print(means)
}



#' Reference distribution for the test statistic between Same-source and Different-source averages using phase selection
#'
#' Assuming an overall sample size of 36 (standing for 6 x 6 comparisons, $n=6$), we are interested in the distribution of
#' T defined as the difference between the average of the same source values and the median average (XXX see paper for definition) of the different source values:
#' $$T = \bar{X}_{SS} - \tilde{X}_{DS}$$, where groups SS and DS are set by phase selection (after inspecting the set of $n$ values).
#' For a (one-sided) hypothesis test of $H_0$: no difference between the means and $H_1:$: the same-source values are larger
#' on average than the different source values, the test statistic $T$ has under the null hypothesis and the group-finding
#' strategy ${\cal G}$ a distribution given as
#' $$
#' P(T < t \mid H_0, {\cal G}) = C \cdot \int_{-\infty}^{\infty} F^2(x) f(x) \left[ F(x + t) - F(x) \right]^3 dx
#' $$
#' where $F$ is the (approximately normal) distribution of $\bar{X}_{SS}$ under $H_0$ with expected value of $\mu$ ($=E[\bar{X}_{SS}] = E[\bar{X}_{DS}]$) and standard deviation
#' $\sigma$ (standard deviation of a group of size $n$ under $H_0$).
#' @param t value of the observed test statistic, $t$ is positive.
#' @param sigma (estimated) standard deviation of an average of size $n$ of the (original) sample
#' @param n integer value, number of values in the same-source group.
#' @param lower.tail should the lower tail probability be returned?
#' @export
#' @examples
#' # example code
#' F_T(0.2, sigma = 0.1, lower.tail = FALSE) # only multitude of sigma informs the test
#' F_T(2, lower.tail = FALSE)
#' F_T(3, lower.tail = FALSE)
#' F_T(4, lower.tail = FALSE)
#' # critical values of t distribution and F_T (for sigma = 1):
#' F_T(qt(1-0.05, df=34), lower.tail = FALSE)
#' F_T(qt(1-0.05/6, df=34), lower.tail = FALSE)
#' F_T(qt(1-0.01/6, df=34), lower.tail = FALSE)
#'
#' x <- 10^-(1:4)
#' x <- sort(c(x, x/2))
#' t_q <- qt(1-x, df=34)
#' adjusted <- F_T(t_q, lower.tail=FALSE)
F_T <- function(t, sigma = 1, n = 6, lower.tail = TRUE) {
  if (length(t) == 1) return(F_T_one(t=t, sigma=sigma, n=n, lower.tail=lower.tail))
  sapply(t, FUN=F_T_one, sigma=sigma, n=n, lower.tail=lower.tail)
}

F_T_one <- function(t, sigma = 1, n = 6, lower.tail = TRUE) {
  # single value t expected for now
  if (t < 0) {
    warning("Range between maximum and median has to be positive. Did you call `DS - SS`?")
    t <- abs(t)
  }
  if (near(t, 0)) return(ifelse(lower.tail, 0, 1))
  med <- floor(n/2)
  C <- prod(med:n)/prod(1:(n-med-1))/med
  t_norm <- t/sigma

  normal_lower <- function(x) {
    # including constant in the integrand is computationally more expensive
    # but helps with numeric accuracy
    bigF <- pnorm(x)

    C*bigF^2 * dnorm(x)*(pnorm(x+t_norm) - bigF)^3
  }

  normal_upper <- function(x) {
    bigF <- pnorm(x)
    left <- (1 - bigF)^3
    right <- (pnorm(x+t_norm) - bigF)^3

    C*bigF^2 * dnorm(x)*(left-right)
  }

  res_lower <- integrate(normal_lower, lower = -Inf, upper=Inf) # P(T < t)
  res_upper <- integrate(normal_upper, lower = -Inf, upper=Inf) # P(T > t)
  # check which of these values we want to return

  # which value are we interested in?
  if (lower.tail) {
    res_upper$value <- 1 - res_upper$value
  } else {
    res_lower$value <- 1 - res_lower$value
  }
  # now both results should be the same - we pick the one with the lower relative value:

  assess_error <- function(est1, est2) {
    rel_err_est1 <- ifelse(est1$abs.error==0, 0, est1$abs.error/(est1$value))
    rel_err_est2 <- ifelse(est2$abs.error==0, 0, est2$abs.error/(est2$value))

    if (rel_err_est1 <  rel_err_est2) return(est1)
    est2
  }

  res <- assess_error(res_lower, res_upper)
  value <- res$value
  warning <- ifelse(res$abs.error/res$value> 1/10, "Warning: estimate and numeric accuracy are on the same order; proceed with caution", "OK")
  attributes(value) <- list(abs.error=res$abs.error, message=warning)
  value
}


#' Reference density for the test statistic between Same-source and Different-source averages using phase selection
#'
#' Assuming an overall sample size of 36 (standing for 6 x 6 comparisons, $n=6$), we are interested in the distribution of
#' T defined as the difference between the average of the same source values and the median average (XXX see paper for definition) of the different source values:
#' $$T = \bar{X}_{SS} - \tilde{X}_{DS}$$, where groups SS and DS are set by phase selection (after inspecting the set of $n$ values).
#' For a (one-sided) hypothesis test of $H_0$: no difference between the means and $H_1:$: the same-source values are larger
#' on average than the different source values, the test statistic $T$ has under the null hypothesis and the group-finding
#' strategy ${\cal G}$ a distribution given as
#' $$
#' P(T < t \mid H_0, {\cal G}) = C \cdot \int_{-\infty}^{\infty} F^2(x) f(x) \left[ F(x + t) - F(x) \right]^3 dx
#' $$
#' where $F$ is the (approximately normal) distribution of $\bar{X}_{SS}$ under $H_0$ with expected value of $\mu$ ($=E[\bar{X}_{SS}] = E[\bar{X}_{DS}]$) and standard deviation
#' $\sigma$ (standard deviation of a group of size $n$ under $H_0$).
#' @param t value of the observed test statistic, $t$ is positive.
#' @param sigma (estimated) standard deviation of an average of size $n$ of the (original) sample
#' @param n integer value, number of values in the same-source group.
#' @export
#' @examples
#' # example code
#' F_T(0.2, sigma = 0.1, lower.tail = FALSE) # only multitude of sigma informs the test
#' F_T(2, lower.tail = FALSE)
#' F_T(3, lower.tail = FALSE)
#' F_T(4, lower.tail = FALSE)
#' # critical values of t distribution and F_T (for sigma = 1):
#' F_T(qt(1-0.05, df=34), lower.tail = FALSE)
#' F_T(qt(1-0.05/6, df=34), lower.tail = FALSE)
#' F_T(qt(1-0.01/6, df=34), lower.tail = FALSE)
#'
#' x <- 10^-(1:4)
#' x <- sort(c(x, x/2))
#' t_q <- qt(1-x, df=34)
#' adjusted <- F_T(t_q, lower.tail=FALSE)
f_T <- function(t, sigma = 1, n = 6) {
  if (length(t) == 1) return(f_T_one(t=t, sigma=sigma, n=n))
  # sigma could be a vector too
  sapply(t, FUN=f_T_one, sigma=sigma, n=n)
}

f_T_one <- function(t, sigma = 1, n = 6) {
  # single value t expected for now
  if (t < 0) {
    warning = "Range between maximum and median has to be positive. Did you call `DS - SS`?"
    value <- 0
    attributes(value) <- list(abs.error=res$abs.error, message=warning)
    return(value)
  }
  if (near(t, 0)) return(0)
  r <- floor(n/2)
  C <- prod(r:n)/prod(1:(n-r-1))
  t_norm <- t/sigma

  integrand <- function(x) {
    # including constant in the integrand is computationally more expensive
    # but helps with numeric accuracy
    bigF <- pnorm(x)

    C/sigma*bigF^2 * dnorm(x)*(pnorm(x+t_norm) - bigF)^2 * dnorm(x+t_norm)
  }


  res <- integrate(integrand, lower = -Inf, upper=Inf)

  value <- res$value
  warning <- ifelse(res$abs.error/res$value> 1/10, "Warning: estimate and numeric accuracy are on the same order; proceed with caution", "OK")
  attributes(value) <- list(abs.error=res$abs.error, message=warning)
  value
}
