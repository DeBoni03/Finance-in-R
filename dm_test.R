dm_test <- function (e1, e2, alternative = c("two.sided", "less", "greater"),
          h = 1, power = 2, varestimator = c("acf", "bartlett", "NeweyWest"))
{
  alternative <- match.arg(alternative)
  varestimator <- match.arg(varestimator)
  h <- as.integer(h)
  if (h < 1L) {
    stop("h must be at least 1")
  }
  if (h > length(e1)) {
    stop("h cannot be longer than the number of forecast errors")
  }
  d <- c(abs(e1))^power - c(abs(e2))^power
  d.cov <- acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance",
               plot = FALSE)$acf[, , 1]
  n <- length(d)

  if(varestimator == "NeweyWest"){
    m <- floor(0.75*(n^(1/3)))
    d.var <- NeweyWest(lm(d~1),lag=m,prewhite=0)
  }else{
    if (varestimator == "acf" | h == 1L) {
      d.var <- sum(c(d.cov[1], 2 * d.cov[-1]))/n
    }else {
      d.var <- sum(c(d.cov[1], 2 * (1 - seq_len(h - 1)/h) *
                       d.cov[-1]))/n
    }
  }
  dv <- d.var

  if (dv > 0) {
    STATISTIC <- mean(d, na.rm = TRUE)/sqrt(dv)
  }
  else if (h == 1) {
    stop("Variance of DM statistic is zero")
  }
  else {
    warning("Variance is negative. Try varestimator = bartlett. Proceeding with horizon h=1.")
    return(dm.test(e1, e2, alternative, h = 1, power, varestimator))
  }
  k <- ((n + 1 - 2 * h + (h/n) * (h - 1))/n)^(1/2)
  STATISTIC <- STATISTIC * k
  names(STATISTIC) <- "DM"
  if (alternative == "two.sided") {
    PVAL <- 2 * pt(-abs(STATISTIC), df = n - 1)
  }
  else if (alternative == "less") {
    PVAL <- pt(STATISTIC, df = n - 1)
  }
  else if (alternative == "greater") {
    PVAL <- pt(STATISTIC, df = n - 1, lower.tail = FALSE)
  }
  PARAMETER <- c(h, power)
  names(PARAMETER) <- c("Forecast horizon", "Loss function power")
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 alternative = alternative, varestimator = varestimator,
                 p.value = PVAL, method = "Diebold-Mariano Test", data.name = c(deparse(substitute(e1)),
                                                                                deparse(substitute(e2)))), class = "htest")
}
