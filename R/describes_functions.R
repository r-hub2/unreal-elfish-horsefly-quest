# DESCRIBES ####

#' @include quest_functions.R psymet_functions.R
NULL

# MEAN DIFFERENCES ####

# mean_test #

#' Test for Sample Mean Against Mu (one-sample t-test)
#'
#' \code{mean_test} computes the sample mean and compares it against a specified
#' population \code{mu} value. This is sometimes referred to as a one-sample
#' t-test. It provides the same results as \code{\link[stats]{t.test}}, but
#' provides the confidence interval for the mean difference from mu rather than
#' the mean itself. The function also calculates the descriptive statistics and
#' the standardized mean difference (i.e., Cohen's d) based on the sample
#' standard deviation.
#'
#' @param x numeric vector.
#'
#' @param mu numeric vector of length 1 specifying the population mean value to
#'   compare the sample mean against.
#'
#' @param d.ci.type character vector with length 1 specifying the type of
#'   confidence interval to compute for the standardized mean difference (i.e.,
#'   Cohen's d). There are currently two options: 1. "tdist" which calculates
#'   the confidence intervals based on the t-distribution using the function
#'   \code{\link[psych]{cohen.d.ci}}. No standard error is calculated for this
#'   option and NA is returned for "d_se" in the return object. 2. "classic"
#'   which calculates the confidence interval of Cohen's d based on the
#'   confidence interval of the mean difference itself. The lower and upper
#'   confidence bounds are divided by the sample standard deviation.
#'   Technically, this confidence interval is biased due to not taking into
#'   account the uncertainty of the standard deviations. No standard error is
#'   calculated for this option and NA is returned for "d_se" in the return
#'   object.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   It must be between 0 and 1.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, checking whether
#'   \code{x} is a numeric vector. This is a tradeoff between computational
#'   efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   sample mean: 1) nhst = one-sample t-test stat info in a numeric vector,
#'   2) desc = descriptive statistics stat info in a numeric vector,
#'   3) std = standardized mean difference stat info in a numeric vector
#'
#' 1) nhst = one-sample t-test stat info in a numeric vector
#'
#' \describe{
#'    \item{est}{mean - mu estimate}
#'    \item{se}{standard error}
#'    \item{t}{t-value}
#'    \item{df}{degrees of freedom}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector
#'
#' \describe{
#'    \item{mean}{mean of \code{x}}
#'    \item{mu}{population value of comparison}
#'    \item{sd}{standard deviation of \code{x}}
#'    \item{n}{sample size of \code{x}}
#' }
#'
#' 3) std = standardized mean difference stat info in a numeric vector
#'
#' \describe{
#'    \item{d_est}{Cohen's d estimate}
#'    \item{d_se}{Cohen's d standard error}
#'    \item{d_lwr}{Cohen's d lower bound of the confidence interval}
#'    \item{d_upr}{Cohen's d upper bound of the confidence interval}
#' }
#'
#' @seealso
#'    \code{\link{means_test}} one-sample t-tests for multiple variables,
#'    \code{\link[stats]{t.test}} same results,
#'    \code{\link{mean_diff}} independent two-sample t-test,
#'    \code{\link{mean_change}} dependent two-sample t-test,
#'
#' @examples
#'
#' # one-sample t-test
#' mean_test(x = mtcars$"mpg")
#' mean_test(x = attitude$"rating", mu = 50)
#' mean_test(x = attitude$"rating", mu = 50, d.ci.type = "classic")
#'
#' # compare to t.test()
#' mean_test(x = attitude$"rating", mu = 50, ci.level = .99)
#' t.test(attitude$"rating", mu = 50, conf.level = .99)
#'
#' # same as intercept-only regression when mu = 0
#' mean_test(x = mtcars$"mpg")
#' lm_obj <- lm(mpg ~ 1, data = mtcars)
#' coef(summary(lm_obj))
#'
#' @export
mean_test <- function(x, mu = 0, d.ci.type = "tdist", ci.level = 0.95,
   check = TRUE) {

   # errors
   if (check) {
      checkmate::assertNumeric(x)
      checkmate::assertNumeric(mu, len = 1L, any.missing = FALSE)
      d.ci.type <- match.arg(d.ci.type, choices = c("tdist","classic"))
      checkmate::assertNumeric(ci.level, lower = 0L, upper = 1L, any.missing = FALSE)
   }

   # t.test
   est_mean <- mean(x, na.rm = TRUE)
   est_mean_mu <- est_mean - mu
   est_var <- var(x, na.rm = TRUE)
   n <- vecNA(x, prop = FALSE, ov = TRUE)
   se_mean <- sqrt(est_var / n)
   df <- n - 1
   tmp <- nhst(est = est_mean_mu, se = se_mean, df = df, ci.level = ci.level)
   ttest_vec <- str2str::d2v(tmp)

   # descriptives
   est_sd <- sqrt(est_var)
   describes_vec <- c("mean" = est_mean, "mu" = mu, "sd" = est_sd, "n" = n)

   # standardized mean difference
   d_est <- est_mean_mu / est_sd
   if (d.ci.type == "classic") {
      d_ci <- ttest_vec[c("lwr","upr")] / est_sd
      d_se <- NA
      d_vec <- c(d_est, d_se, d_ci)
   }
   if (d.ci.type == "tdist") {
      tmp <- psych::cohen.d.ci(d_est, n = n, alpha = 1 - ci.level)
      d_ci <- tmp[1, c(1,3)] # matrix (also incorrectly labeled "d" in the documentation)
      d_se <- NA
      d_vec <- c(d_est, d_se, d_ci)
   }
   names(d_vec) <- c("d_est","d_se","d_lwr","d_upr")

   # return object
   rtn <- list("nhst" = ttest_vec, "desc" = describes_vec, "std" = d_vec)
   return(rtn)
}

# means_test #

#' Test for Multiple Sample Means Against Mu (one-sample t-tests)
#'
#' \code{means_test} computes sample means and compares them against specified
#' population \code{mu} values. These are sometimes referred to as one-sample
#' t-tests. It provides the same results as \code{\link[stats]{t.test}}, but
#' provides the confidence intervals for the mean differences from mu rather
#' than the mean itself. The function also calculates the descriptive statistics
#' and the standardized mean differences (i.e., Cohen's d) based on the sample
#' standard deviations.
#'
#' @param data data.frame or data.
#'
#' @param vrb.nm character vector of colnames specifying the variables in
#'   \code{data} to conduct the one-sample t-tests for.
#'
#' @param mu numeric vector of length = \code{length(vrb.nm)} or length 1
#'   specifying the population mean values to compare the sample means against.
#'   The order of the values should be the same as the order in \code{vrb.nm}.
#'   When length 1, the same population mean value is used for all the
#'   variables.
#'
#' @param d.ci.type character vector with length 1 of specifying the type of
#'   confidence intervals to compute for the standardized mean differences
#'   (i.e., Cohen's d). There are currently two options: 1. "tdist" which
#'   calculates the confidence intervals based on the t-distribution using the
#'   function \code{\link[psych]{cohen.d.ci}}. No standard error is calculated
#'   for this option and NA is returned for "d_se" in the return object. 2.
#'   "classic" which calculates the confidence intervals of Cohen's d based on
#'   the confidence interval of the mean difference itself. The lower and upper
#'   confidence bounds are divided by the sample standard deviation.
#'   Technically, this confidence interval is biased due to not taking into
#'   account the uncertainty of the standard deviations. No standard error is
#'   calculated for this option and NA is returned for "d_se" in the return
#'   object.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   It must be between 0 and 1.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, checking whether
#'   \code{ci.level} is between 0 and 1. This is a tradeoff between
#'   computational efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of data.frames containing statistical information about the
#'   sample means (the rownames of the data.frames are \code{vrb.nm}): 1)
#'   nhst = one-sample t-test stat info in a data.frame, 2) desc = descriptive
#'   statistics stat info in a data.frame, 3) std = standardized mean difference
#'   stat info in a data.frame
#'
#' 1) nhst = one-sample t-test stat info in a data.frame
#'
#' \describe{
#'    \item{est}{mean - mu estimate}
#'    \item{se}{standard error}
#'    \item{t}{t-value}
#'    \item{df}{degrees of freedom}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a data.frame
#'
#' \describe{
#'    \item{mean}{mean of \code{x}}
#'    \item{mu}{population value of comparison}
#'    \item{sd}{standard deviation of \code{x}}
#'    \item{n}{sample size of \code{x}}
#' }
#'
#' 3) std = standardized mean difference stat info in a data.frame
#'
#' \describe{
#'    \item{d_est}{Cohen's d estimate}
#'    \item{d_se}{Cohen's d standard error}
#'    \item{d_lwr}{Cohen's d lower bound of the confidence interval}
#'    \item{d_upr}{Cohen's d upper bound of the confidence interval}
#' }
#'
#' @seealso
#'    \code{\link{mean_test}} one-sample t-test for a single variable,
#'    \code{\link[stats]{t.test}} same results,
#'    \code{\link{means_diff}} independent two-sample t-tests for multiple variables,
#'    \code{\link{means_change}} dependent two-sample t-tests for multiple variables,
#'
#' @examples
#'
#' # one-sample t-tests
#' means_test(data = attitude, vrb.nm = names(attitude), mu = 50)
#' means_test(data = attitude, vrb.nm = c("rating","complaints","privileges"),
#'    mu = c(60, 55, 50))
#' means_test(data = attitude, vrb.nm = names(attitude), mu = 50, ci.level = 0.90)
#' means_test(airquality, vrb.nm = names(airquality)) # different df and n due to missing data
#'
#' # compare to t.test
#' means_test(data = attitude, vrb.nm = "rating", mu = 50, ci.level = .99)
#' t.test(attitude$"rating", mu = 50, conf.level = .99)
#'
#' # same as intercept-only regression
#' means_test(data = attitude, vrb.nm = "rating")
#' lm_obj <- lm(rating ~ 1, data = attitude)
#' coef(summary(lm_obj))
#'
#' @export
means_test <- function(data, vrb.nm, mu = 0, d.ci.type = "tdist", ci.level = 0.95,
   check = TRUE) {

   # errors
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertCharacter(vrb.nm, any.missing = FALSE)
      not_colnames <- str2str::not.colnames(x = data, nm = vrb.nm)
      if (0 != length(not_colnames))
         stop("`vrb.nm` must all be colnames in `data` - these are not: ", not_colnames)
      checkmate::assertNumeric(mu, any.missing = FALSE)
      if (1 != length(mu) && length(vrb.nm) != length(mu))
         stop("`mu` must have length 1 or length equal to the length of `vrb.nm`")
      d.ci.type <- match.arg(d.ci.type, choices = c("tdist","classic"))
      checkmate::assertNumeric(ci.level, lower = 0L, upper = 1L, any.missing = FALSE, null.ok = TRUE)
   }

   # function itself
   if (length(mu) == 1) mu <- rep.int(x = mu, times = length(vrb.nm))
   tmp <- Map(nm = str2str::sn(vrb.nm), pop_mean = mu, f = function(nm, pop_mean) {
      mean_test(x = data[[nm]], mu = pop_mean, d.ci.type = d.ci.type,
         ci.level = ci.level, check = FALSE)
   })
   if (1 == length(vrb.nm)) {
      tmp2 <- tmp[[1]]
      rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
   } else {
      tmp2 <- str2str::t_list(tmp) # t_list() does not work for lists with one element
      rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
   }
   return(rtn)
}

# mean_change #

#' Mean Change Across Two Timepoints (dependent two-samples t-test)
#'
#' \code{mean_change} tests for mean change across two timepoints with a
#' dependent two-samples t-test. The function also calculates the descriptive
#' statistics for the timepoints and the standardized mean difference (i.e.,
#' Cohen's d) based on either the standard deviation of the pre-timepoint,
#' pooled standard deviation of the pre-timepoint and post-timepoint, or the
#' standard deviation of the change score (post - pre). \code{mean_change} is
#' simply a wrapper for \code{\link[stats]{t.test}} plus some extra
#' calculations.
#'
#' \code{mean_change} calculates the mean change as \code{post} - \code{pre}
#' such that increases over time have a positive mean change estimate and
#' decreases over time have a negative mean change estimate. This would be as if
#' the post-timepoint was \code{x} and the pre-timepoint was \code{y} in
#' \code{t.test(paired = TRUE)}.
#'
#' @param pre numeric vector of the variable at the pre-timepoint.
#'
#' @param post numeric vector of the variable at the post-timepoint. The
#'   elements must correspond to the same cases in \code{pre} as pairs by
#'   position. Thus, the length of \code{post} must be the same as \code{pre}.
#'   Note, missing values in \code{post} are expected and handled with listwise
#'   deletion.
#'
#' @param standardizer chararacter vector of length 1 specifying what to use for
#'   standardization when computing the standardized mean difference (i.e.,
#'   Cohen's d). There are three options: 1. "pre" for the standard deviation of
#'   the pre-timepoint, 2. "pooled" for the pooled standard deviation of the
#'   pre-timepoint and post-timepoint, 3. "change" for the standard deviation of
#'   the change score (post - pre). The default is "pre", which I believe makes
#'   the most theoretical sense (see Cumming, 2012); however, "change" is the
#'   traditional choice originally proposed by Jacob Cohen (Cohen, 1988).
#'
#' @param d.ci.type character vector of lenth 1 specifying how to compute the
#'   confidence interval (and standard error) of the standardized mean
#'   difference. There are currently two options: 1. "unbiased" which calculates
#'   the unbiased standard error of Cohen's d based on the formulas in
#'   Viechtbauer (2007). If \code{standardizer} = "pre" or "pooled", then
#'   equation 36 from Table 2 is used. If \code{standardizer} = "change", then
#'   equation 25 from Table 1 is used. A symmetrical confidence interval is then
#'   calculated based on the standard error. 2. "classic" which calculates the
#'   confidence interval of Cohen's d based on the confidence interval of the
#'   mean change itself. The lower and upper confidence bounds are divided by
#'   the \code{standardizer}. Technically, this confidence interval is biased
#'   due to not taking into account the uncertainty of the \code{standardizer}.
#'   No standard error is calculated for this option and NA is returned for
#'   "d_se" in the return object.
#'
#' @param ci.level double vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, checking whether
#'   \code{post} is the same length as \code{pre}. This is a tradeoff between
#'   computational efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   mean change: 1) nhst = dependent two-samples t-test stat info in a numeric vector,
#'   2) desc = descriptive statistics stat info in a numeric vector, 3) std =
#'   standardized mean difference stat info in a numeric vector
#'
#' 1) nhst = dependent two-samples t-test stat info in a numeric vector
#'
#' \describe{
#'    \item{est}{mean change estimate (i.e., post - pre)}
#'    \item{se}{standard error}
#'    \item{t}{t-value}
#'    \item{df}{degrees of freedom}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector
#'
#' \describe{
#'    \item{mean_post}{mean of the post variable}
#'    \item{mean_pre}{mean of the pre variable}
#'    \item{sd_post}{standard deviation of of the post variable}
#'    \item{sd_pre}{standard deviation of the pre variable}
#'    \item{n}{sample size of the change score}
#'    \item{r}{Pearson correlation between the pre and post variables}
#' }
#'
#' 3) std = standardized mean difference stat info in a numeric vector
#'
#' \describe{
#'    \item{d_est}{Cohen's d estimate}
#'    \item{d_se}{Cohen's d standard error}
#'    \item{d_lwr}{Cohen's d lower bound of the confidence interval}
#'    \item{d_upr}{Cohen's d upper bound of the confidence interval}
#' }
#'
#' @references
#'
#'    Cohen, J. (1988). Statistical power analysis for the behavioral sciences,
#'    2nd ed. Hillsdale, NJ: Erlbaum.
#'
#'    Cumming, G. (2012). Understanding the new statistics: Effect sizes,
#'    confidence intervals, and meta-analysis. New York, NY: Rouledge.
#'
#'    Viechtbauer, W. (2007). Approximate confidence intervals for standardized
#'    effect sizes in the two-independent and two-dependent samples design.
#'    Journal of Educational and Behavioral Statistics, 32(1), 39-60.
#'
#' @seealso
#'    \code{\link{means_change}} for multiple sets of prepost pairs of variables,
#'    \code{\link[stats]{t.test}} the workhorse for \code{mean_change},
#'    \code{\link{mean_diff}} for a independent two-samples t-test,
#'    \code{\link{mean_test}} for a one-sample t-test,
#'
#' @examples
#'
#' # dependent two-sample t-test
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp") # standardizer = "pre"
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp", d.ci.type = "classic")
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp", standardizer = "pooled")
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp", ci.level = 0.99)
#' mean_change(pre = mtcars$"hp", post = mtcars$"disp",
#'    ci.level = 0.99) # note, when flipping pre and post, the cohen's d estimate
#'    # changes with standardizer = "pre" because the "pre" variable is different.
#'    # This does not happen for standardizer = "pooled" or "change". For example...
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp", standardizer = "pooled")
#' mean_change(pre = mtcars$"hp", post = mtcars$"disp", standardizer = "pooled")
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp", standardizer = "change")
#' mean_change(pre = mtcars$"hp", post = mtcars$"disp", standardizer = "change")
#'
#' # same as intercept-only regression with the change score
#' mean_change(pre = mtcars$"disp", post = mtcars$"hp")
#' lm_obj <- lm(hp - disp ~ 1, data = mtcars)
#' coef(summary(lm_obj))
#'
#' @export
mean_change <- function(pre, post, standardizer = "pre", d.ci.type = "unbiased",
   ci.level = 0.95, check = TRUE) {

   # error checking
   if (check) {
      if (length(pre) != length(post))
         stop("`pre` and `post` must have the same length")
      checkmate::assertNumeric(pre)
      checkmate::assertNumeric(post)
      standardizer <- match.arg(standardizer, choices = c("pre","pooled","change"))
      d.ci.type <- match.arg(d.ci.type, choices = c("unbiased","classic"))
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
   }

   # complete.cases
   dat0 <- data.frame(pre, post)
   dat <- dat0[complete.cases(dat0), ]

   # t.test
   ttest_obj <- t.test(x = dat$"post", y = dat$"pre", alternative = "two.sided", # t.test.default does x - y so have x = post and y = pre
      paired = TRUE, conf.level = ci.level)
   est <- ttest_obj[["estimate"]]
   se <- ttest_obj[["stderr"]]
   t_value <- ttest_obj[["statistic"]]
   nhst <- unlist(ttest_obj[c("parameter","p.value")])
   ci <- ttest_obj[["conf.int"]]
   ttest_vec <- setNames(c(est, se, t_value, nhst, ci),
      nm = c("est","se","t","df","p","lwr","upr"))

   # descriptives
   mean_by <- colMeans(dat[c("post","pre")])
   sd_by <- apply(X = dat[c("post", "pre")], MARGIN = 2, FUN = sd)
   names(mean_by) <- paste0("mean_", c("post","pre"))
   names(sd_by) <- paste0("sd_", c("post","pre"))
   n <- nrow(dat)
   r <- cor(x = dat$"post", y = dat$"pre")
   describes_vec <- c(mean_by, sd_by, "n" = n, "r" = r)

   # cohen's d
   sd_d <- switch(standardizer,
      pre = sd(dat$"pre"),
      pooled = sqrt(mean(c("post" = var(dat$"post"), "pre" = var(dat$"pre")))),
      change = sd(dat$"post" - dat$"pre"))
   d_est <- ttest_vec["est"] / sd_d
   if (d.ci.type == "classic") {
      d_se <- NA
      d_ci <- ci / sd_d
      d_vec <- c(d_est, d_se, d_ci)
   }
   if ((standardizer == "pre" || standardizer == "pooled") && d.ci.type == "unbiased") {
      ru <- r + ((r * (1 - r^2)) / (2 * (n - 4)))
      m <- n - 1
      cm <- 1 - ((3) / (4*m - 1))
      first_term <- (2 * (1 - ru)) / (n * (cm^2))
      second_term <- (m - 2) / (m * (cm^2))
      d_se <- sqrt(first_term + ((1 - second_term) * d_est^2))
      d_vec <- str2str::d2v(confint2.default(obj = d_est, se = d_se, df = m,
         level = ci.level))
   }
   if (standardizer == "change" && d.ci.type == "unbiased") {
      m <- n - 1
      cm <- 1 - ((3) / (4*m - 1))
      first_term <- 1 / (n * (cm^2))
      second_term <- (m - 2) / (m * (cm^2))
      d_se <- sqrt(first_term + ((1 - second_term) * d_est^2))
      d_vec <- str2str::d2v(confint2.default(obj = d_est, se = d_se, df = m,
         level = ci.level))
   }
   names(d_vec) <- c("d_est","d_se","d_lwr","d_upr")

   # return object
   rtn <- list("nhst" = ttest_vec, "desc" = describes_vec, "std" = d_vec)
   return(rtn)
}

# mean_diff #

#' Mean difference across two independent groups (independent two-samples
#' t-test)
#'
#' \code{mean_diff} tests for mean differences across two independent groups
#' with an independent two-samples t-test. The function also calculates the
#' descriptive statistics for each group and the standardized mean difference
#' (i.e., Cohen's d) based on the pooled standard deviation. \code{mean_diff} is
#' simply a wrapper for \code{\link[stats]{t.test}} plus some extra
#' calculations.
#'
#' \code{mean_diff} calculates the mean difference as \code{x[bin == lvl[2] ]} -
#' \code{x[bin == lvl[1] ]} such that it is group 2 - group 1. Group 1 corresponds
#' to the first factor level of \code{bin} (after being coerced to a factor).
#' Group 2 correspond to the second factor level \code{bin} (after being coerced
#' to a factor). This was set up to handle dummy coded treatment variables in a
#' desirable way. For example, if \code{bin} is a numeric vector with values
#' \code{0} and \code{1}, the default factor coersion will have the first factor
#' level be "0" and the second factor level "1". This would result will
#' correspond to 1 - 0. However, if the first factor level of \code{bin} is
#' "treatment" and the second factor level is "control", the result will
#' correspond to control - treatment. If the opposite is desired (e.g.,
#' treatment - control), this can be reversed within the function by specifying
#' the \code{lvl} argument as \code{c("control","treatment")}. Note,
#' \code{mean_diff} diverts from \code{t.test} by calculating the mean
#' difference as group 2 - group 1 (as opposed to the group 1 - group 2 that
#' \code{t.test} does). However, group 2 - group 1 is the convention that
#' \code{psych::cohen.d} uses as well.
#'
#' \code{mean_diff} calculates the pooled standard deviation in a different way
#' than \code{\link[psych]{cohen.d}}. Therefore, the Cohen's d estimates (and
#' confidence intervals if d.ci.type == "tdist") differ from those in
#' \code{\link[psych]{cohen.d}}. \code{mean_diff} uses the total degrees of
#' freedom in the denomenator while \code{\link[psych]{cohen.d}} uses the total
#' sample size in the denomenator - based on the notation in McGrath & Meyer
#' (2006). However, almost every introduction to statistics textbook uses the
#' total degrees of freedom in the denomenator and that is what makes more sense
#' to me. See examples.
#'
#' @param x numeric vector.
#'
#' @param bin atomic vector (e.g., factor) the same length as \code{x} that is a
#'   binary variable. It identifies the two groups with two (and only two)
#'   unique values (other than missing values).
#'
#' @param lvl character vector with length 2 specifying the unique values for
#'   the two groups. If \code{bin} is a factor, then \code{lvl} should be the
#'   factor levels rather than the underlying integer codes. This argument
#'   allows you to specify the direction of the mean difference.
#'   \code{mean_diff} calculates the mean difference as \code{x[bin == lvl[2] ]} -
#'   \code{x[bin == lvl[1] ]} such that it is group 2 - group 1. By changing which
#'   group is group 1 vs. group 2, the direction of the mean difference can be
#'   changed. See details.
#'
#' @param var.equal logical vector of length 1 specifying whether the variances
#'   of the groups are assumed to be equal (TRUE) or not (FALSE). If TRUE, a
#'   traditional independent two-samples t-test is computed; if FALSE, Welch's
#'   t-test is computed. These two tests differ by their degrees of freedom and
#'   p-values.
#'
#' @param d.ci.type character vector with length 1 of specifying the type of
#'   confidence intervals to compute for the standardized mean difference (i.e.,
#'   Cohen's d). There are currently three options: 1) "unbiased" which
#'   calculates the unbiased standard error of Cohen's d based on formula 25 in
#'   Viechtbauer (2007). A symmetrical confidence interval is then calculated
#'   based on the standard error. 2) "tdist" which calculates the confidence
#'   intervals based on the t-distribution using the function
#'   \code{\link[psych]{cohen.d.ci}}, 3) "classic" which calculates the
#'   confidence interval of Cohen's d based on the confidence interval of the
#'   mean difference itself. The lower and upper confidence bounds are divided
#'   by the pooled standard deviation. Technically, this confidence interval is
#'   biased due to not taking into account the uncertainty of the standard
#'   deviations. No standard error is calculated for this option and NA is
#'   returned for "d_se" in the return object.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{bin} has more
#'   than 2 unique values (other than missing values) or if \code{bin} has
#'   length different than the length of \code{x}. This is a tradeoff between
#'   computational efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   mean difference: 1) nhst = independent two-samples t-test stat info in a
#'   numeric vector, 2) desc = descriptive statistics stat info in a numeric
#'   vector, 3) std = standardized mean difference stat info in a numeric vector
#'
#' 1) nhst = independent two-samples t-test stat info in a numeric vector
#'
#' \describe{
#'    \item{est}{mean difference estimate (i.e., group 2 - group 1)}
#'    \item{se}{standard error}
#'    \item{t}{t-value}
#'    \item{df}{degrees of freedom}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector
#'
#' \describe{
#'    \item{mean_`lvl[2]`}{mean of group 2}
#'    \item{mean_`lvl[1]`}{mean of group 1}
#'    \item{sd_`lvl[2]`}{standard deviation of group 2}
#'    \item{sd_`lvl[1]`}{standard deviation of group 1}
#'    \item{n_`lvl[2]`}{sample size of group 2}
#'    \item{n_`lvl[1]`}{sample size of group 1}
#' }
#'
#' 3) std = standardized mean difference stat info in a numeric vector
#'
#' \describe{
#'    \item{d_est}{Cohen's d estimate}
#'    \item{d_se}{Cohen's d standard error}
#'    \item{d_lwr}{Cohen's d lower bound of the confidence interval}
#'    \item{d_upr}{Cohen's d upper bound of the confidence interval}
#' }
#'
#' @references
#'
#'    McGrath, R. E., & Meyer, G. J. (2006). When effect sizes disagree: the case of
#'    r and d. Psychological Methods, 11(4), 386-401.
#'
#'    Viechtbauer, W. (2007). Approximate confidence intervals for standardized
#'    effect sizes in the two-independent and two-dependent samples design.
#'    Journal of Educational and Behavioral Statistics, 32(1), 39-60.
#'
#' @seealso
#'    \code{\link[stats]{t.test}} the workhorse for \code{mean_diff},
#'    \code{\link{means_diff}} for multiple variables across the same two groups,
#'    \code{\link[psych]{cohen.d}} for another standardized mean difference function,
#'    \code{\link{mean_change}} for dependent two-sample t-test,
#'    \code{\link{mean_test}} for one-sample t-test,
#'
#' @examples
#'
#' # independent two-samples t-test
#' mean_diff(x = mtcars$"mpg", bin = mtcars$"vs")
#' mean_diff(x = mtcars$"mpg", bin = mtcars$"vs", lvl = c("1","0"))
#' mean_diff(x = mtcars$"mpg", bin = mtcars$"vs", lvl = c(1, 0)) # levels don't have to be character
#' mean_diff(x = mtcars$"mpg", bin = mtcars$"vs", d.ci.type = "classic")
#'
#' # compare to psych::cohen.d()
#' mean_diff(x = mtcars$"mpg", bin = mtcars$"vs", d.ci.type = "tdist")
#' tmp_nm <- c("mpg","vs") # because otherwise Roxygen2 gets upset
#' cohend_obj <- psych::cohen.d(mtcars[tmp_nm], group = "vs")
#' as.data.frame(cohend_obj[["cohen.d"]]) # different estimate of cohen's d
#'    # of course, this also leads to different confidence interval bounds as well
#'
#' # same as intercept-only regression when var.equal = TRUE
#' mean_diff(x = mtcars$"mpg", bin = mtcars$"vs", d.ci.type = "tdist")
#' lm_obj <- lm(mpg ~ vs, data = mtcars)
#' coef(summary(lm_obj))
#'
#' # errors
#' \dontrun{
#' mean_diff(x = mtcars$"mpg",
#'    bin = attitude$"ratings") # `bin` has length different than `x`
#' mean_diff(x = mtcars$"mpg",
#'    bin = mtcars$"gear") # `bin` has more than two unique values (other than missing values)
#' }
#'
#' @export
mean_diff <- function(x, bin, lvl = levels(as.factor(bin)), var.equal = TRUE,
   d.ci.type = "unbiased", ci.level = 0.95, check = TRUE) {

   # error checking
   if (check) {
      if (length(x) != length(bin))
         stop("`x` and `bin` must have the same length")
      checkmate::assertNumeric(x)
      if (length(na.omit(unique(bin))) != 2)
         stop("`bin` must contain exactly two unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, len = 2, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(bin)))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `bin` (other than missing values)")
      checkmate::assertLogical(var.equal, len = 1, any.missing = FALSE)
      d.ci.type <- match.arg(d.ci.type, choices = c("unbiased","tdist","classic"))
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
   }

   # complete.cases
   lvl <- as.character(lvl) # so user doesn't have to convert to character themselves
   bin <- factor(bin, levels = lvl) # works even if bin already is a factor
   dat0 <- data.frame(x, bin)
   dat <- dat0[complete.cases(dat0), ]

   # t.test
   ttest_obj <- t.test(x ~ bin, data = dat, alternative = "two.sided", # t.test.formula (can't have the first argument named - e.g., `x`)
      var.equal = var.equal, conf.level = ci.level) # paired = FALSE
   ttest_est <- ttest_obj[["estimate"]]
   est <- ttest_est[2] - ttest_est[1] # to be consistent with psych::cohen.d
   se <- ttest_obj[["stderr"]]
   t_value <- ttest_obj[["statistic"]] * -1
   nhst <- unlist(ttest_obj[c("parameter","p.value")])
   ci <- rev(ttest_obj[["conf.int"]]) * -1
   ttest_vec <- setNames(c(est, se, t_value, nhst, ci),
      nm = c("est","se","t","df","p","lwr","upr"))

   # descriptives
   mean_by <- agg(x = dat$"x", grp = dat$"bin", rep = FALSE, rtn.grp = FALSE, fun = mean)
   sd_by <- agg(x = dat$"x", grp = dat$"bin", rep = FALSE, rtn.grp = FALSE, fun = sd)
   n_by <- agg(x = dat$"x", grp = dat$"bin", rep = FALSE, rtn.grp = FALSE, fun = length)
   names(mean_by) <- paste0("mean_", lvl)
   names(sd_by) <- paste0("sd_", lvl)
   names(n_by) <- paste0("n_", lvl)
   describes_vec <- c(rev(mean_by), rev(sd_by), rev(n_by))

   # cohen's d
   num_pooled <- ((n_by[1] - 1) * sd_by[1]^2) + ((n_by[2] - 1) * sd_by[2]^2)
   den_pooled <- (n_by[1] - 1) + (n_by[2] - 1) # psych::cohen.d() does not include the two minus 1s
   sd_pooled <- sqrt(num_pooled / den_pooled)
   d_est <- ttest_vec["est"] / sd_pooled
   if (d.ci.type == "classic") {
      d_ci <- ci / sd_pooled
      d_se <- NA
      d_vec <- c(d_est, d_se, d_ci)
   }
   if (d.ci.type == "unbiased") {
      n_ene <- prod(n_by) / sum(n_by)
      m <- den_pooled
      cm <- 1 - ((3) / (4*m - 1))
      first_term <- 1 / (n_ene * (cm^2))
      second_term <- (m - 2) / (m * (cm^2))
      d_se <- sqrt(first_term + ((1 - second_term) * d_est^2))
      d_vec <- str2str::d2v(confint2.default(obj = d_est, se = d_se, df = m,
         level = ci.level))
   }
   if (d.ci.type == "tdist") {
      tmp <- psych::cohen.d.ci(d_est, n1 = n_by[1], n2 = n_by[2],
         alpha = 1 - ci.level)
      d_ci <- tmp[1, c(1,3)] # matrix (also incorrectly labeled "d" in the documentation)
      d_se <- NA
      d_vec <- c(d_est, d_se, d_ci)
   }
   names(d_vec) <- c("d_est","d_se","d_lwr","d_upr")

   # return object
   rtn <- list("nhst" = ttest_vec, "desc" = describes_vec, "std" = d_vec)
   return(rtn)
}

# means_diff #

#' Mean differences across two independent groups (independent two-samples
#' t-tests)
#'
#' \code{means_diff} tests for mean differences across two independent groups
#' with independent two-samples t-tests. The function also calculates the
#' descriptive statistics for each group and the standardized mean differences
#' (i.e., Cohen's d) based on the pooled standard deviations. \code{mean_diff}
#' is simply a wrapper for \code{\link[stats]{t.test}} plus some extra
#' calculations.
#'
#' \code{means_diff} calculates the mean differences as
#' \code{data[[vrb.nm]][data[[bin.nm]] == lvl[2], ]} -
#' \code{data[[vrb.nm]][data[[bin.nm]] == lvl[1], ]} such that it is group 2 -
#' group 1. Group 1 corresponds to the first factor level of
#' \code{data[[bin.nm]]} (after being coerced to a factor). Group 2 correspond
#' to the second factor level of \code{data[[bin.nm]]} (after being coerced to a
#' factor). This was set up to handle dummy coded treatment variables in a
#' desirable way. For example, if \code{data[[bin.nm]]} is a numeric vector with
#' values \code{0} and \code{1}, the default factor coersion will have the first
#' factor level be "0" and the second factor level "1". This would result will
#' correspond to 1 - 0. However, if the first factor level of
#' \code{data[[bin.nm]]} is "treatment" and the second factor level is
#' "control", the result will correspond to control - treatment. If the opposite
#' is desired (e.g., treatment - control), this can be reversed within the
#' function by specifying the \code{lvl} argument as
#' \code{c("control","treatment")}. Note, \code{means_diff} diverts from
#' \code{t.test} by calculating the mean difference as group 2 - group 1 (as
#' opposed to the group 1 - group 2 that \code{t.test} does). However, group 2 -
#' group 1 is the convention that \code{psych::cohen.d} uses as well.
#'
#' \code{means_diff} calculates the pooled standard deviation in a different way
#' than \code{\link[psych]{cohen.d}}. Therefore, the Cohen's d estimates (and
#' confidence intervals if d.ci.type == "tdist") differ from those in
#' \code{\link[psych]{cohen.d}}. \code{means_diff} uses the total degrees of
#' freedom in the denomenator while \code{\link[psych]{cohen.d}} uses the total
#' sample size in the denomenator - based on the notation in McGrath & Meyer
#' (2006). However, almost every introduction to statistics textbook uses the
#' total degrees of freedom in the denomenator and that is what makes more sense
#' to me. See examples.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames specifying the variables in
#'   \code{data} to conduct the independent two-sample t-tests for.
#'
#' @param bin.nm character vector of length 1 specifying the binary variable in
#'   \code{data}. It identifies the two groups with two (and only two) unique
#'   values (other than missing values).
#'
#' @param lvl character vector with length 2 specifying the unique values for
#'   the two groups. If \code{data[[bin.nm]]} is a factor, then \code{lvl}
#'   should be the factor levels rather than the underlying integer codes. This
#'   argument allows you to specify the direction of the mean difference.
#'   \code{means_diff} calculates the mean differences as
#'   \code{data[[vrb.nm]][data[[bin.nm]] == lvl[2], ]} -
#'   \code{data[[vrb.nm]][data[[bin.nm]] == lvl[1], ]} such that it is group 2 -
#'   group 1. By changing which group is group 1 vs. group 2, the direction of
#'   the mean difference can be changed. See details.
#'
#' @param var.equal logical vector of length 1 specifying whether the variances
#'   of the groups are assumed to be equal (TRUE) or not (FALSE). If TRUE, a
#'   traditional independent two-samples t-test is computed; if FALSE, Welch's
#'   t-test is computed. These two tests differ by their degrees of freedom and
#'   p-values.
#'
#' @param d.ci.type character vector with length 1 specifying the type of
#'   confidence intervals to compute for the standardized mean difference (i.e.,
#'   Cohen's d). There are currently three options: 1) "unbiased" which
#'   calculates the unbiased standard error of Cohen's d based on formula 25 in
#'   Viechtbauer (2007). A symmetrical confidence interval is then calculated
#'   based on the standard error. 2) "tdist" which calculates the confidence
#'   intervals based on the t-distribution using the function
#'   \code{\link[psych]{cohen.d.ci}}, 3) "classic" which calculates the
#'   confidence interval of Cohen's d based on the confidence interval of the
#'   mean difference itself. The lower and upper confidence bounds are divided
#'   by the pooled standard deviation. Technically, this confidence interval is
#'   biased due to not taking into account the uncertainty of the standard
#'   deviations. No standard error is calculated for this option and NA is
#'   returned for "d_se" in the return object.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if
#'   \code{data[[bin.nm]]} has more than 2 unique values (other than missing
#'   values) or if \code{bin.nm} is not a colname in \code{data}. This is a
#'   tradeoff between computational efficiency (FALSE) and more useful error
#'   messages (TRUE).
#'
#' @return list of data.frames vectors containing statistical information about
#'   the mean differences (the rownames of each data.frame are \code{vrb.nm}):
#'   1) nhst = independent two-samples t-test stat info in a data.frame,
#'   2) desc = descriptive statistics stat info in a data.frame,
#'   3) std = standardized mean difference stat info in a data.frame
#'
#' 1) nhst = independent two-samples t-test stat info in a data.frame
#'
#' \describe{
#'    \item{est}{mean difference estimate (i.e., group 2 - group 1)}
#'    \item{se}{standard error}
#'    \item{t}{t-value}
#'    \item{df}{degrees of freedom}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a data.frame
#'
#' \describe{
#'    \item{mean_`lvl[2]`}{mean of group 2}
#'    \item{mean_`lvl[1]`}{mean of group 1}
#'    \item{sd_`lvl[2]`}{standard deviation of group 2}
#'    \item{sd_`lvl[1]`}{standard deviation of group 1}
#'    \item{n_`lvl[2]`}{sample size of group 2}
#'    \item{n_`lvl[1]`}{sample size of group 1}
#' }
#'
#' 3) std = standardized mean difference stat info in a data.frame
#'
#' \describe{
#'    \item{d_est}{Cohen's d estimate}
#'    \item{d_se}{Cohen's d standard error}
#'    \item{d_lwr}{Cohen's d lower bound of the confidence interval}
#'    \item{d_upr}{Cohen's d upper bound of the confidence interval}
#' }
#'
#' @references
#'
#'    McGrath, R. E., & Meyer, G. J. (2006). When effect sizes disagree: the case of
#'    r and d. Psychological Methods, 11(4), 386-401.
#'
#'    Viechtbauer, W. (2007). Approximate confidence intervals for standardized
#'    effect sizes in the two-independent and two-dependent samples design.
#'    Journal of Educational and Behavioral Statistics, 32(1), 39-60.
#'
#' @seealso
#'    \code{\link{means_diff}} for independent two-sample t-test of a single variable,
#'    \code{\link[stats]{t.test}} the workhorse for \code{mean_diff},
#'    \code{\link[psych]{cohen.d}} for another standardized mean difference function,
#'    \code{\link{means_change}} for dependent two-sample t-tests,
#'    \code{\link{means_test}} for one-sample t-tests,
#'
#' @examples
#'
#' # independent two-samples t-tests
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs")
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs",
#'    d.ci.type = "classic")
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs",
#'    lvl = c("1","0")) # signs are reversed
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs",
#'    lvl = c(1,0)) # can provide numeric levels for dummy variables
#'
#' # compare to psych::cohen.d()
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs",
#'    d.ci.type = "tdist")
#' tmp_nm <- c("mpg","cyl","disp","vs") # so that Roxygen2 doesn't freak out
#' cohend_obj <- psych::cohen.d(mtcars[tmp_nm], group = "vs")
#' as.data.frame(cohend_obj[["cohen.d"]]) # different estimate of cohen's d
#'    # of course, this also leads to different confidence interval bounds as well
#'
#' # same as intercept-only regression when var.equal = TRUE
#' means_diff(data = mtcars, vrb.nm = "mpg", bin.nm = "vs")
#' lm_obj <- lm(mpg ~ vs, data = mtcars)
#' coef(summary(lm_obj))
#'
#' # if levels are not unique values in data[[bin.nm]]
#' \dontrun{
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs",
#'    lvl = c("zero", "1")) # an error message is returned
#' means_diff(data = mtcars, vrb.nm = c("mpg","cyl","disp"), bin.nm = "vs",
#'    lvl = c("0", "one")) # an error message is returned
#' }
#'
#' @export
means_diff <- function(data, vrb.nm, bin.nm, lvl = levels(as.factor(data[[bin.nm]])),
   var.equal = TRUE, d.ci.type = "unbiased", ci.level = 0.95, check = TRUE) {

   # error checking
   if (check) {
      checkmate::assertNames(vrb.nm, subset.of = names(data))
      checkmate::assertNames(bin.nm, subset.of = names(data))
      if (length(na.omit(unique(data[[bin.nm]]))) != 2)
         stop("`data`[[`bin.nm`]] must contain exactly two unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, len = 2, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(data[[bin.nm]])))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `data`[[`bin.nm`]] (other than missing values)")
      checkmate::assertLogical(var.equal, len = 1, any.missing = FALSE)
      d.ci.type <- match.arg(d.ci.type, choices = c("unbiased","tdist","classic"))
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
   }

   # function itself
   tmp <- lapply(X = str2str::sn(vrb.nm), FUN = function(nm) {
      mean_diff(x = data[[nm]], bin = data[[bin.nm]], lvl = lvl, var.equal = var.equal,
         d.ci.type = d.ci.type, ci.level = ci.level, check = FALSE)
   })
   if (1 == length(vrb.nm)) {
      tmp2 <- tmp[[1]]
      rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
   } else {
      tmp2 <- str2str::t_list(tmp)
      rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
   }
   return(rtn)
}

# mean_compare

#' Mean differences for a single variable across 3+ independent groups (one-way
#' ANOVA)
#'
#' \code{mean_compare} compares means across 3+ independent groups with a
#' one-way ANOVA. The function also calculates the descriptive statistics for
#' each group and the variance explained (i.e., R^2 aka eta^2) by the nominal
#' grouping variable. \code{mean_compare} is simply a wrapper for
#' \code{\link[stats]{oneway.test}} plus some extra calculations.
#' \code{mean_compare} will work with 2 independent groups; however it arguably
#' makes more sense to use \code{\link{mean_diff}} in that case.
#'
#' @param x numeric vector.
#'
#' @param nom atomic vector (e.g., factor) the same length as \code{x} that is a
#'   nominal variable. It identifies the 3+ groups with 3+ unique values (other
#'   than missing values).
#'
#' @param lvl character vector with length 3+ specifying the unique values for
#'   the 3+ groups. If \code{nom} is a factor, then \code{lvl} should be the
#'   factor levels rather than the underlying integer codes. This argument
#'   allows you to specify the order of the descriptive statistics in the return
#'   object, which will be opposite the order of \code{lvl} for consistency with
#'   \code{\link{mean_diff}} and \code{\link{mean_change}}.
#'
#' @param var.equal logical vector of length 1 specifying whether the variances
#'   of the groups are assumed to be equal (TRUE) or not (FALSE). If TRUE, a
#'   traditional one-way ANOVA is computed; if FALSE, Welch's ANOVA is computed.
#'   These two tests differ by their denominator degrees of freedom, F-value,
#'   and p-value.
#'
#' @param r2.ci.type character vector with length 1 specifying the type of
#'   confidence intervals to compute for the variance explained (i.e., R^2 aka
#'   eta^2). There are currently two options: 1) "Fdist" which calculates a
#'   non-symmetrical confidence interval based on the non-central F distribution
#'   (pg. 38, Smithson, 2003), 2) "classic" which calculates the confidence
#'   interval based on a large-sample theory standard error (eq. 3.6.3 in Cohen,
#'   Cohen, West, & Aiken, 2003), which is taken from Olkin & Finn (1995) - just
#'   above eq. 10. The confidence intervals for R^2-adjusted use the same
#'   formula as R^2, but replace R^2 with R^2 adjusted. Technically, the R^2
#'   adjusted confidence intervals can have poor coverage (pg. 54, Smithson,
#'   2003)
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of length 1 specifying whether the traditional
#'   ANOVA table should be returned as the last element of the return object.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{nom} has
#'   length different than the length of \code{x}. This is a tradeoff between
#'   computational efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   mean comparison: 1) nhst = one-way ANOVA stat info in a numeric vector,
#'   2) desc = descriptive statistics stat info in a numeric vector,
#'   3) std = standardized effect sizes stat info in a numeric vector,
#'   4) anova = traditional ANOVA table in a numeric matrix (only returned
#'   if rtn.table = TRUE).
#'
#' 1) nhst = one-way ANOVA stat info in a numeric vector
#'
#' \describe{
#'    \item{diff_avg}{average mean difference across group pairs}
#'    \item{se}{NA to remind the user there is no standard error for the average mean difference}
#'    \item{F}{F-value}
#'    \item{df_num}{numerator degrees of freedom}
#'    \item{df_den}{denominator degrees of freedom}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector (note there
#' could be more than 3 groups - groups i, j, and k are just provided as an example)
#'
#' \describe{
#'    \item{mean_`lvl[k]`}{mean of group k}
#'    \item{mean_`lvl[j]`}{mean of group j}
#'    \item{mean_`lvl[i]`}{mean of group i}
#'    \item{sd_`lvl[k]`}{standard deviation of group k}
#'    \item{sd_`lvl[j]`}{standard deviation of group j}
#'    \item{sd_`lvl[i]`}{standard deviation of group i}
#'    \item{n_`lvl[k]`}{sample size of group k}
#'    \item{n_`lvl[j]`}{sample size of group j}
#'    \item{n_`lvl[i]`}{sample size of group i}
#' }
#'
#' 3) std = standardized effect sizes stat info in a numeric vector
#'
#' \describe{
#'    \item{r2_reg_est}{R^2 estimate}
#'    \item{r2_reg_se}{R^2 standard error (only available if \code{r2.ci.type} = "classic")}
#'    \item{r2_reg_lwr}{R^2 lower bound of the confidence interval}
#'    \item{r2_reg_upr}{R^2 upper bound of the confidence interval}
#'    \item{r2_adj_est}{R^2-adjusted estimate}
#'    \item{r2_adj_se}{R^2-adjusted standard error (only available if \code{r2.ci.type} = "classic")}
#'    \item{r2_adj_lwr}{R^2-adjusted lower bound of the confidence interval}
#'    \item{r2_adj_upr}{R^2-adjusted upper bound of the confidence interval}
#' }
#'
#' 4) anova = traditional ANOVA table in a numeric matrix (only returned
#' if rtn.table = TRUE).
#'
#' The dimlabels of the matrix was "effect" for the rows
#' and "info" for the columns. There are two rows with rownames 1. "nom" and 2.
#' "Residuals" where "nom" refers to the between-group effect of the nominal
#' variable and "Residuals" refers to the within-group residual errors. There
#' are 5 columns with colnames 1. "SS" = sum of squares, 2. "df" = degrees of
#' freedom, 3. "MS" = mean squares, 4. "F" = F-value. and 5. "p" = p-value. Note
#' the F-value and p-value will differ from the "nhst" returned vector if
#' \code{var.equal} = FALSE because the traditional ANOVA table always assumes
#' variances are equal (i.e. \code{var.equal} = TRUE).
#'
#' @references
#'
#'    Cohen, J., Cohen, P., West, A. G., & Aiken, L. S. (2003). Applied Multiple
#'    Regression/Correlation Analysis for the Behavioral Science - third edition.
#'    New York, NY: Routledge.
#'
#'    Olkin, I., & Finn, J. D. (1995). Correlations redux. Psychological Bulletin, 118(1), 155-164.
#'
#'    Smithson, M. (2003). Confidence intervals. Thousand Oaks, CA: Sage Publications.
#'
#' @seealso
#'    \code{\link[stats]{oneway.test}} the workhorse for \code{mean_compare},
#'    \code{\link{means_compare}} for multiple variables across the same 3+ groups,
#'    \code{\link[MBESS]{ci.R2}} for confidence intervals of the variance explained,
#'    \code{\link{mean_diff}} for a single variable across only 2 groups,
#'
#' @examples
#'
#' mean_compare(x = mtcars$"mpg", nom = mtcars$"gear")
#' mean_compare(x = mtcars$"mpg", nom = mtcars$"gear", var.equal = FALSE)
#' mean_compare(x = mtcars$"mpg", nom = mtcars$"gear", rtn.table = FALSE)
#' mean_compare(x = mtcars$"mpg", nom = mtcars$"gear", r2.ci.type = "classic")
#'
#' @export
mean_compare <- function(x, nom, lvl = levels(as.factor(nom)),
   var.equal = TRUE, r2.ci.type = "Fdist", ci.level = 0.95,
   rtn.table = TRUE, check = TRUE) {

   # error checking
   if (check) {
      if (length(x) != length(nom))
         stop("`x` and `nom` must have the same length")
      checkmate::assertNumeric(x)
      if (length(na.omit(unique(nom))) < 2)
         stop("`nom` must contain exactly 2+ unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(nom)))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `nom` (other than missing values)")
      checkmate::assertLogical(var.equal, len = 1, any.missing = FALSE)
      r2.ci.type <- match.arg(r2.ci.type, choices = c("Fdist","classic"))
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # complete.cases
   lvl <- as.character(lvl) # so user doesn't have to convert to character themselves
   nom <- factor(nom, levels = lvl) # works even if bin already is a factor
   dat0 <- data.frame(x, nom)
   dat <- dat0[complete.cases(dat0), ]

   # oneway.test
   oneway_obj <- oneway.test(x ~ nom, data = dat, var.equal = var.equal)
   F_value <- oneway_obj[["statistic"]]
   df_vec <- oneway_obj[["parameter"]]
   p_value <- oneway_obj[["p.value"]]
   oneway_vec <- setNames(unname(c(F_value, df_vec, p_value)),
      nm = c("F","df_num","df_den","p"))

   # descriptives
   mean_by <- agg(x = dat$"x", grp = dat$"nom", rep = FALSE, rtn.grp = FALSE, fun = mean)
   sd_by <- agg(x = dat$"x", grp = dat$"nom", rep = FALSE, rtn.grp = FALSE, fun = sd)
   n_by <- agg(x = dat$"x", grp = dat$"nom", rep = FALSE, rtn.grp = FALSE, fun = length)
   names(mean_by) <- paste0("mean_", lvl)
   names(sd_by) <- paste0("sd_", lvl)
   names(n_by) <- paste0("n_", lvl)
   desc_vec <- c(rev(mean_by), rev(sd_by), rev(n_by))
   diff_mat <- outer(X = mean_by, Y = mean_by, FUN = `-`)
   diff_avg <- mean(abs(diff_mat[lower.tri(diff_mat)]))
   str2str::append(oneway_vec, after = 0L) <- c("diff_avg" = diff_avg, "se" = NA_real_)

   # r-squared
   aov_obj <- aov(x ~ nom, data = dat)
   r2reg_est <- summary.lm(aov_obj)[["r.squared"]]
   r2adj_est <- summary.lm(aov_obj)[["adj.r.squared"]]
   n_total <- sum(n_by)
   k <- length(n_by)
   if (r2.ci.type == "classic") {
      num_reg <- (4 * r2reg_est) * (1 - r2reg_est)^2 * (n_total - k - 1)^2
      den_reg <- (n_total^2 - 1) * (n_total + 3)
      r2reg_se <- sqrt(num_reg / den_reg)
      r2reg_vec <- str2str::d2v(confint2.default(obj = r2reg_est, se = r2reg_se,
         df = oneway_vec["df_den"], level = ci.level))
      num_adj <- (4 * r2adj_est) * (1 - r2adj_est)^2 * (n_total - k - 1)^2
      den_adj <- (n_total^2 - 1) * (n_total + 3)
      r2adj_se <- sqrt(num_adj / den_adj)
      r2adj_vec <- str2str::d2v(confint2.default(obj = r2adj_est, se = r2adj_se,
         df = oneway_vec["df_den"], level = ci.level))
      r2_vec <- setNames(c(r2reg_vec, r2adj_vec), nm = c("r2_reg_est","r2_reg_se",
         "r2_reg_lwr","r2_reg_upr","r2_adj_est","r2_adj_se","r2_adj_lwr","r2_adj_upr"))
   }
   if (r2.ci.type == "Fdist") {
      ciR2reg_obj <- MBESS::ci.R2(R2 = r2reg_est, N = n_total, p = k - 1, # number of predictors (aka dummy variables) = k - 1
         conf.level = ci.level, Random.Predictors = FALSE)
      r2reg_ci <- c(ciR2reg_obj[["Lower.Conf.Limit.R2"]], ciR2reg_obj[["Upper.Conf.Limit.R2"]])
      r2reg_vec <- setNames(c(r2reg_est, NA, r2reg_ci),
         nm = c("r2_reg_est","r2_reg_se","r2_reg_lwr","r2_reg_upr"))
      ciR2adj_obj <- MBESS::ci.R2(R2 = r2adj_est, N = n_total, p = k - 1, # number of predictors (aka dummy variables) = k - 1
         conf.level = ci.level, Random.Predictors = FALSE)
      r2adj_ci <- c(ciR2adj_obj[["Lower.Conf.Limit.R2"]], ciR2adj_obj[["Upper.Conf.Limit.R2"]])
      r2adj_vec <- setNames(c(r2adj_est, NA, r2adj_ci),
         nm = c("r2_adj_est","r2_adj_se","r2_adj_lwr","r2_adj_upr"))
      r2_vec <- c(r2reg_vec, r2adj_vec)
   }

   # return object
   rtn <- list("nhst" = oneway_vec, "desc" = desc_vec, "std" = r2_vec)
   if (rtn.table) {
      tmp <- summary.aov(aov_obj)[[1]]
      aov_tab <- tmp[c(2, 1, 3, 4, 5)] # so columns in usual anova table order
      names(aov_tab) <- c("SS","df","MS","F","p")
      row.names(aov_tab) <- c("nom","Residuals")
      aov_mat <- str2str::d2m(aov_tab) # matrix so means_compare() can be a 3D array
      str2str::dimlabels(aov_mat) <- c("effect","info")
      str2str::append(rtn) <- list("anova" = aov_mat)
   }
   return(rtn)
}

# means_compare

#' Mean differences for multiple variables across 3+ independent groups (one-way
#' ANOVAs)
#'
#' \code{means_compare} compares means across 3+ independent groups with a
#' separate one-way ANOVA for each variable. The function also calculates the
#' descriptive statistics for each group and the variance explained (i.e., R^2 -
#' aka eta^2) by the nominal grouping variable. \code{means_compare} is simply a
#' wrapper for \code{\link[stats]{oneway.test}} plus some extra calculations.
#' \code{mean_compare} will work with 2 independent groups; however it arguably
#' makes more sense to use \code{\link{mean_diff}} in that case.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of length 1 with colnames from \code{data}
#'   specifying the variables.
#'
#' @param nom.nm character vector of length 1 with colnames from \code{data}
#'   specifying the nominal variable. It identifies the 3+ groups with 3+ unique values (other
#'   than missing values).
#'
#' @param lvl character vector with length 3+ specifying the unique values for
#'   the 3+ groups. If \code{nom} is a factor, then \code{lvl} should be the
#'   factor levels rather than the underlying integer codes. This argument
#'   allows you to specify the order of the descriptive statistics in the return
#'   object, which will be opposite the order of \code{lvl} for consistency with
#'   \code{\link{mean_diff}} and \code{\link{mean_change}}.
#'
#' @param var.equal logical vector of length 1 specifying whether the variances
#'   of the groups are assumed to be equal (TRUE) or not (FALSE). If TRUE, a
#'   traditional one-way ANOVA is computed; if FALSE, Welch's ANOVA is computed.
#'   These two tests differ by their denominator degrees of freedoms, F-values,
#'   and p-values.
#'
#' @param r2.ci.type character vector with length 1 specifying the type of
#'   confidence intervals to compute for the variance explained (i.e., R^2 or
#'   eta^2). There are currently two options: 1) "Fdist" which calculates a
#'   non-symmetrical confidence interval based on the non-central F distribution
#'   (pg. 38, Smithson, 2003), 2) "classic" which calculates the confidence
#'   interval based on a large-sample theory standard error (eq. 3.6.3 in Cohen,
#'   Cohen, West, & Aiken, 2003), which is taken from Olkin & Finn (1995) - just
#'   above eq. 10. The confidence intervals for R^2-adjusted use the same
#'   formula as R^2, but replace R^2 with R^2 adjusted. Technically, the R^2
#'   adjusted confidence intervals can have poor coverage (pg. 54, Smithson,
#'   2003)
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of length 1 specifying whether the traditional
#'   ANOVA tables should be returned as the last element of the return object.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{vrb.nm} are
#'   not colnames within \code{data}. This is a tradeoff between computational
#'   efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of data.frames containing statistical information about the mean
#'   comparisons for each variable (the rows of the data.frames are
#'   \code{vrb.nm}): 1) nhst = one-way ANOVA stat info in a data.frame,
#'   2) desc = descriptive statistics stat info in a data.frame,
#'   3) std = standardized effect sizes stat info in a data.frame,
#'   4) anova = traditional ANOVA table in a numeric 3D array (only
#'   returned if rtn.table = TRUE)
#'
#' 1) nhst = one-way ANOVA stat info in a data.frame
#'
#' \describe{
#'    \item{diff_avg}{average mean difference across group pairs}
#'    \item{se}{NA to remind the user there is no standard error for the average mean difference}
#'    \item{F}{F-value}
#'    \item{df_num}{numerator degrees of freedom}
#'    \item{df_den}{denominator degrees of freedom}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) desc = descriptive statistics stat info in a data.frame (note there
#' could be more than 3 groups - groups i, j, and k are just provided as an example)
#'
#' \describe{
#'    \item{mean_`lvl[k]`}{mean of group k}
#'    \item{mean_`lvl[j]`}{mean of group j}
#'    \item{mean_`lvl[i]`}{mean of group i}
#'    \item{sd_`lvl[k]`}{standard deviation of group k}
#'    \item{sd_`lvl[j]`}{standard deviation of group j}
#'    \item{sd_`lvl[i]`}{standard deviation of group i}
#'    \item{n_`lvl[k]`}{sample size of group k}
#'    \item{n_`lvl[j]`}{sample size of group j}
#'    \item{n_`lvl[i]`}{sample size of group i}
#' }
#'
#' 3) std = standardized effect sizes stat info in a data.frame
#'
#' \describe{
#'    \item{r2_reg_est}{R^2 estimate}
#'    \item{r2_reg_se}{R^2 standard error (only available if \code{r2.ci.type} = "classic")}
#'    \item{r2_reg_lwr}{R^2 lower bound of the confidence interval}
#'    \item{r2_reg_upr}{R^2 upper bound of the confidence interval}
#'    \item{r2_adj_est}{R^2-adjusted estimate}
#'    \item{r2_adj_se}{R^2-adjusted standard error (only available if \code{r2.ci.type} = "classic")}
#'    \item{r2_adj_lwr}{R^2-adjusted lower bound of the confidence interval}
#'    \item{r2_adj_upr}{R^2-adjusted upper bound of the confidence interval}
#' }
#'
#' 4) anova = traditional ANOVA table in a numeric 3D array (only
#' returned if rtn.table = TRUE).
#'
#' The dimlabels of the array are "effect" for
#' the rows, "info" for the columns, and "vrb" for the layers. There are two
#' rows with rownames 1. "nom" and 2. "Residuals" where "nom" refers to the
#' between-group effect of the nominal variable and "Residuals" refers to the
#' within-group residual errors. There are 5 columns with colnames 1. "SS" = sum
#' of squares, 2. "df" = degrees of freedom, 3. "MS" = mean squares, 4. "F" =
#' F-value. and 5. "p" = p-value. Note the F-value and p-value will differ from
#' the "nhst" returned vector if \code{var.equal} = FALSE because the
#' traditional ANOVA table always assumes variances are equal (i.e.
#' \code{var.equal} = TRUE). There are as many layers as \code{length(vrb.nm)}
#' with the laynames equal to \code{vrb.nm}.
#'
#' @references
#'
#'    Cohen, J., Cohen, P., West, A. G., & Aiken, L. S. (2003). Applied Multiple
#'    Regression/Correlation Analysis for the Behavioral Science - third edition.
#'    New York, NY: Routledge.
#'
#'    Olkin, I., & Finn, J. D. (1995). Correlations redux. Psychological Bulletin, 118(1), 155-164.
#'
#'    Smithson, M. (2003). Confidence intervals. Thousand Oaks, CA: Sage Publications.
#'
#' @seealso
#'    \code{\link[stats]{oneway.test}} the workhorse for \code{means_compare},
#'    \code{\link{mean_compare}} for a single variable across the same 3+ groups,
#'    \code{\link[MBESS]{ci.R2}} for confidence intervals of the variance explained,
#'    \code{\link{means_diff}} for multiple variables across only 2 groups,
#'
#' @examples
#'
#' means_compare(mtcars, vrb.nm = c("mpg","wt","qsec"), nom.nm = "gear")
#' means_compare(mtcars, vrb.nm = c("mpg","wt","qsec"), nom.nm = "gear",
#'    var.equal = FALSE)
#' means_compare(mtcars, vrb.nm = c("mpg","wt","qsec"), nom.nm = "gear",
#'    rtn.table = FALSE)
#' means_compare(mtcars, vrb.nm = "mpg", nom.nm = "gear")
#'
#' @export
means_compare <- function(data, vrb.nm, nom.nm, lvl = levels(as.factor(data[[nom.nm]])),
   var.equal = TRUE, r2.ci.type = "classic", ci.level = 0.95, rtn.table = TRUE,
   check = TRUE) {

   # error checking
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertNames(vrb.nm, subset.of = names(data))
      checkmate::assertNames(nom.nm, subset.of = names(data))
      if (length(na.omit(unique(data[[nom.nm]]))) < 2)
         stop("`data`[[`nom.nm`]] must contain two or more unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(data[[nom.nm]])))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `data`[[`nom.nm`]] (other than missing values)")
      checkmate::assertLogical(var.equal, len = 1, any.missing = FALSE)
      r2.ci.type <- match.arg(r2.ci.type, choices = "classic")
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # function itself
   tmp <- lapply(X = str2str::sn(vrb.nm), FUN = function(nm) {
      mean_compare(x = data[[nm]], nom = data[[nom.nm]], lvl = lvl, var.equal = var.equal,
         r2.ci.type = r2.ci.type, ci.level = ci.level, rtn.table = rtn.table, check = FALSE)
   })
   if (!rtn.table) { # rtn.table = FALSE
      if (1 == length(vrb.nm)) {
         tmp2 <- tmp[[1]]
         rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
      } else {
         tmp2 <- str2str::t_list(tmp)
         rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
      }
   } else { # rtn.table = TRUE
      if (1 == length(vrb.nm)) {
         tmp2 <- tmp[[1]]
         rtn_front <- lapply(X = tmp2[1:3], FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
         rtn <- c(rtn_front, tmp2[4])
      } else {
         tmp2 <- str2str::t_list(tmp)
         rtn_front <- lapply(X = tmp2[1:3], FUN = str2str::lv2d, along = 1)
         rtn_back <- lapply(X = tmp2[4], FUN = str2str::lm2a, dim.order = c(1, 2, 3), dimlab.list = "vrb")
         rtn <- c(rtn_front, rtn_back)
      }
   }
   return(rtn)
}

# PROP DIFFERENCES ####

# prop_test #

#' Test for Sample Proportion Against Pi (chi-square test of goodness of fit)
#'
#' \code{prop_test} tests for a sample proportion difference from a population
#' proportion with a chi-square test of goodness of fit. The default is that the
#' goodness of fit is consistent with a population proportion Pi of 0.50. The
#' function also calculates the descriptive statistics, various standardized
#' effect sizes (e.g., Cramer's V), and can provide the 1x2 contingency tables.
#' \code{prop_test} is simply a wrapper for \code{\link[stats]{prop.test}} plus
#' some extra calculations.
#'
#' @param x numeric vector that only has values of 0 or 1 (or missing values),
#'   otherwise known as a dummy variable.
#'
#' @param pi numeric vector of length 1 specifying the population proportion
#'   value to compare the sample proportion against.
#'
#' @param yates logical vector of length 1 specifying whether the Yate's
#'   continuity correction should be applied for small samples. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of lengh 1 specifying whether the return
#'   object should include the 1x2 contingency table of counts with totals and
#'   the 1x2 overall percentages table. If TRUE, then the last two elements of
#'   the return object are "count" containing a vector of counts and "percent"
#'   containing a vector of overall percentages.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{x} is a dummy
#'   variable that only takes on value of 0 or 1 (or missing values). This is a
#'   tradeoff between computational efficiency (FALSE) and more useful error
#'   messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   proportion difference from pi: 1) nhst = chi-square test of goodness of fit stat
#'   info in a numeric vector, 2) desc = descriptive statistics stat info in a
#'   numeric vector, 3) std = various standardized effect sizes in a numeric vector,
#'   4) count = numeric vector of length 3 with table of counts with an additional
#'   element for the total (if \code{rtn.table} = TRUE), 5) percent = numeric vector
#'   of length 3 with table of overall percentages with an element for the total
#'   (if \code{rtn.table} = TRUE)
#'
#' 1) nhst = chi-square test of goodness of fit stat info in a numeric vector
#'
#' \describe{
#'    \item{est}{proportion difference estimate (i.e., sample proportion - pi)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (will always be 1)}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector
#'
#' \describe{
#'    \item{prop}{sample proportion}
#'    \item{pi}{popularion proportion provided by the user (or 0.50 by default)}
#'    \item{sd}{standard deviation}
#'    \item{n}{sample size}
#'    \item{lwr}{lower bound of the confidence interval of the sample proportion itself}
#'    \item{upr}{upper bound of the confidence interval of the sample proportion itself}
#' }
#'
#' 3) std = various standardized effect sizes in a numeric vector
#'
#' \describe{
#'    \item{cramer}{Cramer's V estimate}
#'    \item{h}{Cohen's h estimate}
#' }
#'
#' 4) count = numeric vector of length 3 with table of counts with an additional
#' element for the total (if \code{rtn.table} = TRUE). The names are 1. "0", 2.
#' "1", 3. "total"
#'
#' 5) percent = numeric vector of length 3 with table of overall percentages with
#' an element for the total (if \code{rtn.table} = TRUE). The names are 1. "0", 2.
#' "1", 3. "total"
#'
#' @seealso
#'    \code{\link[stats]{prop.test}} the workhorse for \code{prop_test},
#'    \code{\link{props_test}} for multiple dummy variables,
#'    \code{\link{prop_diff}} for chi-square test of independence,
#'
#' @examples
#'
#' # chi-square test of goodness of fit
#' table(mtcars$"am")
#' prop_test(mtcars$"am")
#' prop_test(ifelse(mtcars$"am" == 1, yes = 0, no = 1))
#'
#' # different than intercept only logistic regression
#' summary(glm(am ~ 1, data = mtcars, family = binomial(link = "logit")))
#'
#' # error from non-dummy variable
#' \dontrun{
#' prop_test(ifelse(mtcars$"am" == 1, yes = "1", no = "0"))
#' prop_test(ifelse(mtcars$"am" == 1, yes = 2, no = 1))
#' }
#'
#' @export
prop_test <- function(x, pi = 0.50, yates = TRUE, ci.level = 0.95, rtn.table = TRUE,
   check = TRUE) {

   # errors
   if (check) {
      checkmate::assertNumeric(x)
      if(!(str2str::is.dummy(x)))
         stop("`x` must be a dummy variable that only has values 0 or 1 (or missing values)")
      checkmate::assertNumeric(pi, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(yates, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # prop.test
   table_x <- table(x)
   table4prop <- rev(table_x)
   prop_obj <- prop.test(x = table4prop, p = pi, conf.level = ci.level, correct = yates)
   prop_est <- unname(prop_obj[["estimate"]])
   prop_vec <- c("est" = prop_est - unname(prop_obj[["null.value"]]), # sample - population
      "se" = NA_real_,
      "X2" = unname(prop_obj[["statistic"]]),
      "df" = unname(prop_obj[["parameter"]]),
      "p" = prop_obj[["p.value"]])

   # descriptives
   sd_est <- sd(x, na.rm = TRUE)
   n <- length(na.omit(x))
   lwr <- prop_obj[["conf.int"]][1]
   upr <- prop_obj[["conf.int"]][2]
   desc_vec <- c("prop" = prop_est, "pi" = pi, "sd" = sd_est, "n" = n, "lwr" = lwr, "upr" = upr)

   # effect sizes
   cramer <- unname(sqrt(prop_vec["X2"] / n))
   h_prop <- 2 * asin(sqrt(prop_est))
   h_pi <- 2 * asin(sqrt(pi))
   h_est <- unname(h_prop - h_pi)
   effects_vec <- c("cramer" = cramer, "h" = h_est)

   # return object
   rtn <- list("nhst" = prop_vec, "desc" = desc_vec, "std" = effects_vec)
   if (rtn.table) {
      count <- unclass(str2str::undim(table_x))
      str2str::append(count) <- c("total" = sum(count))
      percent <- count / n
      str2str::append(rtn) <- list("count" = count, "percent" = percent)
   }
   return(rtn)
}

# props_test #

#' Test for Multiple Sample Proportion Against Pi (Chi-square Tests of Goodness
#' of Fit)
#'
#' \code{props_test} tests for multiple sample proportion difference from
#' population proportions with chi-square tests of goodness of fit. The default
#' is that the goodness of fit is consistent with a population proportion Pi of
#' 0.50. The function also calculates the descriptive statistics, various
#' standardized effect sizes (e.g., Cramer's V), and can provide the 1x2
#' contingency tables. \code{props_test} is simply a wrapper for
#' \code{\link[stats]{prop.test}} plus some extra calculations.
#'
#' @param data data.frame of data.
#'
#' @param dum.nm character vector of length 1 specifying the colnames in
#'   \code{data} of the variables used to calculate the proportions. The
#'   variables must only have values of 0 or 1 (or missing values), or be
#'   otherwise known as dummy variables. See \code{\link[str2str]{is.dummy}}.
#'
#' @param pi numeric vector of length = \code{length(dum.nm)} or length 1
#'   specifying the population proportion values to compare the sample
#'   proportions against. The order of the values should be the same as the
#'   order in \code{dum.nm}. When length 1, the same population proportion value
#'   is used for all the variables.
#'
#' @param yates logical vector of length 1 specifying whether the Yate's
#'   continuity correction should be applied for small samples. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of lengh 1 specifying whether the return
#'   object should include the rbinded 1x2 contingency table of counts with
#'   totals and the rbinded 1x2 overall percentages table. If TRUE, then the
#'   last two elements of the return object are "count" containing a data.frame
#'   of counts and "percent" containing a data.frame of overall percentages.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{data[dum.nm]}
#'   are all dummy variables that only take on values of 0 or 1 (or missing
#'   values). This is a tradeoff between computational efficiency (FALSE) and
#'   more useful error messages (TRUE).
#'
#' @return list of data.frames containing statistical information about the
#'   proportion differences from pi: 1) nhst = chi-square test of goodness of fit
#'   stat info in a data.frame, 2) desc = descriptive statistics stat info in a
#'   data.frame, 3) std = various standardized effect sizes in a data.frame,
#'   4) count = data.frame containing the rbinded 1x2 tables of counts with an additional
#'   column for the total (if \code{rtn.table} = TRUE), 5) percent = data.frame
#'   containing the rbinded 1x2 tables of overall percentages with an additional
#'   column for the total (if \code{rtn.table} = TRUE)
#'
#' 1) nhst = chi-square test of goodness of fit stat info in a data.frame
#'
#' \describe{
#'    \item{est}{proportion difference estimate (i.e., sample proportion - pi)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (will always be 1)}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) desc = descriptive statistics stat info in a data.frame
#'
#' \describe{
#'    \item{prop}{sample proportion}
#'    \item{pi}{popularion proportion provided by the user (or 0.50 by default)}
#'    \item{sd}{standard deviation}
#'    \item{n}{sample size}
#'    \item{lwr}{lower bound of the confidence interval of the sample proportion itself}
#'    \item{upr}{upper bound of the confidence interval of the sample proportion itself}
#' }
#'
#' 3) std = various standardized effect sizes in a data.frame
#'
#' \describe{
#'    \item{cramer}{Cramer's V estimate}
#'    \item{h}{Cohen's h estimate}
#' }
#'
#' 4) count = data.frame containing the rbinded 1x2 tables of counts with an additional
#' column for the total (if \code{rtn.table} = TRUE). The colnames are 1.
#' "0", 2. "1", 3. "total"
#'
#' 5) percent = data.frame containing the rbinded 1x2 tables of overall percentages
#' with an additional column for the total (if \code{rtn.table} = TRUE). The
#' colnames are 1. "0", 2. "1", 3. "total"
#'
#' @seealso
#'
#'    \code{\link[stats]{prop.test}} the workhorse for \code{prop_test},
#'    \code{\link{prop_test}} for a single dummy variables,
#'    \code{\link{props_diff}} for chi-square tests of independence,
#'
#' @examples
#'
#' # multiple variables
#' mtcars2 <- mtcars
#' mtcars2$"gear_dum" <- ifelse(mtcars2$"gear" > 3, yes = 1L, no = 0L)
#' mtcars2$"carb_dum" <- ifelse(mtcars2$"carb" > 3, yes = 1L, no = 0L)
#' vrb_nm <- c("am","gear_dum","carb_dum") # dummy variables
#' lapply(X = vrb_nm, FUN = function(nm) {
#'    table(mtcars2[nm])
#' })
#' props_test(data = mtcars2, dum.nm = c("am","gear_dum","carb_dum"))
#' props_test(data = mtcars2, dum.nm = c("am","gear_dum","carb_dum"),
#'    rtn.table = FALSE)
#'
#' # single variable
#' props_test(data = mtcars2, dum.nm = "am")
#' props_test(data = mtcars2, dum.nm = "am", rtn.table = FALSE)
#'
#' # error from non-dummy variables
#' \dontrun{
#' props_test(data = mtcars2, dum.nm = c("am","gear","carb"))
#' }
#'
#' @export
props_test <- function(data, dum.nm, pi = 0.5, yates = TRUE, ci.level = 0.95,
   rtn.table = TRUE, check = TRUE) {

   # error checking
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertNames(dum.nm, subset.of = names(data))
      if(any(unlist(lapply(X = data[dum.nm], FUN = str2str::is.dummy)) == FALSE))
         stop("every variable in `data`[`dum.nm`] must be a dummy variable - see help(str2str::is.dummy)")
      if (1 != length(pi) && length(dum.nm) != length(pi))
         stop("`pi` must have length 1 or length equal to the length of `dum.nm`")
      checkmate::assertNumeric(pi, lower = 0, upper = 1, any.missing = FALSE)
      checkmate::assertLogical(yates, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # function itself
   if (length(pi) == 1) pi <- rep.int(x = pi, times = length(dum.nm))
   tmp <- Map(nm = str2str::sn(dum.nm), pop_prop = pi, f = function(nm, pop_prop) {
      prop_test(x = data[[nm]], pi = pop_prop, yates = yates, ci.level = ci.level,
         rtn.table = rtn.table, check = FALSE)
   })
   if (1 == length(dum.nm)) {
      tmp2 <- tmp[[1]]
      rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = dum.nm)
   } else {
      tmp2 <- str2str::t_list(tmp)
      rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
   }
   return(rtn)
}

# CORRELATIONS ####

# add_sig #

#' Add Significance Symbols to a (Atomic) Vector, Matrix, or Array
#'
#' \code{add_sig} adds symbols for various p-values cutoffs of statistical
#' significance. The function inputs a numeric vector, matrix, or array of
#' effect sizes (e.g., correlation matrix) and a numeric vector, matrix, or
#' array of p-values that correspond to the effect size (i.e., each row and
#' column match) and then returns a character vector, matrix, or array of effect
#' sizes with appended significance symbols. One of the primary applications of
#' this function is use within \code{\link{corp}} \code{\link{corp_by}}, and
#' \code{\link{corp_ml}} for correlation matrices.
#'
#' There are several functions out there that do similar things. Here is one
#' posted to R-bloggers that does it for correlation matrices using the
#' \code{corr} function from the \code{Hmisc} package:
#' \href{https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/}{Click
#' Here}.
#'
#' @param x double numeric vector of effect sizes for which statistical
#'   significance is available.
#'
#' @param p double matrix of p-values for the effect sizes in \code{x} that are
#'   matched by element index for vectors, by row and column index with
#'   matrices, by row, column, and layer index for 3D arrays, etc. For example,
#'   the p-value in the first row and second column of \code{p} is associated
#'   with the effect size in the first row and second column of \code{x}. If
#'   \code{x} and \code{p} do not have the same dimensions, an error is
#'   returned.
#'
#' @param digits integer vector of length 1 specifying the number of decimals to
#'   round to.
#'
#' @param p.10 character vector of length 1 specifying which symbol to append to
#'   the end of any effect size significant at the p < .10 level.
#'
#' @param p.05 character vector of length 1 specifying which symbol to append to
#'   the end of any effect size significant at the p < .05 level.
#'
#' @param p.01 character vector of length 1 specifying which symbol to append to
#'   the end of any effect size significant at the p < .01 level.
#'
#' @param p.001 character vector of length 1 specifying which symbol to append
#'   to the end of any effect size significant at the p < .001 level.
#'
#' @param lead.zero logical vector of length 1 specifying whether to retain a
#'   zero in front of the decimal place if the effect size is within 1 or -1.
#'
#' @param trail.zero logical vector of length 1 specifying whether to retain
#'   zeros after the decimal place (due to rounding).
#'
#' @param plus logical vector of length 1 specifying whether to include a plus
#'   sign in front of positive effect sizes (minus signs are always in front of
#'   negative effect sizes).
#'
#' @return character vector, matrix, or array with the same dimensions as
#'   \code{x} and \code{p} containing the effect sizes with their significance
#'   symbols appended to the end of each value.
#'
#' @examples
#'
#' corr_test <- psych::corr.test(mtcars[1:5])
#' r <- corr_test[["r"]]
#' p <- corr_test[["p"]]
#' add_sig(x = r, p = p)
#' add_sig(x = r, p = p, digits = 2)
#' add_sig(x = r, p = p, lead.zero = TRUE, trail.zero = FALSE)
#' add_sig(x = r, p = p, plus = TRUE)
#' noquote(add_sig(x = r, p = p)) # no quotes for character elements
#'
#' @export
add_sig <- function(x, p, digits = 3,
   p.10 = "", p.05 = "*", p.01 = "**", p.001 = "***",
   lead.zero = FALSE, trail.zero = TRUE, plus = FALSE) {

   # errors
   x_ndim <-  str2str::ndim(x)
   p_ndim <- str2str::ndim(p)
   if (is.null(x_ndim) && !(is.null(p_ndim))) # x = vector; p = matrix/array
      stop("if `x` is a vector, then `p` must also be a vector.")
   if (!(is.null(x_ndim)) && is.null(p_ndim)) # x = matrix/array; p = vector
      stop("if `p` is a vector, then `x` must also be a vector.")
   if (is.null(x_ndim) && is.null(p_ndim)) { # x = vector; p = vector
      if (length(x) != length(p))
         stop("if `x` and `p` are both atomic vectors, `x` and `p` must have the same length")
   }
   if (!(is.null(x_ndim)) && !(is.null(p_ndim))) { # x = matrix/array; p = matrix/array
      x_dim <- dim(x)
      p_dim <- dim(p)
      if (length(x_dim) != length(p_dim))
         stop("if `x` and `p` are both matrix/array, they must have the same number of dimensions")
      if (any(x_dim != p_dim))
         stop("`x` and `p` are both matrix/array, they must have the same dimensions")
   }

   # function itself
   if (plus) flag <- "+" else flag <- ""
   if (trail.zero) drop0trailing <- FALSE else drop0trailing <- TRUE
   rtn <- formatC(x = round(x = x, digits = digits), digits = digits, # have to use `digits` within round() *and* within formatC() or prettyNum() to retain trailing zeros: https://kmyu.wordpress.com/2011/01/11/formatting-numbers-for-printing-in-r-rounding-and-trailing-zeroes/
      format = "f", flag = flag, drop0trailing = drop0trailing) # need to specify `format` = "f" to prevent as.character(x) being called internally, which drops trailing 0s.
   if (!lead.zero) rtn <- sub(pattern = "0", replacement = "", x = rtn)
   rtn <- ifelse(p < .001, yes = paste0(rtn, p.001), no = rtn)
   rtn <- ifelse(p >= .001 & p < .01, yes = paste0(rtn, p.01), no = rtn)
   rtn <- ifelse(p >= .01 & p < .05, yes = paste0(rtn, p.05), no = rtn)
   rtn <- ifelse(p >= .05 & p < .10, yes = paste0(rtn, p.10), no = rtn)
   return(rtn)
}

# add_sig_cor #

#' Add Significance Symbols to a Correlation Matrix
#'
#' \code{add_sig_cor} adds symbols for various p-values cutoffs of statistical
#' significance. The function inputs a correlation matrix and a numeric matrix
#' of p-values that correspond to the correlations (i.e., each row and column
#' match) and then returns a data.frame of correlations with appended
#' significance symbols. One of the primary applications of this function is use
#' within \code{\link{corp}} \code{\link{corp_by}}, and \code{\link{corp_ml}}
#' for correlation matrices.
#'
#' There are several functions out there that do similar things. Here is one
#' posted to R-bloggers that uses the \code{corr} function from the \code{Hmisc}
#' package:
#' \href{https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/}{Click
#' Here}.
#'
#' @param r double numeric matrix of correlation coefficients for which
#'   statistical significance is available. Since its a correlation matrix, it
#'   must be symmetrical and is expected to be a full matrix with all elements
#'   included (not just lower or upper diagonals values included).
#'
#' @param p double matrix of p-values for the correlations in \code{r} that are
#'   matched by row and column index. For example, the p-value in the first row
#'   and second column of \code{p} is associated with the correlation in the
#'   first row and second column of \code{r}. If \code{r} and \code{p} do not
#'   have the same dimensions, an error is returned.
#'
#' @param digits integer vector of length 1 specifying the number of decimals to
#'   round to.
#'
#' @param p.10 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .10 level.
#'
#' @param p.05 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .05 level.
#'
#' @param p.01 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .01 level.
#'
#' @param p.001 character vector of length 1 specifying which symbol to append
#'   to the end of any correlation significant at the p < .001 level.
#'
#' @param lead.zero logical vector of length 1 specifying whether to retain a
#'   zero in front of the decimal place.
#'
#' @param trail.zero logical vector of length 1 specifying whether to retain
#'   zeros after the decimal place (due to rounding).
#'
#' @param plus logical vector of length 1 specifying whether to include a plus
#'   sign in front of positive correlations (minus signs are always in front of
#'   negative correlations).
#'
#' @param diags logical vector of length 1 specifying whether to retain the
#'   values in the diagonal of the correlation matrix. If TRUE, then the
#'   diagonal will be 1s with \code{digits} number of zeros after the decimal
#'   place (and no significant symbols). If FALSE, then the diagonal will be NA.
#'
#' @param lower logical vector of length 1 specifying whether to retain the
#'   lower triangle of the correlation matrix. If TRUE, then the lower triangle
#'   correlations and their significance symbols are retained. If FAlSE, then
#'   the lower triangle will all be NA.
#'
#' @param upper logical vector of length 1 specifying whether to retain the
#'   upper triangle of the correlation matrix. If TRUE, then the upper triangle
#'   correlations and their significance symbols are retained. If FAlSE, then
#'   the upper triangle will all be NA.
#'
#' @return data.frame with the same dimensions as \code{r} containing the
#'   correlations and their significance symbols. Elements may or may not contain NA
#'   values depending on the arguments \code{diags}, \code{lower}, and
#'   \code{upper}.
#'
#' @examples
#'
#' corr_test <- psych::corr.test(mtcars[1:5])
#' r <- corr_test[["r"]]
#' p <- corr_test[["p"]]
#' add_sig_cor(r = r, p = p)
#' add_sig_cor(r = r, p = p, digits = 2)
#' add_sig_cor(r = r, p = p, diags = TRUE)
#' add_sig_cor(r = r, p = p, lower = FALSE, upper = TRUE)
#' add_sig_cor(r = r, p = p, lead.zero = TRUE, trail.zero = FALSE)
#' add_sig_cor(r = r, p = p, plus = TRUE)
#'
#' @export
add_sig_cor <- function(r, p, digits = 3,
   p.10 = "", p.05 = "*", p.01 = "**", p.001 = "***",
   lead.zero = FALSE, trail.zero = TRUE, plus = FALSE,
   diags = FALSE, lower = TRUE, upper = FALSE) {

   rtn <- add_sig(x = r, p = p, digits = digits,
      p.10 = p.10, p.05 = p.05, p.01 = p.01, p.001 = p.001,
      lead.zero = lead.zero, trail.zero = trail.zero, plus = plus)
   if (!diags)
      diag(rtn) <- NA_character_
   else
      diag(rtn) <- paste0("1.", paste0(rep.int(x = "0", times = digits), collapse = ""))
   if (!lower) rtn[lower.tri(rtn)] <- NA_character_
   if (!upper) rtn[upper.tri(rtn)] <- NA_character_
   return(as.data.frame(rtn, stringsAsFactors = FALSE))
}

# corp #

#' Bivariate Correlations with Significant Symbols
#'
#' \code{corp} computes bivariate correlations and their associated p-values.
#' The function is primarily for preparing a correlation table for publication:
#' the correlations are appended by significant symbols (e.g., asterixis),
#' \code{corp} is simply \code{\link[psych]{corr.test}} + \code{add_sig_cor}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variable columns.
#'
#' @param use character vector of length 1 specifying how to handle missing data
#'   when computing the correlations. The options are 1)
#'   "pairwise.complete.obs", 2) "complete.obs", 3) "na.or.complete", 4)
#'   "all.obs", or 5) "everything". See details of \code{\link[stats]{cor}}.
#'
#' @param method character vector of length 1 specifying the type of
#'   correlations to be computed. The options are 1) "pearson", 2) "kendall", or
#'   3) "spearman". See details of \code{\link[stats]{cor}}.
#'
#' @param digits integer vector of length 1 specifying the number of decimals to
#'   round to.
#'
#' @param p.10 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .10 level.
#'
#' @param p.05 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .05 level.
#'
#' @param p.01 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .01 level.
#'
#' @param p.001 character vector of length 1 specifying which symbol to append
#'   to the end of any correlation significant at the p < .001 level.
#'
#' @param lead.zero logical vector of length 1 specifying whether to retain a
#'   zero in front of the decimal place.
#'
#' @param trail.zero logical vector of length 1 specifying whether to retain
#'   zeros after the decimal place (due to rounding).
#'
#' @param plus logical vector of length 1 specifying whether to include a plus
#'   sign in front of positive correlations (minus signs are always in front of
#'   negative correlations).
#'
#' @param diags logical vector of length 1 specifying whether to retain the
#'   values in the diagonal of the correlation matrix. If TRUE, then the
#'   diagonal will be 1s with \code{digits} number of zeros after the decimal
#'   place (and no significant symbols). If FALSE, then the diagonal will be NA.
#'
#' @param lower logical vector of length 1 specifying whether to retain the
#'   lower triangle of the correlation matrix. If TRUE, then the lower triangle
#'   correlations and their significance symbols are retained. If FAlSE, then
#'   the lower triangle will all be NA.
#'
#' @param upper logical vector of length 1 specifying whether to retain the
#'   upper triangle of the correlation matrix. If TRUE, then the upper triangle
#'   correlations and their significance symbols are retained. If FAlSE, then
#'   the upper triangle will all be NA.
#'
#' @return data.frame with rownames and colnames equal to \code{vrb.nm}
#'   containing the bivariate correlations with significance symbols after the
#'   correlation value, specified by the arguments \code{p.10}, \code{p.05},
#'   \code{p.01}, and \code{p.001} arguments. The specific elements of the
#'   return object are determined by the other arguments.
#'
#' @seealso
#'    \code{\link{add_sig_cor}} for adding significant symbols to a correlation matrix,
#'    \code{\link{add_sig}} for adding significant symbols to any (atomic) vector, matrix, or (3D+) array,
#'    \code{\link[stats]{cor}} for computing only the correlation coefficients themselves
#'    \code{\link[psych]{corr.test}} for a function providing confidence intervals as well
#'
#' @examples
#'
#' corp(data = mtcars, vrb.nm = c("mpg","cyl","disp","hp","drat")) # no quotes b/c a data.frame
#' corp(data = attitude, vrb.nm = colnames(attitude))
#' corp(data = attitude, vrb.nm = colnames(attitude), p.10 = "'") # advance & privileges
#' corp(data = airquality, vrb.nm = colnames(airquality), plus = TRUE)
#'
#' @export
corp <- function(data, vrb.nm, use = "pairwise.complete.obs", method = "pearson",
   digits = 3L, p.10 = "", p.05 = "*", p.01 = "**", p.001 = "***",
   lead.zero = FALSE, trail.zero = TRUE, plus = FALSE,
   diags = FALSE, lower = TRUE, upper = FALSE) {

   corr_test <- psych::corr.test(x = data[vrb.nm], use = use, method = method,
      adjust = "none", ci = FALSE)
   r <- corr_test[["r"]]
   p <- corr_test[["p"]]
   rtn <- add_sig_cor(r = r, p = p, digits = digits, p.10 = p.10, p.05 = p.05, p.01 = p.01,
      p.001 = p.001, lead.zero = lead.zero, trail.zero = trail.zero, plus = plus,
      diags = diags, lower = lower, upper = upper)
   return(rtn)
}

# cor_by #

#' Correlation Matrix by Group
#'
#' \code{cor_by} computes a correlation matrix for each group within numeric
#' data. Only the correlation coefficients are determined and not any NHST
#' information. If that is desired, use \code{\link{corp_by}} which includes
#' significance symbols. \code{cor_by} is simply \code{cor} + \code{by2}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param use character vector of length 1 specifying how to handle missing data
#'   when computing the correlations. The options are 1)
#'   "pairwise.complete.obs", 2) "complete.obs", 3) "na.or.complete", 4)
#'   "all.obs", or 5) "everything". See details of \code{\link[stats]{cor}}.
#'
#' @param method character vector of length 1 specifying the type of
#'   correlations to be computed. The options are 1) "pearson", 2) "kendall", or
#'   3) "spearman". See details of \code{\link[stats]{cor}}.
#'
#' @param sep character vector of length 1 specifying the string to combine the
#'   group values together with. \code{sep} is only used if there are multiple
#'   grouping variables (i.e., \code{length(grp.nm)} > 1).
#'
#' @param check logical vector of length 1 specifying whether to check the
#'   structure of the input arguments. For example, check whether
#'   \code{data[vrb.nm]} are all mode numeric. This argument is available to
#'   allow flexibility in whether the user values informative error messages
#'   (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of numeric matrices containing the correlations from each group.
#'   The listnames are the unique combinations of the grouping variables,
#'   separated by "sep" if multiple grouping variables (i.e.,
#'   \code{length(grp.nm)} > 1) are input:
#'   \code{unique(interaction(data[grp.nm], sep = sep))}. The rownames and
#'   colnames of each numeric matrix are \code{vrb.nm}.
#'
#' @seealso
#'    \code{\link[stats]{cor}} for full sample correlation matrixes,
#'    \code{\link{corp}} for full sample correlation data.frames with significance symbols,
#'    \code{\link{corp_by}} for full sample correlation data.farmes with significance symbols
#'    by group.
#'
#' @examples
#'
#' # one grouping variable
#' cor_by(airquality, vrb.nm = c("Ozone","Solar.R","Wind"), grp.nm = "Month")
#' cor_by(airquality, vrb.nm = c("Ozone","Solar.R","Wind"), grp.nm = "Month",
#'    use = "complete.obs", method = "spearman")
#'
#' # two grouping variables
#' cor_by(mtcars, vrb.nm = c("mpg","disp","drat","wt"), grp.nm = c("vs","am"))
#' cor_by(mtcars, vrb.nm = c("mpg","disp","drat","wt"), grp.nm = c("vs","am"),
#'    use = "complete.obs", method = "spearman", sep = "_")
#'
#' @export
cor_by <- function(data, vrb.nm, grp.nm, use = "pairwise.complete.obs",
   method = "pearson", sep = ".", check = TRUE) {

   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertCharacter(vrb.nm, any.missing = FALSE, unique = TRUE)
      checkmate::assertCharacter(grp.nm, any.missing = FALSE, unique = TRUE)
      checkmate::assertNames(vrb.nm, subset.of = names(data), what = "names")
      checkmate::assertNames(grp.nm, subset.of = names(data), what = "names")
      checkmate::assertDataFrame(data[vrb.nm],
         types = c("integer","integerish","double","numeric"))
      match.arg(use, choices = c("everything","all.obs","complete.obs",
         "na.or.complete","pairwise.complete.obs"))
      match.arg(method, choices = c("pearson","kendall","spearman"))
   }

   by2(.data = data, .vrb.nm = vrb.nm, .grp.nm = grp.nm, .sep = sep,
      .fun = cor, use = use, method = method)
}

# corp_by #

#' Bivariate Correlations with Significant Symbols by Group
#'
#' \code{corp_by} computes a correlation data.frame for each group within
#' numeric data. The correlation coefficients are appended by their significant
#' symbols based on their associated p-values. If only the correlation
#' coefficients are desired, use \code{cor_by} which returns a list of numeric
#' matrices. \code{corp_by} is simply \code{corp} + \code{by2}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param use character vector of length 1 specifying how to handle missing data
#'   when computing the correlations. The options are 1)
#'   "pairwise.complete.obs", 2) "complete.obs", 3) "na.or.complete", 4)
#'   "all.obs", or 5) "everything". See details of \code{\link[stats]{cor}}.
#'
#' @param method character vector of length 1 specifying the type of
#'   correlations to be computed. The options are 1) "pearson", 2) "kendall", or
#'   3) "spearman". See details of \code{\link[stats]{cor}}.
#'
#' @param sep character vector of length 1 specifying the string to combine the
#'   group values together with. \code{sep} is only used if there are multiple
#'   grouping variables (i.e., \code{length(grp.nm)} > 1).
#'
#' @param digits integer vector of length 1 specifying the number of decimals to
#'   round to.
#'
#' @param p.10 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .10 level.
#'
#' @param p.05 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .05 level.
#'
#' @param p.01 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .01 level.
#'
#' @param p.001 character vector of length 1 specifying which symbol to append
#'   to the end of any correlation significant at the p < .001 level.
#'
#' @param lead.zero logical vector of length 1 specifying whether to retain a
#'   zero in front of the decimal place.
#'
#' @param trail.zero logical vector of length 1 specifying whether to retain
#'   zeros after the decimal place (due to rounding).
#'
#' @param plus logical vector of length 1 specifying whether to include a plus
#'   sign in front of positive correlations (minus signs are always in front of
#'   negative correlations).
#'
#' @param diags logical vector of length 1 specifying whether to retain the
#'   values in the diagonal of the correlation matrix. If TRUE, then the
#'   diagonal will be 1s with \code{digits} number of zeros after the decimal
#'   place (and no significant symbols). If FALSE, then the diagonal will be NA.
#'
#' @param lower logical vector of length 1 specifying whether to retain the
#'   lower triangle of the correlation matrix. If TRUE, then the lower triangle
#'   correlations and their significance symbols are retained. If FAlSE, then
#'   the lower triangle will all be NA.
#'
#' @param upper logical vector of length 1 specifying whether to retain the
#'   upper triangle of the correlation matrix. If TRUE, then the upper triangle
#'   correlations and their significance symbols are retained. If FAlSE, then
#'   the upper triangle will all be NA.
#'
#' @return list of data.frames containing the correlation coefficients and their
#'   appended significance symbols based upon their associated p-values. The
#'   listnames are the unique combinations of the grouping variables, separated
#'   by "sep" if multiple grouping variables (i.e., \code{length(grp.nm)} > 1)
#'   are input: \code{unique(interaction(data[grp.nm], sep = sep))}. For each
#'   data.frame, the rownames and colnames = \code{vrb.nm}. The significance
#'   symbols are specified by the arguments \code{p.10}, \code{p.05},
#'   \code{p.01}, and \code{p.001}, after the correlation value. The specific
#'   elements of the return object are determined by the other arguments.
#'
#' @seealso
#'    \code{\link{corp}}
#'    \code{\link{cor_by}}
#'    \code{\link[stats]{cor}}
#'
#' @examples
#'
#' # one grouping variable
#' corp_by(airquality, vrb.nm = c("Ozone","Solar.R","Wind"), grp.nm = "Month")
#' corp_by(airquality, vrb.nm = c("Ozone","Solar.R","Wind"), grp.nm = "Month",
#'    use = "complete.obs", method = "spearman")
#'
#' # two grouping variables
#' corp_by(mtcars, vrb.nm = c("mpg","disp","drat","wt"), grp.nm = c("vs","am"))
#' corp_by(mtcars, vrb.nm = c("mpg","disp","drat","wt"), grp.nm = c("vs","am"),
#'    use = "complete.obs", method = "spearman", sep = "_")
#'
#' @export
corp_by <- function(data, vrb.nm, grp.nm, use = "pairwise.complete.obs",
   method = "pearson", sep = ".",
   digits = 3L, p.10 = "", p.05 = "*", p.01 = "**", p.001 = "***",
   lead.zero = FALSE, trail.zero = TRUE, plus = FALSE,
   diags = FALSE, lower = TRUE, upper = FALSE) {

   by2(.data = data, .vrb.nm = vrb.nm, .grp.nm = grp.nm, .sep = sep,
      .fun = corp, use = use, method = method, digits = digits, p.10 = p.10,
      p.05 = p.05, p.01 = p.01, p.001 = p.001,
      lead.zero = lead.zero, trail.zero = trail.zero, plus = plus,
      diags = diags, lower = lower, upper = upper)
}

# cor_miss #

#' Point-biserial Correlations of Missingness
#'
#' \code{cor_miss} computes (point-biserial) correlations between missingness on
#' data columns and scores on other data columns.
#'
#' \code{cor_miss} calls \code{\link{make.dumNA}} to create dummy vectors representing
#' missingness on the \code{data[m.nm]} columns.
#'
#' @param data data.frame of data.
#'
#' @param x.nm character vector of colnames in \code{data} to be the predictors
#'   of missingness.
#'
#' @param m.nm character vector of colnames in \code{data} to specify missing
#'   data on.
#'
#' @param ov logical vector of length 1 specifying whether the correlations
#'   should be with "observedness" rather than missingness.
#'
#' @param use character vector of length 1 specifying how to deal with missing
#'   data in the predictor columns. See \code{\link[stats]{cor}}.
#'
#' @param method character vector of length 1 specifying what type of
#'   correlations to compute. See \code{\link[stats]{cor}}.
#'
#' @return numeric matrix of (point-biserial) correlations between rows of
#'   predictors and columns of missingness.
#'
#' @examples
#'
#' cor_miss(data = airquality, x.nm = c("Wind","Temp","Month","Day"),
#'    m.nm = c("Ozone","Solar.R"))
#' cor_miss(data = airquality, x.nm = c("Wind","Temp","Month","Day"),
#'    m.nm = c("Ozone","Solar.R"), ov = TRUE) # correlations with "observedness"
#' cor_miss(data = airquality, x.nm = c("Wind","Temp","Month","Day"),
#'    m.nm = c("Ozone","Solar.R"), use = "complete.obs", method = "kendall")
#'
#' @export
cor_miss <- function(data, x.nm, m.nm, ov = FALSE,
   use = "pairwise.complete.obs", method = "pearson") {

   na_dummy <- make.dumNA(data = data, vrb.nm = m.nm, ov = ov, suffix = "")
   rtn <- apply(X = na_dummy, MARGIN = 2, FUN = function(vec) {
      cor(x = data[x.nm], y = vec, use = use, method = method)
   })
   if (ov) m_nm <- "observedness" else m_nm <- "missingness"
   dimnames(rtn) <- setNames(list(x.nm, m.nm), c("predictor", m_nm))
   return(rtn)
}

# corp_miss #

#' Point-biserial Correlations of Missingness With Significant Symbols
#'
#' \code{corp_miss} computes (point-biserial) correlations between missingness
#' on data columns and scores on other data columns. It also appends
#' significance symbols at the end of the correlations.
#'
#' \code{cor_miss} calls \code{make.dumNA} to create dummy vectors representing
#' missingness on the \code{data[m.nm]} columns.
#'
#' @param data data.frame of data.
#'
#' @param x.nm character vector of colnames in \code{data} to be the predictors
#'   of missingness.
#'
#' @param m.nm character vector of colnames in \code{data} to specify missing
#'   data on.
#'
#' @param ov logical vector of length 1 specifying whether the correlations
#'   should be with "observedness" rather than missingness.
#'
#' @param use character vector of length 1 specifying how to deal with missing
#'   data in the predictor columns. See \code{\link[stats]{cor}}.
#'
#' @param method character vector of length 1 specifying what type of
#'   correlations to compute. See \code{\link[stats]{cor}}.
#'
#' @param m.suffix character vector of length 1 specifying a string to oppend to
#'   the end of the colnames to clarify whether they refer to missingness or
#'   "observedness". Default is "_na" if \code{ov} = FALSE and "_ov" if
#'   \code{ov} = TRUE.
#'
#' @param digits integer vector of length 1 specifying the number of decimals to
#'   round to.
#'
#' @param p.10 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .10 level.
#'
#' @param p.05 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .05 level.
#'
#' @param p.01 character vector of length 1 specifying which symbol to append to
#'   the end of any correlation significant at the p < .01 level.
#'
#' @param p.001 character vector of length 1 specifying which symbol to append
#'   to the end of any correlation significant at the p < .001 level.
#'
#' @param lead.zero logical vector of length 1 specifying whether to retain a
#'   zero in front of the decimal place.
#'
#' @param trail.zero logical vector of length 1 specifying whether to retain
#'   zeros after the decimal place (due to rounding).
#'
#' @param plus logical vector of length 1 specifying whether to include a plus
#'   sign in front of positive correlations (minus signs are always in front of
#'   negative correlations).
#'
#' @return numeric matrix of (point-biserial) correlations between rows of
#'   predictors and columns of missingness.
#'
#' @examples
#'
#' corp_miss(data = airquality, x.nm = c("Wind","Temp","Month","Day"),
#'    m.nm = c("Ozone","Solar.R"))
#' corp_miss(data = airquality, x.nm = c("Wind","Temp","Month","Day"),
#'    m.nm = c("Ozone","Solar.R"), ov = TRUE) # correlations with "observedness"
#' corp_miss(data = airquality, x.nm = c("Wind","Temp","Month","Day"),
#'    m.nm = c("Ozone","Solar.R"), use = "complete.obs", method = "kendall")
#'
#' @export
corp_miss <- function(data, x.nm, m.nm, ov = FALSE,
   use = "pairwise.complete.obs", method = "pearson",
   m.suffix = if (ov) "_ov" else "_na",
   digits = 3L, p.10 = "", p.05 = "*", p.01 = "**", p.001 = "***",
   lead.zero = FALSE, trail.zero = TRUE, plus = FALSE) {

   na_dummy <- make.dumNA(data = data, vrb.nm = m.nm, ov = ov, suffix = "")
   r <- apply(X = na_dummy, MARGIN = 2, FUN = function(vec) {
      corr_test <- psych::corr.test(x = data[x.nm], y = vec,
         use = use, method = method, adjust = "none", ci = FALSE)
      return(corr_test[["r"]])
   })
   p <- apply(X = na_dummy, MARGIN = 2, FUN = function(vec) {
      corr_test <- psych::corr.test(x = data[x.nm], y = vec,
         use = use, method = method, adjust = "none", ci = FALSE)
      return(corr_test[["p"]])
   })
   mat <- add_sig(x = r, p = p, digits = digits, p.10 = p.10, p.05 = p.05, p.01 = p.01,
      p.001 = p.001, lead.zero = lead.zero, trail.zero = trail.zero, plus = plus)
   rownames(mat) <- x.nm
   colnames(mat) <- paste0(colnames(mat), m.suffix)
   rtn <- as.data.frame(mat, strinsAsFactors = FALSE)
   return(rtn)
}

# covs_test #

#' Covariances Test of Significance
#'
#' \code{covs_test} computes sample covariances and tests for their significance
#' with the Pearson method assuming multivariate normality of the data. Note,
#' the normal-theory significance test for the covariance is much more sensitive
#' to departures from normality than the significant test for the mean. This
#' function is the covariance analogue to the \code{psych::corr.test()} function
#' for correlations.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames specifying the variables in
#'   \code{data} to conduct the significant test of the covariances.
#'
#' @param use character vector of length 1 specifying how missing values are
#'   handled. Currently, there are only two options: 1) "pairwise" for pairwise
#'   deletion (i.e., \code{cov(use = "pairwise.complete.obs")}), or 2)
#'   "complete" for listwise deletion (i.e., \code{cov(use = "complete.obs")}).
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   It must be between 0 and 1 - or it can be NULL in which case confidence
#'   intervals are not computed and the return object does not have "lwr" or
#'   "upr" columns.
#'
#' @param rtn.dfm logical vector of length 1 specifying whether the return
#'   object should be an array (FALSE) or data.frame (TRUE). If an array, then
#'   the first two dimensions are the matrix dimensions from the covariance
#'   matrix and the 3rd dimension (aka layers) contains the statistical
#'   information (e.g., est, se, t). If  data.frame, then the first two columns
#'   are the matrix dimensions from the covariance matrix expanded and the rest
#'   of the columns contain the statistical information (e.g., est, se, t).
#'
#' @return If \code{rtn.dfm = FALSE}, an array where its first two dimensions
#' are the matrix dimensions from the covariance matrix and the 3rd dimension
#' (aka layers) contains the statistical information detailed below. If
#' \code{rtn.dfm = TRUE}, a data.frame where its first two columns are the
#' expanded matrix dimensions from the covariance matrix and the rest of the
#' columns contain the statistical information detailed below:
#'
#' \describe{
#'    \item{cov}{sample covariances}
#'    \item{se}{standard errors of the covariances}
#'    \item{t}{t-values}
#'    \item{df}{degrees of freedom (n - 2)}
#'    \item{p}{two-sided p-values}
#'    \item{lwr}{lower bound of the confidence intervals (excluded if \code{ci.level = NULL})}
#'    \item{upr}{upper bound of the confidence intervals (excluded if \code{ci.level = NULL})}
#' }
#'
#' @seealso
#'    \code{\link{cov}} for covariance matrix estimates,
#'    \code{\link[psych]{corr.test}} for correlation matrix significant testing,
#'
#' @examples
#'
#' # traditional use
#' covs_test(data = attitude, vrb.nm = names(attitude))
#' covs_test(data = attitude, vrb.nm = names(attitude),
#'    ci.level = NULL) # no confidence intervals
#' covs_test(data = attitude, vrb.nm = names(attitude),
#'    rtn.dfm = TRUE) # return object as data.frame
#'
#' # NOT same as simple linear regression slope
#' covTest <- covs_test(data = attitude, vrb.nm = names(attitude),
#'    ci.level = NULL, rtn.dfm = TRUE)
#' x <- covTest[with(covTest, rownames == "rating" & colnames == "complaints"), ]
#' lm_obj <- lm(rating ~ complaints, data = attitude)
#' y <- coef(summary(lm_obj))["complaints", , drop = FALSE]
#' print(x); print(y)
#' z <- x[, "cov"] / var(attitude$"complaints")
#' print(z) # dividing by variance of the predictor gives you the regression slope
#' # but the t-values and p-values are still different
#'
#' # NOT same as correlation coefficient
#' covTest <- covs_test(data = attitude, vrb.nm = names(attitude),
#'    ci.level = NULL, rtn.dfm = TRUE)
#' x <- covTest[with(covTest, rownames == "rating" & colnames == "complaints"), ]
#' cor_test <- cor.test(x = attitude[[1]], y = attitude[[2]])
#' print(x); print(cor_test)
#' z <- x[, "cov"] / sqrt(var(attitude$"rating") * var(attitude$"complaints"))
#' print(z) # dividing by sqrt of the variances gives you the correlation
#' # but the t-values and p-values are still different
#'
#' @export
covs_test <- function(data, vrb.nm, use = "pairwise", ci.level = 0.95, rtn.dfm = FALSE) {

   # setup
   k <- length(vrb.nm)
   data_matrix <- as.matrix(data[vrb.nm], rownames.force = TRUE)
   use <- match.arg(use, choices = c("pairwise","complete")) # it got too complicated trying to allow all 5 `use` options from cov()

   # descriptives
   cov_est <- cov(data_matrix, use = use, method = "pearson")
   var_est <- diag(cov_est) # accounts for pairwise vs. complete
   var_crossProduct <- var_est %*% t(var_est)
   bivariate.moment4 <- function(dat, n, means) { # mean((x - mx)^2 * (y - my)^2) from miceadds:::covTest()
      # ARGUMENTS ASSUME YOU HAVE ALREADY DEALT WITH PAIRWISE VS. COMPLETE DATA
      # dat = data as a numeric matrix (same dimensions as data[vrb.nm])
      # n = sample sizes as a numeric matrix (same dimensions as cov_est)
      # means = mean estimates as numeric matrix

      sample_size <- nrow(dat)
      p <- ncol(dat)
      mean_mat <- matrix(data = means, nrow = sample_size, ncol = p, byrow = TRUE)
      dev <- dat - mean_mat
      dev2 <- dev ^ 2
      sscp2 <- t(dev2) %*% dev2
      output <- sscp2 / n
      return(output)
   }
   if (use == "complete") {
      data_matrix <- na.omit(data_matrix)
      n <- matrix(nrow(data_matrix), nrow = k, ncol = k)
      mean_est <- colMeans(data_matrix) # should have no missing data
      bivariate_moment4 <- bivariate.moment4(dat = data_matrix, n = n, means = mean_est)
   }
   if (use == "pairwise") {
      n <- t(!(is.na(data_matrix))) %*% !(is.na(data_matrix)) # from psych::corr.test() - don't add as.numeric() b/c converts to atomic vector
      mean_est <- colMeans(data_matrix, na.rm = TRUE)
      bivariate_moment4 <- bivariate.moment4(dat = data_matrix, n = n, means = mean_est)
   }

   # standard errors
   cov_se2 <- ((1 / n) * (bivariate_moment4 - cov_est^2)) +
      ((1 / n) * (cov_est^2 + var_crossProduct) / (n - 1))
   cov_se <- sqrt(cov_se2)

   # nhst & ci
   t <- cov_est / cov_se
   df <- n - 2
   p <- 2 * pt(q = abs(t), df = df, lower.tail = FALSE)
   cov_list <- list("cov" = cov_est, "se" = cov_se, "t" = t, "df" = df, "p" = p)
   if (!(is.null(ci.level))) {
      crit <- qt(p = ci.level +  ((1 - ci.level) / 2), df = df, lower.tail = TRUE)
      hw <- cov_se * crit
      lwr <- cov_est - hw
      upr <- cov_est + hw
      cov_list <- append(x = cov_list, values = list("lwr" = lwr, "upr" = upr))
   }

   # return object
   rtn <- abind::abind(cov_list, along = 3L, use.first.dimnames = TRUE, use.dnns = TRUE)
   if (rtn.dfm) {
      rtn <- suppressWarnings(str2str::a2d(rtn, col = 3)) # at one point str2str::a2d was causing problems...
      names(rtn)[1:2] <- c("rownames","colnames")
   }
   return(rtn)
}

# ROXYGEN2 PROBLEM ####

# means_change #

#' Mean Changes Across Two Timepoints For Multiple PrePost Pairs of Variables
#' (dependent two-samples t-tests)
#'
#' \code{means_change} tests for mean changes across two timepoints for multiple
#' prepost pairs of variables via dependent two-samples t-tests. The function
#' also calculates the descriptive statistics for the timepoints and the
#' standardized mean differences (i.e., Cohen's d) based on either the standard
#' deviation of the pre-timepoint, pooled standard deviation of the
#' pre-timepoint and post-timepoint, or the standard deviation of the change
#' score (post - pre). \code{means_change} is simply a wrapper for
#' \code{\link[stats]{t.test}} plus some extra calculations.
#'
#' For each prepost pair of variables, \code{means_change} calculates the mean
#' change as \code{data[[ prepost.nm.list[[i]][2] ]]} - \code{data[[
#' prepost.nm.list[[i]][1] ]]} (which corresponds to post - pre) such that
#' increases over time have a positive mean change estimate and decreases over
#' time have a negative mean change estimate. This would be as if the
#' post-timepoint was \code{x} and the pre-timepoint \code{y} in
#' \code{t.test(paired = TRUE)}.
#'
#' @param data data.frame of data.
#'
#' @param prepost.nm.list list of length-2 character vectors specifying the
#'   colnames from \code{data} corresponding to the prepost pairs of variables.
#'   For each element of the list, the character vector should have length 2
#'   where the first element corresponds to the pre-timepoint variable colname
#'   of that prepost pair and the second element corresponds to the
#'   post-timepoint variable colname of that prepost pair. The names of the list
#'   will be the rownames in the data.frames of the return object. See examples.
#'   \code{prepost.nm.list} can also be a single length-2 character vector for
#'   the case of a single pre-post pair of variables, which is functionally
#'   equivalent to \code{\link{mean_change}}.
#'
#' @param standardizer chararacter vector of length 1 specifying what to use for
#'   standardization when computing the standardized mean difference (i.e.,
#'   Cohen's d). There are three options: 1. "pre" for the standard deviation of
#'   the pre-timepoint, 2. "pooled" for the pooled standard deviation of the
#'   pre-timepoint and post-timepoint, 3. "change" for the standard deviation of
#'   the change score (post - pre). The default is "pre", which I believe makes
#'   the most theoretical sense (see Cumming, 2012); however, "change" is the
#'   traditional choice originally proposed by Jacob Cohen (Cohen, 1988).
#'
#' @param d.ci.type character vector of lenth 1 specifying how to compute the
#'   confidence intervals (and standard errors) of the standardized mean
#'   differences. There are currently two options: 1. "unbiased" which
#'   calculates the unbiased standard error of Cohen's d based on the formulas
#'   in Viechtbauer (2007). If \code{standardizer} = "pre" or "pooled", then
#'   equation 36 from Table 2 is used. If \code{standardizer} = "change", then
#'   equation 25 from Table 1 is used. A symmetrical confidence interval is then
#'   calculated based on the standard error. 2. "classic" which calculates the
#'   confidence interval of Cohen's d based on the confidence interval of the
#'   mean change itself. The lower and upper confidence bounds are divided by
#'   the \code{standardizer}. Technically, this confidence interval is biased
#'   due to not taking into account the uncertainty of the \code{standardizer}.
#'   No standard error is calculated for this option and NA is returned for
#'   "d_se" in the return object.
#'
#' @param ci.level double vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, checking whether
#'   \code{prepost.nm.list} is a list of length-2 character vectors. This is a
#'   tradeoff between computational efficiency (FALSE) and more useful error
#'   messages (TRUE).
#'
#' @return list of data.frames containing statistical information about the mean
#'   change for each prepost pair of variables (the rownames of the data.frames
#'   are the names of \code{prepost.nm.list}): 1) nhst = dependent two-samples
#'   t-test stat info in a data.frame, 2) desc = descriptive statistics stat info
#'   in a data.frame, 3) std = standardized mean difference stat info in a data.frame,
#'
#'
#' 1) nhst = dependent two-samples t-test stat info in a data.frame
#'
#' \describe{
#'    \item{est}{mean change estimate (i.e., post - pre)}
#'    \item{se}{standard error}
#'    \item{t}{t-value}
#'    \item{df}{degrees of freedom}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a data.frame
#'
#' \describe{
#'    \item{mean_post}{mean of the post variable}
#'    \item{mean_pre}{mean of the pre variable}
#'    \item{sd_post}{standard deviation of of the post variable}
#'    \item{sd_pre}{standard deviation of the pre variable}
#'    \item{n}{sample size of the change score}
#'    \item{r}{Pearson correlation between the pre and post variables}
#' }
#'
#' 3) std = standardized mean difference stat info in a data.frame
#'
#' \describe{
#'    \item{d_est}{Cohen's d estimate}
#'    \item{d_se}{Cohen's d standard error}
#'    \item{d_lwr}{Cohen's d lower bound of the confidence interval}
#'    \item{d_upr}{Cohen's d upper bound of the confidence interval}
#' }
#'
#' @references
#'
#'    Cohen, J. (1988). Statistical power analysis for the behavioral sciences,
#'    2nd ed. Hillsdale, NJ: Erlbaum.
#'
#'    Cumming, G. (2012). Understanding the new statistics: Effect sizes,
#'    confidence intervals, and meta-analysis. New York, NY: Rouledge.
#'
#'    Viechtbauer, W. (2007). Approximate confidence intervals for standardized
#'    effect sizes in the two-independent and two-dependent samples design.
#'    Journal of Educational and Behavioral Statistics, 32(1), 39-60.
#'
#' @seealso
#'    \code{\link{mean_change}} for a single pair of prepost variables,
#'    \code{\link[stats]{t.test}} fixes the table of contents for some unknown reason,
#'    \code{\link{means_diff}} for multiple independent two-sample t-tests,
#'    \code{\link{means_test}} for multiple one-sample t-tests,
#'
#' @examples
#'
#' # dependent two-sample t-tests
#' prepost_nm_list <- list("first_pair" = c("disp","hp"), "second_pair" = c("carb","gear"))
#' means_change(mtcars, prepost.nm.list = prepost_nm_list)
#' means_change(mtcars, prepost.nm.list = prepost_nm_list, d.ci.type = "classic")
#' means_change(mtcars, prepost.nm.list = prepost_nm_list, standardizer = "change")
#' means_change(mtcars, prepost.nm.list = prepost_nm_list, ci.level = 0.99)
#'
#' # same as intercept-only regression with the change score
#' means_change(data = mtcars, prepost.nm.list = c("disp","hp"))
#' lm_obj <- lm(hp - disp ~ 1, data = mtcars)
#' coef(summary(lm_obj))
#'
#' @export
means_change <- function(data, prepost.nm.list, standardizer = "pre",
   d.ci.type = "unbiased", ci.level = 0.95, check = TRUE) {

   # error checking
   if (!(is.list(prepost.nm.list))) prepost.nm.list <- list(prepost.nm.list) # so user can provide a single vector
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertList(prepost.nm.list, any.missing = FALSE, null.ok = FALSE)
      checkmate::assertCharacter(unlist(prepost.nm.list), any.missing = FALSE, null.ok = FALSE)
      checkmate::assertSubset(x = unlist(prepost.nm.list), choices = names(data), empty.ok = FALSE)
      standardizer <- match.arg(standardizer, choices = c("pre","pooled","change"))
      d.ci.type <- match.arg(d.ci.type, choices = c("unbiased","classic"))
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
   }

   # function itself
   tmp <- lapply(X = prepost.nm.list, FUN = function(nm_vec) {
      mean_change(pre = data[[ nm_vec[1] ]], post = data[[ nm_vec[2] ]],
         standardizer = standardizer, d.ci.type = d.ci.type, ci.level = ci.level, check = FALSE)
   })
   if (1 == length(prepost.nm.list)) {
      tmp2 <- tmp[[1]]
      rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = names(prepost.nm.list))
   } else {
      tmp2 <- str2str::t_list(tmp)
      rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
   }
   return(rtn)
}

# n_compare #

#' Test for Equal Frequency of Values (chi-square test of goodness of fit)
#'
#' \code{n_compare} tests whether all the values for a variable have equal
#' frequency with a chi-square test of goodness of fit. \code{n_compare} does
#' not currently allow for user-specified unequal frequencies of values; this is
#' possible with \code{\link[stats]{chisq.test}}. The function also calculates
#' the counts and overall percentages for the value frequencies.
#' \code{prop_test} is simply a wrapper for \code{\link[stats]{chisq.test}} plus
#' some extra calculations.
#'
#' @param x atomic vector. Probably makes sense to contain relatively few unique
#'   values.
#'
#' @param simulate.p.value logial vector of length 1 specifying whether the
#'   p-value should be based on a Monte Carlo simulation rather than the classic
#'   formula. See \code{\link[stats]{chisq.test}} for details.
#'
#' @param B integer vector of length 1 specifying how much Monte Carlo
#'   simulations run. Only used if \code{simulate.p.value} = TRUE. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @return list of numeric vectors containing statistical information about the
#'   frequency comparison: 1) nhst = chi-square test of goodness of fit stat info
#'   in a numeric vector, 2) count = numeric vector of length 3 with table of counts,
#'   3) percent = numeric vector of length 3 with table of overall percentages
#'
#' 1) nhst = chi-square test of goodness of fit stat info in a numeric vector
#'
#' \describe{
#'    \item{diff_avg}{average difference in subsample sizes (i.e., |ni - nj|)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (# of unique values = 1)}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) count = numeric vector of length 3 with table of counts with an additional
#' element for the total. The names are 1. "n_`lvl[k]`", 2. "n_`lvl[j]`", 3.
#' "n_`lvl[i]`", ...,  X = "total"
#'
#' 3) percent = numeric vector of length 3 with table of overall percentages with an additional
#' element for the total. The names are 1. "n_`lvl[k]`", 2. "n_`lvl[j]`", 3.
#' "n_`lvl[i]`", ...,  X = "total"
#'
#' @seealso
#'    \code{\link[stats]{chisq.test}} the workhorse for \code{n_compare},
#'    \code{\link{props_test}} for multiple dummy variables,
#'    \code{\link{prop_diff}} for chi-square test of independence,
#'
#' @examples
#'
#' n_compare(mtcars$"cyl")
#' n_compare(mtcars$"gear")
#' n_compare(mtcars$"cyl", simulate.p.value = TRUE)
#'
#' # compare to chisq.test()
#' n_compare(mtcars$"cyl")
#' chisq.test(table(mtcars$"cyl"))
#'
#' @export
n_compare <- function(x, simulate.p.value = FALSE, B = 2000) {

   # chisq.test
   table_x <- table(x)
   table4chisq <- rev(table_x)
   chisq_obj <- chisq.test(x = table4chisq,
      simulate.p.value = simulate.p.value, B = B)
   n_by <- rev(chisq_obj[["observed"]])
   names(n_by) <- paste0("n_", names(n_by))
   diff_mat <- outer(X = n_by, Y = n_by, FUN = `-`)
   diff_avg <- mean(abs(diff_mat[lower.tri(diff_mat)]))
   chisq_vec <- c("diff_avg" = diff_avg,
      "se" = NA_real_,
      "X2" = unname(chisq_obj[["statistic"]]),
      "df" = unname(chisq_obj[["parameter"]]),
      "p" = chisq_obj[["p.value"]])

   # tables
   count <- n_by
   n_total <- sum(count)
   str2str::append(count) <- c("total" = n_total)
   percent <- count / n_total

   # return object
   rtn <- list("nhst" = chisq_vec, "count" = count, "percent" = percent)
   return(rtn)
}

# prop_diff #

#' Proportion Difference for a Single Variable across Two Independent Groups
#' (Chi-square Test of Independence)
#'
#' \code{prop_diff} tests for proportion differences across two independent
#' groups with a chi-square test of independence. The function also calculates
#' the descriptive statistics for each group, various standardized effect sizes
#' (e.g., Cramer's V), and can provide the 2x2 contingency tables.
#' \code{prop_diff} is simply a wrapper for \code{\link[stats]{prop.test}} plus
#' some extra calculations.
#'
#' @param x numeric vector that only has values of 0 or 1 (or missing values),
#'   otherwise known as a dummy variable.
#'
#' @param bin atomic vector that only takes on two values (or missing values),
#'   otherwise known as a binary variable.
#'
#' @param lvl character vector with length 2 specifying the unique values for
#'   the two groups. If \code{bin} is a factor, then \code{lvl} should be the
#'   factor levels rather than the underlying integer codes. This argument
#'   allows you to specify the direction of the prop difference.
#'   \code{prop_diff} calculates the prop difference as \code{x[ bin == lvl[2]
#'   ]} - \code{x[ bin == lvl[1] ]} such that it is group 2 - group 1. By
#'   changing which group is group 1 vs. group 2, the direction of the prop
#'   difference can be changed. See details.
#'
#' @param yates logical vector of length 1 specifying whether the Yate's
#'   continuity correction should be applied for small samples. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @param zero.cell numeric vector of length 1 specifying what value to impute
#'   for zero cell counts in the 2x2 contingency table when computing the
#'   tetrachoric correlation. See \code{\link[psych]{tetrachoric}} for details.
#'
#' @param smooth logical vector of length 1 specifying whether a smoothing
#'   algorithm should be applied when estimating the tetrachoric correlation.
#'   See \code{\link[psych]{tetrachoric}} for details.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of lengh 1 specifying whether the return
#'   object should include the 2x2 contingency table of counts with totals and
#'   the 2x2 overall percentages table. If TRUE, then the last two elements of
#'   the return object are "count" containing a matrix of counts and "percent"
#'   containing a matrix of overall percentages.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{bin} has more
#'   than 2 unique values (other than missing values) or if \code{bin} has
#'   length different than the length of \code{x}. This is a tradeoff between
#'   computational efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   mean difference: 1) nhst = chi-square test of independence stat info in a numeric vector,
#'   2) desc = descriptive statistics stat info in a numeric vector, 3) std = various
#'   standardized effect sizes in a numeric vector, 4) count = numeric matrix with
#'   dim = \code{[3, 3]} of the 2x2 contingency table of counts with an additional
#'   row and column for totals (if \code{rtn.table} = TRUE), 5) percent = numeric
#'   matrix with dim = \code{[3, 3]} of the 2x2 contingency table of overall percentages
#'   with an additional row and column for totals (if \code{rtn.table} = TRUE)
#'
#' 1) nhst = chi-square test of independence stat info in a numeric vector
#'
#' \describe{
#'    \item{est}{mean difference estimate (i.e., group 2 - group 1)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (will always be 1)}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector
#'
#' \describe{
#'    \item{prop_`lvl[2]`}{proportion of group 2}
#'    \item{prop_`lvl[1]`}{proportion of group 1}
#'    \item{sd_`lvl[2]`}{standard deviation of group 2}
#'    \item{sd_`lvl[1]`}{standard deviation of group 1}
#'    \item{n_`lvl[2]`}{sample size of group 2}
#'    \item{n_`lvl[1]`}{sample size of group 1}
#' }
#'
#' 3) std = various standardized effect sizes in a numeric vector
#'
#' \describe{
#'    \item{cramer}{Cramer's V estimate}
#'    \item{h}{Cohen's h estimate}
#'    \item{phi}{Phi coefficient estimate}
#'    \item{yule}{Yule coefficient estimate}
#'    \item{tetra}{Tetrachoric correlation estimate}
#'    \item{OR}{odds ratio estimate}
#'    \item{RR}{risk ratio estimate calculated as (i.e., group 2 / group 1).
#'    Note this value will often differ when recoding variables (as it should).}
#' }
#'
#' 4) count = numeric matrix with dim = \code{[3, 3]} of the 2x2 contingency table of
#' counts with an additional row and column for totals (if \code{rtn.table} = TRUE).
#'
#' The two unique observed values of \code{x} (i.e., 0 and 1) - plus the
#' total - are the rows and the two unique observed values of \code{bin} - plus
#' the total - are the columns. The dimlabels are "bin" for the rows and "x" for
#' the columns. The rownames are 1. "0", 2. "1", 3. "total". The colnames are 1.
#' `lvl[1]`, 2. `lvl[2]`, 3. "total"
#'
#' 5) percent = numeric matrix with dim = \code{[3, 3]} of the 2x2 contingency table of overall percentages with an additional
#' row and column for totals (if \code{rtn.table} = TRUE).
#'
#' The two unique observed values of \code{x} (i.e., 0 and 1) - plus the total -
#' are the rows and the two unique observed values of \code{bin} - plus the total -
#' are the columns. The dimlabels are "bin" for the rows and "x" for the columns.
#' The rownames are 1. "0", 2. "1", 3. "total". The colnames are 1. `lvl[1]`,
#' 2. `lvl[2]`, 3. "total"
#'
#' @seealso
#'    \code{\link[stats]{prop.test}} the workhorse for \code{prop_diff},
#'    \code{\link{props_diff}} for multiple dummy variables,
#'    \code{\link[psych]{phi}} for another phi coefficient function
#'    \code{\link[psych]{Yule}} for another yule coefficient function
#'    \code{\link[psych]{tetrachoric}} for another tetrachoric coefficient function
#'
#' @examples
#'
#' # chi-square test of independence
#' # x = "am", bin = "vs"
#' mtcars2 <- mtcars
#' mtcars2$"vs_bin" <- ifelse(mtcars$"vs" == 1, yes = "yes", no = "no")
#' agg(mtcars2$"am", grp = mtcars2$"vs_bin", rep = FALSE, fun = mean)
#' prop_diff(x = mtcars2$"am", bin = mtcars2$"vs_bin")
#' prop_diff(x = mtcars2$"am", bin = mtcars2$"vs")
#'
#' # using \code{lvl} argument
#' prop_diff(x = mtcars2$"am", bin = mtcars2$"vs_bin")
#' prop_diff(x = mtcars2$"am", bin = mtcars2$"vs_bin",
#'    lvl = c("yes","no")) # reverses the direction of the effect
#' prop_diff(x = mtcars2$"am", bin = mtcars2$"vs",
#'    lvl = c(1, 0)) # levels don't have to be character
#'
#' # recoding the variables
#' prop_diff(x = mtcars2$"am", bin = ifelse(mtcars2$"vs_bin" == "yes",
#'    yes = "no", no = "yes")) # reverses the direction of the effect
#' prop_diff(x = ifelse(mtcars2$"am" == 1, yes = 0, no = 1),
#'    bin = mtcars2$"vs") # reverses the direction of the effect
#' prop_diff(x = ifelse(mtcars2$"am" == 1, yes = 0, no = 1),
#'    bin = ifelse(mtcars2$"vs_bin" == "yes",
#'       yes = "no", no = "yes")) # double reverse means same direction of the effect
#'
#' # compare to stats::prop.test
#' # x = "am", bin = "vs_bin" (binary as the rows; dummy as the columns)
#' tmp <- c("vs_bin","am") # b/c Roxygen2 will cause problems
#' table_obj <- table(mtcars2[tmp])
#' row_order <- nrow(table_obj):1
#' col_order <- ncol(table_obj):1
#' table_obj4prop <- table_obj[row_order, col_order]
#' prop.test(table_obj4prop)
#'
#' # compare to stats:chisq.test
#' chisq.test(x = mtcars2$"am", y = mtcars2$"vs_bin")
#'
#' # compare to psych::phi
#' cor(mtcars2$"am", mtcars$"vs")
#' psych::phi(table_obj, digits = 7)
#'
#' # compare to psych::yule()
#' psych::Yule(table_obj)
#'
#' # compare to psych::tetrachoric
#' psych::tetrachoric(table_obj)
#' # Note, I couldn't find a case where psych::tetrachoric() failed to compute
#' psych::tetrachoric(table_obj4prop)
#'
#' # different than single logistic regression
#' summary(glm(am ~ vs, data = mtcars, family = binomial(link = "logit")))
#'
#' @export
prop_diff <- function(x, bin, lvl = levels(as.factor(bin)), yates = TRUE,
   zero.cell = .05, smooth = TRUE, ci.level = 0.95, rtn.table = TRUE, check = TRUE) {

   # error checking
   if (check) {
      if (length(x) != length(bin))
         stop("`x` and `bin` must have the same length")
      checkmate::assertNumeric(x)
      if(!(str2str::is.dummy(x)))
         stop("`x` must be a dummy variable that only has values 0 or 1 (or missing values)")
      if (length(na.omit(unique(bin))) != 2)
         stop("`bin` must contain exactly two unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, len = 2, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(bin)))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `bin` (other than missing values)")
      checkmate::assertLogical(yates, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(zero.cell, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(smooth, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # complete.cases
   lvl <- as.character(lvl) # so user doesn't have to convert to character themselves
   bin_fct <- factor(bin, levels = lvl) # works even if bin already is a factor
   dat0 <- data.frame("x" = x, "bin" = bin_fct)
   dat <- dat0[complete.cases(dat0), ]

   # prop.test
   table_obj <- table(dat[c("bin","x")], dnn = c("bin","x")) # so `x` values are the columns in the table
   table_obj4prop <- table_obj[nrow(table_obj):1, ncol(table_obj):1] # reverse both rows and columns
   prop_obj <- stats::prop.test(table_obj4prop, conf.level = ci.level, correct = yates)
   prop_by <- setNames(prop_obj[["estimate"]], nm = paste0("prop_", rev(lvl)))
   prop_vec <- c("est" = unname(prop_by[1] - prop_by[2]), # group 2 - group 1
      "se" = NA_real_,
      "X2" = unname(prop_obj[["statistic"]]),
      "df" = unname(prop_obj[["parameter"]]),
      "p" = prop_obj[["p.value"]],
      "lwr" = prop_obj[["conf.int"]][1],
      "upr" = prop_obj[["conf.int"]][2])

   # descriptives
   sd_by <- setNames(sqrt(prop_by * (1 - prop_by)), nm = paste0("sd_", rev(lvl)))
   n_by <- setNames(rev(rowSums(table_obj)), nm = paste0("n_", rev(lvl)))
   describes_vec <- c(prop_by, sd_by, n_by)

   # standardized effect sizes
   n_total <- sum(n_by)
   cramer <- unname(sqrt(prop_vec["X2"] / n_total))
   prop_asin <- 2 * asin(sqrt(prop_by))
   h_est <- unname(prop_asin[1] - prop_asin[2])
   row_total <- rowSums(table_obj)
   col_total <- colSums(table_obj)
   tmp <- rbind(table_obj, "total" = col_total)
   table_count <- cbind(tmp, "total" = c(row_total, n_total))
   str2str::dimlabels(table_count) <- c("bin","x")
   table_percent <- table_count / n_total
   phi_num <- (table_percent[1, 1] * table_percent[2, 2]) -
      (table_percent[1, 2] * table_percent[2, 1])
   phi_den <- table_percent["total", 1] * table_percent["total", 2] *
      table_percent[1, "total"] * table_percent[2, "total"]
   phi <- phi_num / sqrt(phi_den) # based on table_obj
   agree <- table_count[1, 1] * table_count[2, 2]
   disagree <- table_count[1, 2] * table_count[2, 1]
   yule <- (agree - disagree) / (agree + disagree)
   tetra_obj <- try(psych::tetrachoric(table_obj, correct = zero.cell, smooth = smooth),
      silent = TRUE)
   if (any(class(tetra_obj) != "try-error")) {
      tetra_coef <- tetra_obj[["rho"]] # no SE or CI included
   } else {
      tetra_coef <- NA_real_
      warning("Unable to compute tetrachoric correlation; NA is returned instead.")
   }
   OR <- agree / disagree
   RR <- unname(prop_by[1] / prop_by[2])
   effects_vec <- c("cramer" = cramer, "h" = h_est, "phi" = phi, "yule" = yule,
      "tetra" = tetra_coef, "OR" = OR, "RR" = RR)

   # return object
   rtn <- list("nhst" = prop_vec, "desc" = describes_vec, "std" = effects_vec)
   if (rtn.table)
      str2str::append(rtn) <- list("count" = table_count, "percent" = table_percent)
   return(rtn)
}

# props_diff

#' Proportion Difference of Multiple Variables Across Two Independent Groups
#' (Chi-square Tests of Independence)
#'
#' \code{props_diff} tests the proportion difference of multiple variables
#' across two independent groups with chi-square tests of independence. The
#' function also calculates the descriptive statistics for each group, various
#' standardized effect sizes (e.g., Cramer's V), and can provide the 2x2
#' contingency tables. \code{props_diff} is simply a wrapper for
#' \code{\link[stats]{prop.test}} plus some extra calculations.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector specifying the colnames in \code{data} for the
#'   variables. Since we are testing proportions, the variables must be dummy
#'   codes such that they only have values of 0 or 1 (or missing values).
#'
#' @param bin.nm character vector of length 1 specifying the colname in \code{data}
#'   for the binary variable that only takes on two values (or missing values),
#'   specifying the two independent groups.
#'
#' @param lvl character vector with length 2 specifying the unique values for
#'   the two groups. If \code{bin} is a factor, then \code{lvl} should be the
#'   factor levels rather than the underlying integer codes. This argument
#'   allows you to specify the direction of the prop difference.
#'   \code{prop_diff} calculates the prop differences as \code{x[ bin == lvl[2]
#'   ]} - \code{x[ bin == lvl[1] ]} such that it is group 2 - group 1. By
#'   changing which group is group 1 vs. group 2, the direction of the prop
#'   differences can be changed. See details of \code{\link{prop_diff}}.
#'
#' @param yates logical vector of length 1 specifying whether the Yate's
#'   continuity correction should be applied for small samples. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @param zero.cell numeric vector of length 1 specifying what value to impute
#'   for zero cell counts in the 2x2 contingency table when computing the
#'   tetrachoric correlations. See \code{\link[psych]{tetrachoric}} for details.
#'
#' @param smooth logical vector of length 1 specifying whether a smoothing
#'   algorithm should be applied when estimating the tetrachoric correlations.
#'   See \code{\link[psych]{tetrachoric}} for details.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of lengh 1 specifying whether the return
#'   object should include the 2x2 contingency table of counts with totals and
#'   the 2x2 overall percentages table. If TRUE, then the last two elements of
#'   the return object are "count" containing a 3D array of counts and "percent"
#'   containing a 3D array of overall percentages.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if
#'   \code{data[[bin.nm]]} has more than 2 unique values (other than missing
#'   values). This is a tradeoff between computational efficiency (FALSE) and
#'   more useful error messages (TRUE).
#'
#' @return list of data.frames containing statistical information about the prop
#'   differences (the rownames of each data.frame are \code{vrb.nm}): 1)
#'   chisqtest = chi-square tests of independence stat info in a data.frame, 2)
#'   describes = descriptive statistics stat info in a data.frame, 3) effects =
#'   various standardized effect sizes in a data.frame, 4) count = numeric 3D
#'   array with dim = \code{[3, 3, length(vrb.nm)]} of the 2x2 contingency
#'   tables of counts with additional rows and columns for totals (if
#'   \code{rtn.table} = TRUE), 5) percent = numeric 3D array with dim =
#'   \code{[3, 3, length(vrb.nm)]} of the 2x2 contingency tables of overall
#'   percentages with additional rows and columns for totals (if
#'   \code{rtn.table} = TRUE).
#'
#' 1) chisqtest = chi-square tests of independence stat info in a data.frame
#'
#' \describe{
#'    \item{est}{mean difference estimate (i.e., group 2 - group 1)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (will always be 1)}
#'    \item{p}{two-sided p-value}
#'    \item{lwr}{lower bound of the confidence interval}
#'    \item{upr}{upper bound of the confidence interval}
#' }
#'
#' 2) describes = descriptive statistics stat info in a data.frame
#'
#' \describe{
#'    \item{prop_`lvl[2]`}{proportion of group 2}
#'    \item{prop_`lvl[1]`}{proportion of group 1}
#'    \item{sd_`lvl[2]`}{standard deviation of group 2}
#'    \item{sd_`lvl[1]`}{standard deviation of group 1}
#'    \item{n_`lvl[2]`}{sample size of group 2}
#'    \item{n_`lvl[1]`}{sample size of group 1}
#' }
#'
#' 3) effects = various standardized effect sizes in a data.frame
#'
#' \describe{
#'    \item{cramer}{Cramer's V estimate}
#'    \item{h}{Cohen's h estimate}
#'    \item{phi}{Phi coefficient estimate}
#'    \item{yule}{Yule coefficient estimate}
#'    \item{tetra}{Tetrachoric correlation estimate}
#'    \item{OR}{odds ratio estimate}
#'    \item{RR}{risk ratio estimate calculated as (i.e., group 2 / group 1).
#'    Note this value will often differ when recoding variables (as it should).}
#' }
#'
#' 4) count = numeric 3D array with dim = \code{[3, 3, length(vrb.nm)]} of the
#' 2x2 contingency tables of counts with additional rows and columns for totals
#' (if \code{rtn.table} = TRUE).
#'
#'   The two unique observed values of \code{data[vrb.nm]} (i.e., 0 and 1) -
#'   plus the total - are the rows and the two unique observed values of
#'   \code{data[[bin.nm]]} - plus the total - are the columns. The variables
#'   themselves as the layers (i.e., 3rd dimension of the array). The dimlabels
#'   are "bin" for the rows, "x" for the columns, and "vrb" for the layers. The
#'   rownames are 1. "0", 2. "1", 3. "total". The colnames are 1. `lvl[1]`, 2.
#'   `lvl[2]`, 3. "total". The laynames are \code{vrb.nm}.
#'
#' 5) percent = numeric 3D array with dim = \code{[3, 3, length(vrb.nm)]} of the
#' 2x2 contingency tables of overall percentages with additional rows and
#' columns for totals (if \code{rtn.table} = TRUE).
#'
#'   The two unique observed values of \code{data[vrb.nm]} (i.e., 0 and 1) -
#'   plus the total - are the rows and the two unique observed values of
#'   \code{data[[bin]]} - plus the total - are the columns. The variables
#'   themselves as the layers (i.e., 3rd dimension of the array). The dimlabels
#'   are "bin" for the rows, "x" for the columns, and "vrb" for the layers. The
#'   rownames are 1. "0", 2. "1", 3. "total". The colnames are 1. `lvl[1]`, 2.
#'   `lvl[2]`, 3. "total". The laynames are \code{vrb.nm}.
#'
#' @seealso
#'    \code{\link[stats]{prop.test}} the workhorse for \code{props_diff},
#'    \code{\link{prop_diff}} for a single dummy variable,
#'    \code{\link[psych]{phi}} for another phi coefficient function
#'    \code{\link[psych]{Yule}} for another yule coefficient function
#'    \code{\link[psych]{tetrachoric}} for another tetrachoric coefficient function
#'
#' @examples
#'
#' # rtn.table = TRUE (default)
#'
#' # multiple variables
#' mtcars2 <- mtcars
#' mtcars2$"vs_bin" <- ifelse(mtcars$"vs" == 1, yes = "yes", no = "no")
#' mtcars2$"gear_dum" <- ifelse(mtcars2$"gear" > 3, yes = 1L, no = 0L)
#' mtcars2$"carb_dum" <- ifelse(mtcars2$"carb" > 3, yes = 1L, no = 0L)
#' vrb_nm <- c("am","gear_dum","carb_dum") # dummy variables
#' lapply(X = vrb_nm, FUN = function(nm) {
#'    tmp <- c("vs_bin", nm)
#'    table(mtcars2[tmp])
#' })
#' props_diff(data = mtcars2, vrb.nm = c("am","gear_dum","carb_dum"), bin.nm = "vs_bin")
#'
#' # single variable
#' props_diff(mtcars2, vrb.nm = "am", bin.nm = "vs_bin")
#'
#' # rtn.table = FALSE (no "count" or "percent" list elements)
#'
#' # multiple variables
#' props_diff(data = mtcars2, vrb.nm = c("am","gear_dum","carb_dum"), bin.nm = "vs",
#'    rtn.table = FALSE)
#'
#' # single variable
#' props_diff(mtcars, vrb.nm = "am", bin.nm = "vs",
#'    rtn.table = FALSE)
#'
#' @export
props_diff <- function(data, vrb.nm, bin.nm, lvl = levels(as.factor(data[[bin.nm]])),
   yates = TRUE, zero.cell = 0.05, smooth = TRUE, ci.level = 0.95, rtn.table = TRUE,
   check = TRUE) {

   # error checking
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertNames(vrb.nm, subset.of = names(data))
      checkmate::assertNames(bin.nm, subset.of = names(data))
      if (length(na.omit(unique(data[[bin.nm]]))) != 2)
         stop("`data`[[`bin.nm`]] must contain exactly two unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, len = 2, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(data[[bin.nm]])))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `data`[[`bin.nm`]] (other than missing values)")
      checkmate::assertLogical(yates, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(zero.cell, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(smooth, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # function itself
   tmp <- lapply(X = str2str::sn(vrb.nm), FUN = function(nm) {
      prop_diff(x = data[[nm]], bin = data[[bin.nm]], lvl = lvl, yates = yates,
         zero.cell = zero.cell, smooth = smooth, ci.level = ci.level, rtn.table = rtn.table,
         check = FALSE)
   })
   if (!rtn.table) { # rtn.table = FALSE
      if (1 == length(vrb.nm)) {
         tmp2 <- tmp[[1]]
         rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
      } else {
         tmp2 <- str2str::t_list(tmp)
         rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
      }
   } else { # rtn.table = TRUE
      if (1 == length(vrb.nm)) {
         tmp2 <- tmp[[1]]
         rtn_front <- lapply(X = tmp2[1:3], FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
         rtn <- c(rtn_front, tmp2[4:5])
      } else {
         tmp2 <- str2str::t_list(tmp)
         rtn_front <- lapply(X = tmp2[1:3], FUN = str2str::lv2d, along = 1)
         rtn_back <- lapply(X = tmp2[4:5], FUN = str2str::lm2a, dim.order = c(1, 2, 3), dimlab.list = "vrb")
         rtn <- c(rtn_front, rtn_back)
      }
   }
   return(rtn)
}

# prop_compare

#' Proportion Comparisons for a Single Variable across 3+ Independent Groups
#' (Chi-square Test of Independence)
#'
#' \code{prop_compare} tests for proportion differences across 3+ independent
#' groups with a chi-square test of independence. The function also calculates
#' the descriptive statistics for each group, Cramer's V and its confidence
#' interval as a standardized effect size, and can provide the X by 2
#' contingency tables. \code{prop_compare} is simply a wrapper for
#' \code{\link[stats]{prop.test}} plus some extra calculations.
#'
#' The confidence interval for Cramer's V is calculated with fisher's r to z
#' transformation as Cramer's V is a kind of multiple correlation coefficient.
#' Cramer's V is transformed to fisher's z units, a symmetric confidence
#' interval for fisher's z is calculated, and then the lower and upper bounds
#' are back-transformed to Cramer's V units.
#'
#' @param x numeric vector that only has values of 0 or 1 (or missing values),
#'   otherwise known as a dummy variable.
#'
#' @param nom atomic vector that takes on three or more unordered values (or
#'   missing values), otherwise known as a nominal variable.
#'
#' @param lvl character vector with length 2 specifying the unique values for
#'   the two groups. If \code{nom} is a factor, then \code{lvl} should be the
#'   factor levels rather than the underlying integer codes. This argument
#'   allows you to specify order of the proportions in the return object.
#'
#' @param yates logical vector of length 1 specifying whether the Yate's
#'   continuity correction should be applied for small samples. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of lengh 1 specifying whether the return
#'   object should include the X by 2 contingency table of counts with totals
#'   and the X by 2 overall percentages table. If TRUE, then the last two
#'   elements of the return object are "count" containing a matrix of counts and
#'   "percent" containing a matrix of overall percentages.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{nom} has
#'   length different than the length of \code{x}. This is a tradeoff between
#'   computational efficiency (FALSE) and more useful error messages (TRUE).
#'
#' @return list of numeric vectors containing statistical information about the
#'   proportion comparisons: 1) nhst = chi-square test of independence stat info
#'   in a numeric vector, 2) desc = descriptive statistics stat info in a
#'   numeric vector, 3) std = standardized effect size and its confidence
#'   interval in a numeric vector, 4) count = numeric matrix with dim =
#'   \code{[X+1, 3]} of the X by 2 contingency table of counts with an
#'   additional row and column for totals (if \code{rtn.table} = TRUE), 5)
#'   percent = numeric matrix with dim = \code{[X+1, 3]} of the X by 2
#'   contingency table of overall percentages with an additional row and column
#'   for totals (if \code{rtn.table} = TRUE).
#'
#' 1) nhst = chi-square test of independence stat info in a numeric vector
#'
#' \describe{
#'    \item{est}{average proportion difference absolute value (i.e., |group j - group i|)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (of the nominal variable)}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) desc = descriptive statistics stat info in a numeric vector (note there
#' could be more than 3 groups - groups i, j, and k are just provided as an example):
#'
#' \describe{
#'    \item{prop_`lvl[k]`}{proportion of group k}
#'    \item{prop_`lvl[j]`}{proportion of group j}
#'    \item{prop_`lvl[i]`}{proportion of group i}
#'    \item{sd_`lvl[k]`}{standard deviation of group k}
#'    \item{sd_`lvl[j]`}{standard deviation of group j}
#'    \item{sd_`lvl[i]`}{standard deviation of group i}
#'    \item{n_`lvl[k]`}{sample size of group k}
#'    \item{n_`lvl[j]`}{sample size of group j}
#'    \item{n_`lvl[i]`}{sample size of group i}
#' }
#'
#' 3) std = standardized effect size and its confidence interval in a numeric vector
#'
#' \describe{
#'    \item{cramer}{Cramer's V estimate}
#'    \item{lwr}{lower bound of Cramer's V confidence interval}
#'    \item{upr}{upper bound of Cramer's V confidence interval}
#' }
#'
#' 4) count = numeric matrix with dim = \code{[X+1, 3]} of the X by 2
#' contingency table of counts with an additional row and column for totals (if
#' \code{rtn.table} = TRUE).
#'
#' The 3+ unique observed values of \code{nom} - plus the total - are the rows
#' and the two unique observed values of \code{x} (i.e., 0 and 1) - plus the
#' total - are the columns. The dimlabels are "nom" for the rows and "x" for the
#' columns. The rownames are 1. `lvl[i]`, 2. `lvl[j]`, 3. `lvl[k]`, 4. "total".
#' The colnames are 1. "0", 2. "1", 3. "total".
#'
#' 5) percent = numeric matrix with dim = \code{[X+1, 3]} of the X by 2
#' contingency table of overall percentages with an additional row and column
#' for totals (if \code{rtn.table} = TRUE).
#'
#' The 3+ unique observed values of \code{nom} - plus the total - are the rows
#' and the two unique observed values of \code{x} (i.e., 0 and 1) - plus the
#' total - are the columns. The dimlabels are "nom" for the rows and "x" for the
#' columns. The rownames are 1. `lvl[i]`, 2. `lvl[j]`, 3. `lvl[k]`, 4. "total".
#' The rownames are 1. "0", 2. "1", 3. "total".
#'
#' @seealso
#'    \code{\link[stats]{prop.test}} the workhorse for \code{prop_compare},
#'    \code{\link{props_compare}} for multiple dummy variables,
#'    \code{\link{prop_diff}} for only 2 independent groups (aka binary variable),
#'
#' @examples
#'
#' tmp <- replicate(n = 10, expr = mtcars, simplify = FALSE)
#' mtcars2 <- str2str::ld2d(tmp)
#' mtcars2$"cyl_fct" <- car::recode(mtcars2$"cyl",
#'    recodes = "4='four'; 6='six'; 8='eight'", as.factor = TRUE)
#' prop_compare(x = mtcars2$"am", nom = mtcars2$"cyl_fct")
#' prop_compare(x = mtcars2$"am", nom = mtcars2$"cyl_fct",
#'    lvl = c("four","six","eight")) # specify order of levels in return object
#'
#' # more than 3 groups
#' prop_compare(x = ifelse(airquality$"Wind" >= 10, yes = 1, no = 0), nom = airquality$"Month")
#' prop_compare(x = ifelse(airquality$"Wind" >= 10, yes = 1, no = 0), nom = airquality$"Month",
#'    rtn.table = FALSE) # no contingency tables
#'
#' @export
prop_compare <- function(x, nom, lvl = levels(as.factor(nom)), yates = TRUE,
   ci.level = 0.95, rtn.table = TRUE, check = TRUE) {

   # error checking
   if (check) {
      if (length(x) != length(nom))
         stop("`x` and `nom` must have the same length")
      if (length(na.omit(unique(x))) != 2)
         stop("`x` must contain exactly two unique values (excluding missing values)")
      if (length(na.omit(unique(nom))) < 2)
         stop("`nom` must contain two or more unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(nom)))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `nom` (other than missing values)")
      checkmate::assertLogical(yates, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # complete.cases
   lvl <- as.character(lvl) # so user doesn't have to convert to character themselves
   nom_fct <- factor(nom, levels = lvl) # works even if bin already is a factor
   dat0 <- data.frame("x" = x, "nom" = nom_fct)
   dat <- dat0[complete.cases(dat0), ]

   # prop.test
   table_obj <- table(dat[c("nom","x")], dnn = c("nom","x")) # so `x` values are the columns in the table
   table_obj4prop <- table_obj[nrow(table_obj):1, ncol(table_obj):1] # reverse both rows and columns
   prop_obj <- stats::prop.test(table_obj4prop, correct = yates) # we aren't using the conf.level
   prop_by <- setNames(prop_obj[["estimate"]], nm = paste0("prop_", rev(lvl)))
   diff_mat <- outer(X = prop_by, Y = prop_by, FUN = `-`)
   diff_avg <- mean(abs(diff_mat[lower.tri(diff_mat)]))
   prop_vec <- c("diff_avg" = diff_avg, # average difference between the proportions
      "se" = NA_real_,
      "X2" = unname(prop_obj[["statistic"]]),
      "df" = unname(prop_obj[["parameter"]]),
      "p" = prop_obj[["p.value"]])

   # descriptives
   sd_by <- setNames(sqrt(prop_by * (1 - prop_by)), nm = paste0("sd_", rev(lvl)))
   n_by <- setNames(rev(rowSums(table_obj)), nm = paste0("n_", rev(lvl)))
   descibes_vec <- c(prop_by, sd_by, n_by)

   # standardized effect sizes
   n_total <- sum(n_by)
   cramer <- unname(sqrt(prop_vec["X2"] / (n_total * prop_vec["df"])))
   fisher_z <- atanh(cramer)
   se_z <- 1 / sqrt(n_total - 3)
   tmp <- confint2.default(obj = fisher_z, se = se_z, level = ci.level)
   ci_z <- str2str::d2v(tmp[, c("lwr","upr"), drop = FALSE])
   ci_cramer <- tanh(ci_z)
   # prop_asin <- 2 * asin(sqrt(prop_by))
   # h_mat <- outer(X = prop_asin, Y = prop_asin, FUN = `-`)
   # h_avg <- mean(abs(h_mat[lower.tri(h_mat)]))
   effects_vec <- c("cramer" = cramer,  ci_cramer)

   # tables
   row_total <- rowSums(table_obj)
   col_total <- colSums(table_obj)
   tmp <- rbind(table_obj, "total" = col_total)
   table_count <- cbind(tmp, "total" = c(row_total, n_total))
   str2str::dimlabels(table_count) <- c("nom","x")
   table_percent <- table_count / n_total

   # return object
   rtn <- list("nhst" = prop_vec, "desc" = descibes_vec, "std" = effects_vec)
   if (rtn.table)
      str2str::append(rtn) <- list("count" = table_count, "percent" = table_percent)
   return(rtn)
}

# props_compare

#' Proportion Comparisons for Multiple Variables across 3+ Independent Groups
#' (Chi-square Tests of Independence)
#'
#' \code{prop_compare} tests for proportion differences across 3+ independent
#' groups with chi-square tests of independence. The function also calculates
#' the descriptive statistics for each group, Cramer's V and its confidence
#' interval as a standardized effect size, and can provide the X by 2
#' contingency tables. \code{prop_compare} is simply a wrapper for
#' \code{\link[stats]{prop.test}} plus some extra calculations.
#'
#' The confidence interval for Cramer's V is calculated with fisher's r to z
#' transformation as Cramer's V is a kind of multiple correlation coefficient.
#' Cramer's V is transformed to fisher's z units, a symmetric confidence
#' interval for fisher's z is calculated, and then the lower and upper bounds
#' are back-transformed to Cramer's V units.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   dummy variables, in other words, variables that only have values of 0 or 1
#'   (or missing values).
#'
#' @param nom.nm character vector of length 1 specifying the colname in
#'   \code{data} containing a nominal variable that takes on three or more
#'   unordered values (or missing values).
#'
#' @param lvl character vector with length 3+ specifying the unique values for
#'   the 3+ independent groups. If \code{nom} is a factor, then \code{lvl}
#'   should be the factor levels rather than the underlying integer codes. This
#'   argument allows you to specify order of the proportions in the return
#'   object.
#'
#' @param yates logical vector of length 1 specifying whether the Yate's
#'   continuity correction should be applied for small samples. See
#'   \code{\link[stats]{chisq.test}} for details.
#'
#' @param ci.level numeric vector of length 1 specifying the confidence level.
#'   \code{ci.level} must range from 0 to 1.
#'
#' @param rtn.table logical vector of lengh 1 specifying whether the return
#'   object should include the X by 2 contingency table of counts with totals
#'   for each dummy variable and the X by 2 overall percentages table with
#'   totals for each dummy variable. If TRUE, then the last two elements of the
#'   return object are "count" containing an array of counts  and "percent"
#'   containing an array of overall percentages.
#'
#' @param check logical vector of length 1 specifying whether the input
#'   arguments should be checked for errors. For example, if \code{lvl} has
#'   values that are not present in \code{data[[nom.nm]]}. This is a tradeoff
#'   between computational efficiency (FALSE) and more useful error messages
#'   (TRUE).
#'
#' @return list of data.frames containing statistical information about the
#'   proportion comparisons: 1) nhst = chi-square test of independence stat info
#'   in a data.frame, 2) desc = descriptive statistics stat info in a data.frame
#'   (note there could be more than 3 groups - groups i, j, and k are just
#'   provided as an example), 3) std = standardized effect size and its
#'   confidence interval in a data.frame, 4) count = numeric array with dim =
#'   \code{[X+1, 3, length(vrb.nm)]} of the X by 2 contingency table of counts
#'   for each dummy variable with an additional row and column for totals (if
#'   \code{rtn.table} = TRUE), 5) percent = numeric array with dim = \code{[X+1,
#'   3, length(vrb.nm)]} of the X by 2 contingency table of overall percentages
#'   for each dummy variable with an additional row and column for totals (if
#'   \code{rtn.table} = TRUE).
#'
#' 1) nhst = chi-square test of independence stat info in a data.frame
#'
#' \describe{
#'    \item{est}{average proportion difference absolute value (i.e., |group j - group i|)}
#'    \item{se}{NA (to remind the user there is no standard error for the test)}
#'    \item{X2}{chi-square value}
#'    \item{df}{degrees of freedom (of the nominal variable)}
#'    \item{p}{two-sided p-value}
#' }
#'
#' 2) desc = descriptive statistics stat info in a data.frame (note there
#' could be more than 3 groups - groups i, j, and k are just provided as an example):
#'
#' \describe{
#'    \item{prop_`lvl[k]`}{proportion of group k}
#'    \item{prop_`lvl[j]`}{proportion of group j}
#'    \item{prop_`lvl[i]`}{proportion of group i}
#'    \item{sd_`lvl[k]`}{standard deviation of group k}
#'    \item{sd_`lvl[j]`}{standard deviation of group j}
#'    \item{sd_`lvl[i]`}{standard deviation of group i}
#'    \item{n_`lvl[k]`}{sample size of group k}
#'    \item{n_`lvl[j]`}{sample size of group j}
#'    \item{n_`lvl[i]`}{sample size of group i}
#' }
#'
#' 3) std = standardized effect size and its confidence interval in a data.frame
#'
#' \describe{
#'    \item{cramer}{Cramer's V estimate}
#'    \item{lwr}{lower bound of Cramer's V confidence interval}
#'    \item{upr}{upper bound of Cramer's V confidence interval}
#' }
#'
#' 4) count = numeric array with dim = \code{[X+1, 3, length(vrb.nm)]} of the X
#' by 2 contingency table of counts for each dummy variable with an additional
#' row and column for totals (if \code{rtn.table} = TRUE).
#'
#' The 3+ unique observed values of \code{data[[nom.nm]]} - plus the total - are
#' the rows and the two unique observed values of \code{data[[vrb.nm]]} (i.e., 0
#' and 1) - plus the total - are the columns. The variables in
#' \code{data[vrb.nm]} are the layers. The dimlabels are "nom" for the rows and
#' "x" for the columns and "vrb" for the layers. The rownames are 1. `lvl[i]`,
#' 2. `lvl[j]`, 3. `lvl[k]`, 4. "total". The colnames are 1. "0", 2. "1", 3.
#' "total". The laynames are \code{vrb.nm}.
#'
#' 5) percent = numeric array with dim = \code{[X+1, 3, length(vrb.nm)]} of the
#' X by 2 contingency table of overall percentages for each dummy variable with
#' an additional row and column for totals (if \code{rtn.table} = TRUE).
#'
#' The 3+ unique observed values of \code{data[[nom.nm]]} - plus the total - are
#' the rows and the two unique observed values of \code{data[[vrb.nm]]} (i.e., 0
#' and 1) - plus the total - are the columns. The variables in
#' \code{data[vrb.nm]} are the layers. The dimlabels are "nom" for the rows, "x"
#' for the columns, and "vrb" for the layers. The rownames are 1. `lvl[i]`, 2.
#' `lvl[j]`, 3. `lvl[k]`, 4. "total". The colnames are 1. "0", 2. "1", 3.
#' "total". The laynames are \code{vrb.nm}.
#'
#' @seealso
#'    \code{\link[stats]{prop.test}} the workhorse for \code{prop_compare},
#'    \code{\link{prop_compare}} for a single dummy variable,
#'    \code{\link{props_diff}} for only 2 independent groups (aka binary variable),
#'
#' @examples
#'
#' # rtn.table = TRUE (default)
#'
#' # multiple variables
#' tmp <- replicate(n = 10, expr = mtcars, simplify = FALSE)
#' mtcars2 <- str2str::ld2d(tmp)
#' mtcars2$"gear_dum" <- ifelse(mtcars2$"gear" > 3, yes = 1L, no = 0L)
#' mtcars2$"carb_dum" <- ifelse(mtcars2$"carb" > 3, yes = 1L, no = 0L)
#' vrb_nm <- c("am","gear_dum","carb_dum") # dummy variables
#' lapply(X = vrb_nm, FUN = function(nm) {
#'    tmp <- c("cyl", nm)
#'    table(mtcars2[tmp])
#' })
#' props_compare(data = mtcars2, vrb.nm = c("am","gear_dum","carb_dum"), nom.nm = "cyl")
#'
#' # single variable
#' props_compare(mtcars2, vrb.nm = "am", nom.nm = "cyl")
#'
#' # rtn.table = FALSE (no "count" or "percent" list elements)
#'
#' # multiple variables
#' props_compare(data = mtcars2, vrb.nm = c("am","gear_dum","carb_dum"), nom.nm = "cyl",
#'    rtn.table = FALSE)
#'
#' # single variable
#' props_compare(mtcars2, vrb.nm = "am", nom.nm = "cyl",
#'    rtn.table = FALSE)
#'
#' # more than 3 groups
#' airquality2 <- airquality
#' airquality2$"Wind_dum" <- ifelse(airquality$"Wind" >= 10, yes = 1, no = 0)
#' airquality2$"Solar.R_dum" <- ifelse(airquality$"Solar.R" >= 100, yes = 1, no = 0)
#' props_compare(airquality2, vrb.nm = c("Wind_dum","Solar.R_dum"), nom.nm = "Month")
#' props_compare(airquality2, vrb.nm = "Wind_dum", nom.nm = "Month")
#'
#' @export
props_compare <- function(data, vrb.nm, nom.nm, lvl = levels(as.factor(data[[nom.nm]])),
   yates = TRUE, ci.level = 0.95, rtn.table = TRUE, check = TRUE) {

   # error checking
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertNames(vrb.nm, subset.of = names(data))
      checkmate::assertNames(nom.nm, subset.of = names(data))
      if (length(na.omit(unique(data[[nom.nm]]))) < 2)
         stop("`data`[[`nom.nm`]] must contain two or more unique values (excluding missing values)")
      checkmate::assertAtomic(lvl, any.missing = FALSE, unique = TRUE)
      if (length(setdiff(x = as.character(lvl), as.character(unique(data[[nom.nm]])))) > 0)
         stop("as.character(`lvl`) must contain only the unique values of `data`[[`nom.nm`]] (other than missing values)")
      checkmate::assertLogical(yates, len = 1, any.missing = FALSE)
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      checkmate::assertLogical(rtn.table, len = 1, any.missing = FALSE)
   }

   # function itself
   tmp <- lapply(X = str2str::sn(vrb.nm), FUN = function(nm) {
      prop_compare(x = data[[nm]], nom = data[[nom.nm]], lvl = lvl, yates = yates,
         ci.level = ci.level, rtn.table = rtn.table, check = FALSE)
   })
   if (!rtn.table) { # rtn.table = FALSE
      if (1 == length(vrb.nm)) {
         tmp2 <- tmp[[1]]
         rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
      } else {
         tmp2 <- str2str::t_list(tmp)
         rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
      }
   } else { # rtn.table = TRUE
      if (1 == length(vrb.nm)) {
         tmp2 <- tmp[[1]]
         rtn_front <- lapply(X = tmp2[1:3], FUN = str2str::v2d, along = 1, rtn.dim.nm = vrb.nm)
         rtn <- c(rtn_front, tmp2[4:5])
      } else {
         tmp2 <- str2str::t_list(tmp)
         rtn_front <- lapply(X = tmp2[1:3], FUN = str2str::lv2d, along = 1)
         rtn_back <- lapply(X = tmp2[4:5], FUN = str2str::lm2a, dim.order = c(1, 2, 3), dimlab.list = "vrb")
         rtn <- c(rtn_front, rtn_back)
      }
   }
   return(rtn)
}
