# PSYMET ####

#' @include quest_functions.R
NULL

# CONFINT2 ####

# nhst #

#' Null Hypothesis Significance Testing
#'
#' \code{nhst} computes the statistical information for null hypothesis
#' significance testing (NHST), t-values, p-values, etc., from parameter
#' estimates, standard errors, and degrees of freedom. If degrees of freedom are
#' not applicable or available, then \code{df} can be set to \code{Inf} (the
#' default) and z-values rather than t-values will be computed.
#'
#' @param est numeric vector of parameter estimates.
#'
#' @param se numeric vector of standard errors. Must be the same length as
#'   \code{est}.
#'
#' @param df numeric vector of degrees of freedom. Must be length of 1 or have
#'   same length as \code{est} and \code{se}. If degrees of freedom are not
#'   applicable or available, then \code{df} can be set to \code{Inf} (the
#'   default) and z-values rather than t-values will be computed. Note,
#'   \code{df} can be non-integers with decimals.
#'
#' @param p.value character vector of length 1 specifying the type of p-values
#'   to compute. The options are 1) "two.sided" which computed non-directional,
#'   two-tailed p-values, 2) "less", which computes negative-directional,
#'   one-tailed p-values, or 3) "greater", which computes positive-directional,
#'   one-tailed p-values.
#'
#' @param ci.level double vector of length 1 specifying the confidence level.
#'   Must be between 0 and 1 - or can be NULL in which case no confidence
#'   intervals are computed and the return object does not have the columns
#'   "lwr" or "upr".
#'
#' @return data.frame with nrow equal to the lengths of \code{est} and
#' \code{se}. The rownames are taken from \code{est}, unless \code{est} does not
#' have any names and then the rownames are taken from the names of \code{se}.
#' If neither have names, then the rownames are automatic (i.e.,
#' \code{1:nrow()}). The columns are the following:
#'
#' \describe{
#'    \item{est}{parameter estimates}
#'    \item{se}{standard errors}
#'    \item{t}{t-values (z-values if df = Inf)}
#'    \item{df}{degrees of freedom}
#'    \item{p}{p-values}
#'    \item{lwr}{lower bound of the confidence intervals (excluded if \code{ci.level =  NULL})}
#'    \item{upr}{upper bound of the confidence intervals (excluded if \code{ci.level =  NULL})}
#' }
#'
#' @seealso
#'    \code{\link{confint2.default}}
#'
#' @examples
#'
#' est <- colMeans(attitude)
#' se <- apply(X = str2str::d2m(attitude), MARGIN = 2, FUN = function(vec)
#'    sqrt(var(vec) / length(vec)))
#' df <- nrow(attitude) - 1
#' nhst(est = est, se = se, df = df)
#' nhst(est = est, se = se) # default is df = Inf resulting in z-values
#' nhst(est = est, se = se, df = df, ci.level = NULL) # no "lwr" or "upr" columns
#' nhst(est = est, se = se, df = df, ci.level = 0.99)
#'
#' @export
nhst <- function(est, se, df = Inf, ci.level = 0.95, p.value = "two.sided") {

   t <- est / se
   if (p.value == "two.sided")
      p <- 2 * pt(q = abs(t), df = df, lower.tail = FALSE)
   if (p.value == "less")
      p <- pt(q = t, df = df, lower.tail = TRUE)
   if (p.value == "greater")
      p <- pt(q = t, df = df, lower.tail = FALSE)
   rtn <- data.frame("est" = est, "se" = se, "t" = t, "df" = df, "p" = p)
   if (!(is.null(ci.level))) {
      crit <- qt(p = ci.level +  ((1 - ci.level) / 2), df = df, lower.tail = TRUE)
      hw <- se * crit
      ci <- cbind("lwr" = est - hw, "upr" = est + hw)
      rtn <- cbind.data.frame(rtn, ci)
   }
   return(rtn)
}

# confint2 #

#' Confidence Intervals from Statistical Information
#'
#' \code{confint2} is a generic function for creating confidence intervals from
#' various statistical information (e.g., \code{\link{confint2.default}}) or
#' object classes (e.g., \code{\link{confint2.boot}}). It is an alternative to
#' the original \code{\link[stats]{confint}} generic function in the \code{stats}
#' package.
#'
#' @param obj object of a particular class (e.g., "boot") or the first argument
#' in the default method (e.g., the \code{obj} argument in \code{\link{confint2.default}})
#'
#' @param ... additional arguments specific to the particular method of \code{confint2}.
#'
#' @return depends on the particular method of \code{confint2}, but usually a data.frame
#' with a column for the parameter estimate ("est"), standard error ("se"),
#' lower bound of the confidence interval ("lwr"), and upper bound of the confidence interval ("upr").
#'
#' @seealso
#'    \code{\link{confint2.default}} for the default method,
#'    \code{\link{confint2.boot}} for the \code{boot} method,
#'
#' @export
confint2 <- function(obj, ...) {
   UseMethod(generic = "confint2", object = obj)
}

# confint2.boot #

#' Bootstrapped Confidence Intervals from a \code{boot} Object
#'
#' \code{confint2.boot} is the \code{boot} method for the generic function
#' \code{\link{confint2}} and computes bootstrapped confidence intervals from an object
#' of class \code{boot} (aka an object returned by the function
#' \code{\link[boot]{boot}}. The function is a simple wrapper for the car boot
#' methods for the \code{summary} and \code{confint} generics. See
#' \code{\link[car]{hist.boot}} for details on those methods.
#'
#' The bias-corrected and accelerated percentile method (\code{boot.ci.type} =
#' "bca") will often fail if the number of bootstrapped resamples is less than
#' the sample size. Even still, it can fail for other reasons. Following
#' \code{car:::confint.boot}, \code{confint2.boot} gives a warning if the
#' bias-corrected and accelerated percentile method fails for any statistic, and
#' implicitly switches to the regular percentile method to prevent an error.
#' When multiple statistics were bootstrapped, it might be that the
#' bias-corrected and accelerated percentile method succeeded for most of the
#' statistics and only failed for one statistic; however, \code{confint2.boot}
#' will switch to using the regular percentile method for ALL the statistics.
#' This may change in the future.
#'
#' @param obj an object of class \code{boot} (aka an object returned by the
#'   function \code{\link[boot]{boot}}).
#'
#' @param boot.ci.type character vector of length 1 specifying the type of
#'   bootstrapped confidence interval to compute. The options are 1) "perc" for
#'   the regular percentile method, 2) "bca" for bias-corrected and accelerated
#'   percentile method, 3) "norm" for the normal method that uses the
#'   bootstrapped standard error to construct symmetrical confidence intervals
#'   with the classic formula around the bias-corrected estimate, and 4) "basic"
#'   for the basic method. Note, "stud" for the studentized method is NOT an
#'   option. See \code{\link[boot]{boot.ci}} for details. Although a more
#'   informative link is the following blogpost on bootstrapped confidence
#'   intervals with the boot package:
#'   \href{https://www.r-bloggers.com/2019/09/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/}{Click
#'   Here}.
#'
#' @param level double vector of length 1 specifying the confidence level. Must
#'   be between 0 and 1.
#'
#' @param ... This argument has no use. Technically, it is additional arguments
#'   for \code{confint2.boot}, but is only included for Roxygen2 to satisfy
#'   "checking S3 generic/method consistency".
#'
#' @return data.frame will be returned with nrow equal to the number of
#' statistics bootstrapped and columns specified below. The rownames are the
#' names in the "t0" element of the \code{boot} object (default data.frame
#' rownames if the "t0" element does not have any names). The columns are the
#' following:
#'
#' \describe{
#'    \item{est}{original parameter estimates}
#'    \item{se}{bootstrapped standard errors (does not differ by \code{boot.ci.type})}
#'    \item{lwr}{lower bound of the bootstrapped confidence intervals}
#'    \item{upr}{upper bound of the bootstrapped confidence intervals}
#' }
#'
#' @seealso
#'    \code{\link[boot]{boot.ci}}
#'    \code{\link[car]{hist.boot}}
#'
#' @examples
#'
#' # a single statistic
#' mean2 <- function(x, i) mean(x[i], na.rm = TRUE)
#' boot_obj <- boot::boot(data = attitude[[1]], statistic = mean2, R = 200L)
#' confint2.boot(boot_obj)
#' confint2.boot(boot_obj, boot.ci.type = "bca")
#' confint2.boot(boot_obj, level = 0.99)
#'
#' # multiple statistics
#' colMeans2 <- function(dat, i) colMeans(dat[i, ], na.rm = TRUE)
#' boot_obj <- boot::boot(data = attitude, statistic = colMeans2, R = 200L)
#' confint2.boot(boot_obj)
#' confint2.boot(boot_obj, boot.ci.type = "bca")
#' confint2.boot(boot_obj, level = 0.99)
#'
#' @export confint2.boot
#' @export
confint2.boot <- function(obj, boot.ci.type = "perc", level = 0.95, ...) {

   car_pkg <- requireNamespace("car", quietly = TRUE)
   if (!car_pkg) stop("The `car` package is required to use this function, but is not installed on this device")
   loadNamespace("car") # need this for summary() and confint() to call the boot methods in `car`
   est_boot <- obj[["t0"]]
   summary_boot <- summary(obj, high.moments = FALSE, extremes = FALSE) # car:::summary.boot
   se_boot <- summary_boot$"bootSE" # summary_boot is a data.frame
   ci_boot <- confint(obj, level = level, # car:::confint.boot
      type = boot.ci.type) # ci_boot is a matrix
   rtn <- data.frame(est_boot, se_boot, ci_boot)
   names(rtn) <- c("est","se","lwr","upr")
   if (nrow(rtn) < 1L) stop("`car:::confint.boot` did not return any rows")
   return(rtn)
}

# confint2.default #

#' Confidence Intervals from Parameter Estimates and Standard Errors
#'
#' \code{confint2.default} is the default method for the generic function
#' \code{\link{confint2}} and computes the statistical information for confidence
#' intervals from parameter estimates, standard errors, and degrees of freedom.
#' If degrees of freedom are not applicable or available, then \code{df} can be
#' set to \code{Inf} (the default) and critical z-values rather than critical
#' t-values will be used.
#'
#' @param obj numeric vector of parameter estimates. A better name for this
#' argument would be \code{est}; however, uses of S3 generic functions requires
#' the first argument to be the same name (i.e., \code{obj}) across methods.
#'
#' @param se numeric vector of standard errors. Must be the same length as
#'   \code{obj}.
#'
#' @param df numeric vector of degrees of freedom. Must have length 1 or the
#'   same length as \code{obj} and \code{se}. If degrees of freedom are not
#'   applicable or available, then \code{df} can be set to \code{Inf} (the
#'   default) and critical z-values rather than critical t-values will be used.
#'
#' @param level double vector of length 1 specifying the confidence level. Must
#'   be between 0 and 1.
#'
#' @param ... This argument has no use. Technically, it is additional arguments
#'   for \code{confint2.default}, but is only included for Roxygen2 to satisfy
#'   "checking S3 generic/method consistency".
#'
#' @return data.frame with nrow equal to the lengths of \code{obj} and
#' \code{se}. The rownames are taken from \code{obj}, unless \code{obj} does not
#' have any names and then the rownames are taken from the names of \code{se}.
#' If neither have names, then the rownames are automatic (i.e.,
#' \code{1:nrow()}). The columns are the following:
#'
#' \describe{
#'    \item{est}{parameter estimates}
#'    \item{se}{standard errors}
#'    \item{lwr}{lower bound of the confidence intervals}
#'    \item{upr}{upper bound of the confidence intervals}
#' }
#'
#' @seealso
#'    \code{\link{confint2.boot}}
#'    \code{\link{nhst}}
#'
#' @examples
#'
#' # single estimate
#' confint2.default(obj = 10, se = 3)
#'
#' # multiple estimates
#' est <- colMeans(attitude)
#' se <- apply(X = str2str::d2m(attitude), MARGIN = 2, FUN = function(vec)
#'    sqrt(var(vec) / length(vec)))
#' df <- nrow(attitude) - 1
#' confint2.default(obj = est, se = se, df = df)
#' confint2.default(obj = est, se = se) # default is df = Inf and use of ctitical z-values
#' confint2.default(obj = est, se = se, df = df, level = 0.99)
#'
#' # error
#' \dontrun{
#' confint2.default(obj = c(10, 12), se = c(3, 4, 5))
#' }
#'
#' @export confint2.default
#' @export
confint2.default <- function(obj, se, df = Inf, level = 0.95, ...) {

   if (length(obj) < 1L) stop("`obj` must have length greater than 0")
   if (length(obj) != length(se)) stop("`obj` and `se` must have the same length")
   crit <- qt(p = level + ((1 - level) / 2), df = df, lower.tail = TRUE)
   hw <- se * crit
   rtn <- data.frame("est" = obj, "se" = se, "lwr" = obj - hw, "upr" = obj + hw)
   return(rtn)
}

# boot_ci

#' Bootstrapped Confidence Intervals from a Matrix of Coefficients
#'
#' \code{boot_ci} computes bootstrapped confidence intervals from a matrix of
#' coefficients (or any statistical information of interest). The function is an
#' alternative to \code{confint2.boot} for when the user does not have an object
#' of class \code{boot}, but rather creates their own matrix of coefficients. It
#' has limited types of bootstrapped confidence intervals at the moment, but
#' future versions are expected to have more options.
#'
#' @param coef numeric matrix (or data.frame of numeric columns) of
#'   coefficients. The rows correspond to each bootstrapped resample and the
#'   columns to different coefficients. This is the equivalent of the "t"
#'   element in a \code{boot} object.
#'
#' @param est numeric vector of observed coefficients from the full sample. This
#'   is the equivalent of the "t0" element in a \code{boot} object. The default
#'   takes the mean of each coefficient across bootstrapped resamples; however,
#'   this usually results in small amount of bias in the coefficients.
#'
#' @param boot.ci.type character vector of length 1 specifying the type of
#'   bootstrapped confidence interval to compute. The options are 1) "perc2" for
#'   the naive percentile method using \code{\link{quantile}}, and 2) "norm2"
#'   for the normal method that uses the bootstrapped standard error to
#'   construct symmetrical confidence intervals with the classic formula around
#'   the estimate, The options have a "2" after them because, although they are
#'   conceptually similar to the "perc" and "norm" methods in the
#'   \code{\link[boot]{boot.ci}} function, they are slightly different
#'   mathematically.
#'
#' @param level double vector of length 1 specifying the confidence level. Must
#'   be between 0 and 1.
#'
#' @return data.frame will be returned with nrow equal to the number of
#' coefficients bootstrapped and columns specified below. The rownames are the
#' colnames in the \code{coef} argument or the names in the \code{est} argument
#' (default data.frame rownames if neither have any names). The columns are the
#' following:
#'
#' \describe{
#'    \item{est}{original parameter estimates}
#'    \item{se}{bootstrapped standard errors (does not differ by \code{boot.ci.type})}
#'    \item{lwr}{lower bound of the bootstrapped confidence intervals}
#'    \item{upr}{upper bound of the bootstrapped confidence intervals}
#' }
#'
#' @seealso
#'    \code{\link[boot]{boot.ci}} for the confidence interval function in the \code{boot} package,
#'    \code{\link[car]{confint.boot}} for an alternative function with \code{boot} objects
#'
#' @examples
#'
#' tmp <- replicate(n = 100, expr = {
#'    i <- sample.int(nrow(attitude), replace = TRUE)
#'    colMeans(attitude[i, ])
#' }, simplify = FALSE)
#' mat <- str2str::lv2m(tmp, along = 1)
#' boot_ci(mat, est = colMeans(attitude))
#'
#' @export
boot_ci <- function(coef, est = colMeans(coef), boot.ci.type = "perc2", level = 0.95) {

   boot.ci.type <- match.arg(boot.ci.type, choices = c("perc2","norm2")) # so arg abbreviations can be used
   mat <- as.matrix(coef)
   se <- apply(X = mat, MARGIN = 2, FUN = sd)
   if (boot.ci.type == "perc2") {
      tmp <- (1 - level) / 2
      probs <- c(tmp, level + tmp)
      ci <- apply(X = mat, MARGIN = 2, FUN = quantile, probs = probs) # apply() binds probs along rows
      rtn <- data.frame("est" = est, "se" = se, "lwr" = ci[1, ], "upr" = ci[2, ])
   }
   if (boot.ci.type == "norm2") {
      rtn <- confint2.default(obj = est, se = se, level = level)
   }
   return(rtn)
}

# CFA ####

# make.latent #

#' Make Model Syntax for a Latent Factor in Lavaan
#'
#' \code{make.latent} makes the model syntax for a latent factor in
#' \code{lavaan}. The return object can be used as apart of the model syntax for
#' calls to \code{\link[lavaan]{lavaan}}, \code{\link[lavaan]{sem}},
#' \code{\link[lavaan]{cfa}}, etc.
#'
#' @param x character vector specifying the colnames in your data that
#'   correspond to the variables indicating the latent factor (e.g.,
#'   questionnaire items).
#'
#' @param nm.latent character vector of length 1 specifying what the latent
#'   factor should be labeled as in the return object.
#'
#' @param error.var logical vector of length 1 specifying whether the model
#'   syntax for the error variances should be included in the return object.
#'
#' @param nm.par logical vector of length 1 specifying whether the model syntax
#'   should include names for the factor loading (and error variance)
#'   parameters.
#'
#' @param suffix.load character vector of length 1 specifying what string should
#'   be appended to the end of the elements of \code{x} when creating names for
#'   the factor loading parameters. Only used if \code{nm.par} is TRUE.
#'
#' @param suffix.error character vector of length 1 specifying what string
#'   should be appended to the end of the elements of \code{x} when creating
#'   names for the error variance parameters. Only used if \code{nm.par} is
#'   TRUE.
#'
#' @return character vector of length 1 providing the model syntax. The regular
#'   expression "\\n" is used to delineate new lines within the model
#'   syntax.
#'
#' @examples
#'
#' make.latent(x = names(psych::bfi)[1:5], error.var = FALSE, nm.par = FALSE)
#' make.latent(x = names(psych::bfi)[1:5], error.var = FALSE, nm.par = TRUE)
#' make.latent(x = names(psych::bfi)[1:5], error.var = TRUE, nm.par = FALSE)
#' make.latent(x = names(psych::bfi)[1:5], error.var = TRUE, nm.par = TRUE)
#'
#' @export
make.latent <- function(x, nm.latent = "latent", error.var = FALSE, nm.par = FALSE,
   suffix.load = "_l", suffix.error = "_e") {

   if (!(nm.par)) {
      load_syntax <- paste0(nm.latent, " =~ ", paste0(x, collapse = " + "))
      if (error.var) {
         error_syntax <- paste0(x, " ~~ ", x, collapse = " \n ")
         rtn <- paste(load_syntax, error_syntax, sep = " \n ")
      } else rtn <- load_syntax
   }

   if (nm.par) {
      load_labels <- paste0(x, suffix.load)
      load_syntax <- paste0(nm.latent, " =~ ", paste0(load_labels, "*", x, collapse = " + "))
      if (error.var) {
         error_labels <- paste0(x, suffix.error)
         error_syntax <- paste0(x, " ~~ ", error_labels, "*", x, collapse = " \n ")
         rtn <- paste(load_syntax, error_syntax, sep = " \n ")
      } else rtn <- load_syntax

   }
   return(rtn)
}

# ucfa #

#' Unidimensional Confirmatory Factor Analysis
#'
#' \code{ucfa} conducts a unidimensional confirmatory factor analysis on a set
#' of variables/items. Unidimensional meaning a one-factor model where all
#' variables/items load on that factor. The function is a wrapper for
#' \code{\link[lavaan]{cfa}} and returns an object of class "lavaan":
#' \link[lavaan:lavaan-class]{lavaan}. This then allows the user to extract
#' statistical information from the object (e.g.,
#' \code{\link[lavaan]{lavInspect}}). For details on all the arguments see
#' \code{\link[lavaan]{lavOptions}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} providing the
#'   variables/items
#'
#' @param std.ov logical vector of length 1 specifying if the variables/items
#'   should be standardized
#'
#' @param std.lv logical vector of length 1 specifying if the latent factor
#'   should be standardized resulting in all factor loadings being estimated. If
#'   FALSE, then the first variable/item in \code{data[vrb.nm]} is fixed to a
#'   factor loading of 1.
#'
#' @param ordered logical vector of length 1 specifying if the variables/items
#'   should be treated as ordered categorical items where polychoric
#'   correlations are used.
#'
#' @param meanstructure logical vector of length 1 specifying if the mean
#'   structure of the factor model should be estimated. This would be the
#'   variable/item intercepts (and latent factor mean if \code{std.lv} = FALSE).
#'   Note, this must be true to use Full Information Maximum Likelihood (FIML)
#'   to handle missing data via \code{missing} = "fiml".
#'
#' @param estimator character vector of length 1 specifying the estimator to use
#'   for parameter estimation. Popular options are 1) "ML" = maximum likelihood
#'   estimation based on the multivariate normal distribution, 2) "DWLS" =
#'   diagonally weighted least squares which uses the diagnonal of the weight
#'   matrix, 3) "WLS" for weighted least squares whiches uses the full weight
#'   matrix (often results in computational problems), 4) "ULS" for unweighted
#'   least squares that doesn't use a weight matrix. "DWLS", "WLS", and "ULS"
#'   can each be used with ordered categorical items when \code{ordered} = TRUE.
#'
#' @param se character vector of length 1 specifying how standard errors should
#'   be calculated. Popular options are 1) "standard" for conventional standard
#'   errors from inverting the information matrix, 2) "robust.sem" for robust
#'   standard errors, 3) "robust.huber.white" for sandwich standard errors.
#'
#' @param test character vector of length 1 specifying how the omnibus test
#'   statistic should be calculated. Popular options are 1) "standard" for the
#'   conventional chi-square statistic, 2) "Satorra-Bentler" for the
#'   Satorra-Bentler test statistic, 3) "Yaun.Bentler.Mplus" for the version of
#'   the Yuan-Bentler test statistic that Mplus uses, 4) "mean.var.adjusted" for
#'   a mean and variance adjusted test statistic, 5) "scaled.shifted" for the
#'   version of the mean and variance adjusted test statistic Mplus uses.
#'
#' @param missing character vector of length 1 specifying how to handle missing
#'   data. Popular options are 1) "fiml" = Full Information Maximum Likelihood
#'   (FIML), 2) "pairwise" = pairwise deletion, 3) "listwise" = listwise
#'   deletion.
#'
#' @param ... any other named arguments available in the
#'   \code{\link[lavaan]{cfa}} function. See \code{\link[lavaan]{lavOptions}}
#'   for the list of arguments.
#'
#' @return object of class "lavaan" \link[lavaan:lavaan-class]{lavaan}
#'   providing the return object from a call to \code{\link[lavaan]{cfa}}.
#'
#' @seealso
#'    \code{\link{summary_ucfa}}
#'    \code{\link[lavaan]{cfa}}
#'    \code{\link[lavaan]{lavaan}}
#'
#' @examples
#'
#' dat <- psych::bfi[1:250, 16:20] # nueroticism items
#' ucfa(data = dat, vrb.nm = names(dat))
#' ucfa(data = dat, vrb.nm = names(dat), std.ov = TRUE)
#' ucfa(data = dat, vrb.nm = names(dat), meanstructure = FALSE, missing = "pairwise")
#' ucfa(data = dat, vrb.nm = names(dat), estimator = "ML", # MLR
#'    se = "robust.huber.white", test = "yuan.bentler.mplus", missing = "fiml")
#' ucfa(data = dat, vrb.nm = names(dat), estimator = "ML", # MLM
#'    se = "robust.sem", test = "satorra.bentler", missing = "listwise")
#' ucfa(data = dat, vrb.nm = names(dat), ordered = TRUE, estimator = "DWLS", # WLSMV
#'    se = "robust", test = "scaled.shifted", missing = "listwise")
#'
#' @export
ucfa <- function(data, vrb.nm, std.ov = FALSE, std.lv = TRUE,
   ordered = FALSE, meanstructure = TRUE,
   estimator = "ML", se = "standard", test = "standard",
   missing = "fiml", ...) { # can't figure out how to use switch or multiple if statements in default arguments so that "DWLS" is default for `ordinal` = TRUE

   model_syntax <- make.latent(x = vrb.nm, nm.latent = "latent", error.var = FALSE, # need error.var = FALSE for categorical items
      nm.par = FALSE, suffix.load = "_l", suffix.error = "_e")
   lavaan_obj <- lavaan::cfa(model = model_syntax, data = data,
      std.ov = std.ov, std.lv = std.lv, ordered = ordered, meanstructure = meanstructure,
      estimator = estimator, se = se, test = test, missing = missing, ...)
   return(lavaan_obj)
}

# summary_ucfa #

#' Summary of a Unidimensional Confirmatory Factor Analysis
#'
#' \code{summary_ucfa} provides a summary of a unidimensional confirmatory
#' factor analysis on a set of variables/items. Unidimensional meaning a
#' one-factor model where all variables/items load on that factor. The function
#' is a wrapper for \code{\link[lavaan]{cfa}} and returns a list with four
#' vectors/matrices: 1) model info, 2) fit measures, 3) factor loadings, 4)
#' covariance/correlation residuals. For details on all the
#' \code{\link[lavaan]{cfa}} arguments see \code{\link[lavaan]{lavOptions}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} providing the
#'   variables/items
#'
#' @param std.ov logical vector of length 1 specifying if the variables/items
#'   should be standardized
#'
#' @param std.lv logical vector of length 1 specifying if the latent factor
#'   should be standardized resulting in all factor loadings being estimated. If
#'   FALSE, then the first variable/item in \code{data[vrb.nm]} is fixed to a
#'   factor loading of 1.
#'
#' @param ordered logical vector of length 1 specifying if the variables/items
#'   should be treated as ordered categorical items where polychoric
#'   correlations are used.
#'
#' @param meanstructure logical vector of length 1 specifying if the mean
#'   structure of the factor model should be estimated. This would be the
#'   variable/item intercepts (and latent factor mean if \code{std.lv} = FALSE).
#'   Note, this must be true to use Full Information Maximum Likelihood (FIML)
#'   to handle missing data via \code{missing} = "fiml".
#'
#' @param estimator character vector of length 1 specifying the estimator to use
#'   for parameter estimation. Popular options are 1) "ML" = maximum likelihood
#'   estimation based on the multivariate normal distribution, 2) "DWLS" =
#'   diagonally weighted least squares which uses the diagnonal of the weight
#'   matrix, 3) "WLS" for weighted least squares whiches uses the full weight
#'   matrix (often results in computational problems), 4) "ULS" for unweighted
#'   least squares that doesn't use a weight matrix. "DWLS", "WLS", and "ULS"
#'   can each be used with ordered categorical items when \code{ordered} = TRUE.
#'
#' @param se character vector of length 1 specifying how standard errors should
#'   be calculated. Popular options are 1) "standard" for conventional standard
#'   errors from inverting the information matrix, 2) "robust.sem" for robust
#'   standard errors, 3) "robust.huber.white" for sandwich standard errors.
#'
#' @param test character vector of length 1 specifying how the omnibus test
#'   statistic should be calculated. Popular options are 1) "standard" for the
#'   conventional chi-square statistic, 2) "Satorra-Bentler" for the
#'   Satorra-Bentler test statistic, 3) "Yaun.Bentler.Mplus" for the version of
#'   the Yuan-Bentler test statistic that Mplus uses, 4) "mean.var.adjusted" for
#'   a mean and variance adjusted test statistic, 5) "scaled.shifted" for the
#'   version of the mean and variance adjusted test statistic Mplus uses.
#'
#' @param missing character vector of length 1 specifying how to handle missing
#'   data. Popular options are 1) "fiml" = Full Information Maximum Likelihood
#'   (FIML), 2) "pairwise" = pairwise deletion, 3) "listwise" = listwise
#'   deletion.
#'
#' @param fit.measures character vector specifying which model fit indices to
#'   include in the return object. The default option includes the chi-square
#'   test statistic ("chisq"), degrees of freedom ("df"), tucker-lewis index
#'   ("tli"), comparative fit index ("cfi"), root mean square error of
#'   approximation ("rmsea"), and standardized root mean residual ("srmr").
#'   Note, if using robust corrections for \code{se} and \code{test}, you will
#'   probably want to call the scaled versions of model fit indices (e.g.,
#'   "chisq.scaled"). See \code{\link[lavaan]{fitMeasures}} for details.
#'
#' @param std.load logical vector of length 1 specifying whether the factor
#'   loadings included in the return object should be standardized (TRUE) or not
#'   (FALSE).
#'
#' @param resid.type character vector of length 1 specifying the type of
#'   covariance/correlation residuals to include in the return object. Popular
#'   options are 1) "raw" for conventional covariance residuals, 2) "cor.bollen"
#'   for conventional correlation residuals, 3) "cor.bentler" for correlation
#'   residuals that standardizes the model-implied covariance matrix with the
#'   observed variances, 4) "standardized" for conventional z-scores of the
#'   covariance residuals.
#'
#' @param add.class logical vector of length 1 specifying whether the lavaan
#'   classes should be added to the returned vectors/matrices (TRUE) or not
#'   (FALSE). These classes do not change the underlying vector/matrix and only
#'   affect printing.
#'
#' @param ... any other named arguments available in the
#'   \code{\link[lavaan]{cfa}} function. See \code{\link[lavaan]{lavOptions}}
#'   for the list of arguments.
#'
#' @return list of vectors/matrices providing statistical information about
#' the unidimensional confirmatory factor analysis. If \code{add.class} = TRUE,
#' then the elements have lavaan classes which affect printing (except for the
#' first "model_info" element which always is just an integer vector). The four
#' elements are:
#'
#' \describe{
#'    \item{model_info}{integer vector providing model information. The first element
#'    "converged" is 1 if the model converged and 0 if not. The second element
#'    "admissible" is 1 if the model is admissible (e.g., no negative variances)
#'    and 0 if not. The third element "nobs" is the number of observations used
#'    in the analysis. The fourth element "npar" is the number of parameter estimates.}
#'    \item{fit_measures}{double vector providing model fit indices. The number
#'    and names of the fit indices is determined by the \code{fit.measures} argument.}
#'    \item{factor_load}{1-column double matrix providing factor loadings. The colname
#'    is "latent" and the rownames are the \code{vrb.nm} argument.}
#'    \item{cov_resid}{covariance/correlation residuals for the model. Note, even
#'    though the name has "cov" in it, the residuals can be "cor" if the argument
#'    \code{resid.type} = "cor.bollen" or "cor.bentler".}
#' }
#'
#' @seealso
#'    \code{\link{ucfa}}
#'    \code{\link[lavaan]{cfa}}
#'    \code{\link[lavaan]{lavaan}}
#'
#' @examples
#'
#' # types of models
#' dat <- psych::bfi[1:250, 16:20] # nueroticism items
#' summary_ucfa(data = dat, vrb.nm = names(dat)) # default
#' summary_ucfa(data = dat, vrb.nm = names(dat), estimator = "ML", # MLR
#'    se = "robust.huber.white", test = "yuan.bentler.mplus", missing = "fiml",
#'    fit.measures = c("chisq.scaled","df.scaled","tli.scaled","cfi.scaled",
#'       "rmsea.scaled","srmr"))
#' summary_ucfa(data = dat, vrb.nm = names(dat), estimator = "ML", # MLM
#'    se = "robust.sem", test = "satorra.bentler", missing = "listwise",
#'    fit.measures = c("chisq.scaled","df.scaled","tli.scaled","cfi.scaled",
#'       "rmsea.scaled","srmr"))
#' summary_ucfa(data = dat, vrb.nm = names(dat), ordered = TRUE, estimator = "DWLS", # WLSMV
#'    se = "robust", test = "scaled.shifted", missing = "listwise",
#'    fit.measures = c("chisq.scaled","df.scaled","tli.scaled","cfi.scaled",
#'       "rmsea.scaled","wrmr"))
#'
#' # types of info
#' dat <- psych::bfi[1:250, 16:20] # nueroticism items
#' w <- summary_ucfa(data = dat, vrb.nm = names(dat))
#' x <- summary_ucfa(data = dat, vrb.nm = names(dat), add.class = FALSE)
#' y <- summary_ucfa(data = dat, vrb.nm = names(dat),
#'    std.load = FALSE, resid.type = "raw")
#' z <- summary_ucfa(data = dat, vrb.nm = names(dat),
#'    std.load = FALSE, resid.type = "raw", add.class = FALSE)
#' lapply(w, class)
#' lapply(x, class)
#' lapply(y, class)
#' lapply(z, class)
#'
#' @export
summary_ucfa <- function(data, vrb.nm, std.ov = FALSE, std.lv = TRUE,
   ordered = FALSE, meanstructure = TRUE,
   estimator = "ML", se = "standard", test = "standard", missing = "fiml",
   fit.measures = c("chisq","df","tli","cfi","rmsea","srmr"),
   std.load = TRUE, resid.type = "cor.bollen", add.class = TRUE, ...) {

   # ucfa call
   lavaan_obj <- ucfa(data = data, vrb.nm = vrb.nm, std.ov = std.ov, std.lv = std.lv,
      ordered = ordered, meanstructure = meanstructure,
      estimator = estimator, se = se, test = test,
      missing = missing, ...)

   # model info
   converged <- lavaan::lavInspect(lavaan_obj, what = "converged")
   admissible <- lavaan::lavInspect(lavaan_obj, what = "post.check")
   nobs <- lavaan::lavInspect(lavaan_obj, what = "nobs")
   npar <- lavaan::lavInspect(lavaan_obj, what = "npar")
   model_info <- c("converged" = converged, "admissible" = admissible, # converted from logical to integer automatically
      "nobs" = nobs, "npar" = npar)

   # fit measures
   fit_measures <- lavaan::fitMeasures(lavaan_obj, fit.measures = fit.measures)
   if (!add.class) fit_measures <- unclass(fit_measures)

   # factor loadings
   if (!std.load) {
      tmp <- lavaan::lavInspect(lavaan_obj, what = "est", add.labels = TRUE,
         add.class = add.class)
   }
   if (std.load) {
      tmp <- lavaan::lavInspect(lavaan_obj, what = "std", add.labels = TRUE,
         add.class = add.class)
   }
   factor_load <- tmp[["lambda"]]

   # cov residuals
   all_resid <- lavaan::residuals(lavaan_obj, type = resid.type)
   cov_resid <- all_resid[["cov"]]
   if (!add.class) cov_resid <- unclass(cov_resid)

   # return object
   rtn <- list("model_info" = model_info, "fit_measures" = fit_measures,
      "factor_load" = factor_load, "cov_resid" = cov_resid)
   return(rtn)
}

# COEF ####

# .cronbach #

#' Bootstrap Function for \code{cronbach()} Function
#'
#' \code{.cronbach} is the function used by the \code{\link[boot]{boot}} function
#' within the \code{\link{cronbach}} function. It is primarily created to increase the
#' computational efficiency of bootstrap confidence intervals within the
#' \code{cronbach} function by doing only the minimal computations needed to
#' compute cronbach's alpha.
#'
#' @param dat data.frame with only the items you wish to include in the cronbach's
#' alpha computation and no other variables.
#'
#' @param i integer vector of length = \code{nrow(dat)} specifying which rows
#' should be included in the computation. When used by the \code{boot::boot} function
#' this argument will change with every new bootstrapped resample.
#'
#' @param use character vector of length 1 specifying how missing data should be
#' handled when computing covariances. See \code{cov} for details.
#'
#' @return double vector of length 1 providing cronbach's alpha
#'
#' @examples
#' .cronbach(dat = attitude,
#'    i = sample(x = 1:nrow(attitude), size = nrow(attitude), replace = TRUE), use = "pairwise")
#' @export
.cronbach <- function(dat, i, use) {
   dat_boot <- dat[i, ]
   cov_mat <- cov(x = dat_boot, use = use)
   var_vec <- diag(cov_mat)
   sum_cov <- sum(cov_mat)
   sum_var <- sum(var_vec)
   k <- ncol(dat_boot)
   rtn <- (1 - (sum_var / sum_cov)) * (k / (k - 1))
   return(rtn)
}

# .cronbachs #

#' Bootstrap Function for \code{cronbachs()} Function
#'
#' \code{.cronbachs} is the function used by the \code{\link[boot]{boot}}
#' function within the \code{cronbachs} function. It is primarily created to
#' increase the computational efficiency of bootstrap confidence intervals
#' within the \code{cronbachs} function by doing only the minimal computations
#' needed to compute cronbach's alpha for each set of variables/items.
#'
#' @param dat data.frame of data. It can contain variables other than those used
#'   for cronbach's alpha calculation.
#'
#' @param i integer vector of length = \code{nrow(dat)} specifying which rows
#'   should be included in the computation. When used by the \code{boot::boot}
#'   function this argument will change with every new bootstrapped resample.
#'
#' @param nm.list list of character vectors specifying the sets of
#'   variables/items associated with each of the cronbach's alpha calculations.
#'
#' @param use character vector of length 1 specifying how missing data should be
#'   handled when computing covariances. See \code{cov} for details.
#'
#' @return double vector of length = \code{length(nm.list)} providing cronbach's
#'   alpha for each set of variables/items.
#'
#' @examples
#'
#' dat0 <- psych::bfi[1:250, ]
#' dat1 <- str2str::pick(x = dat0, val = c("A1","C4","C5","E1","E2","O2","O5",
#'    "gender","education","age"), not = TRUE, nm = TRUE)
#' vrb_nm_list <- lapply(X = str2str::sn(c("E","N","C","A","O")), FUN = function(nm) {
#'    str2str::pick(x = names(dat1), val = nm, pat = TRUE)})
#' .cronbachs(dat = dat1,
#'    i = sample(x = 1:nrow(dat1), size = nrow(dat1), replace = TRUE),
#'    nm.list = vrb_nm_list, use = "pairwise")
#' @export
.cronbachs <- function(dat, i, nm.list, use) { # `i` must be the second argument in the function

   tmp <- lapply(X = nm.list, FUN = function(nm) {
      dat_boot <- dat[i, nm]
      cov_mat <- cov(x = dat_boot, use = use)
      var_vec <- diag(cov_mat)
      sum_cov <- sum(cov_mat)
      sum_var <- sum(var_vec)
      k <- ncol(dat_boot)
      rtn <- (1 - (sum_var / sum_cov)) * (k / (k - 1))
      return(rtn)
   })
   unlist(tmp)
}

# cronbach #

#' Cronbach's Alpha of a Set of Variables/Items
#'
#' \code{cronbach} computes Cronbach's alpha for a set of variables/items as an
#' estimate of reliability for a score. There are three different options for
#' confidence intervals. Missing data can be handled by either pairwise deletion
#' (\code{use} = "pairwise.complete.obs") or listwise deletion (\code{use} =
#' "complete.obs"). \code{cronbach} is a wrapper for the
#' \code{\link[psych]{alpha}} function in the \code{psych} package.
#'
#' When \code{ci.type} = "classic" the confidence interval is based on the mean
#' covariance. It is the same as the confidence interval used by
#' \code{\link[psych]{alpha.ci}} (Feldt, Woodruff, & Salih, 1987). When
#' \code{ci.type} = "delta" the confidence interval is based on the delta method
#' of the covariance matrix. It is based on the standard error returned by
#' \code{\link[psych]{alpha}} (Duhachek & Iacobucci, 2004).
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames of \code{data} specifying the
#'   variables/items.
#'
#' @param ci.type character vector of length 1 specifying the type of confidence
#'   interval to compute. The options are 1) "classic" is the Feldt et al.
#'   (1987) procedure using only the mean covariance, 2) "delta" is the
#'   Duhhacheck & Iacobucci (2004) procedure using the delta method of the
#'   covariance matrix, or 3) "boot" is bootstrapped confidence intervals with
#'   the method specified by \code{boot.ci.type}.
#'
#' @param level double vector of length 1 with a value between 0 and 1
#'   specifying what confidence level to use.
#'
#' @param use character vector of length 1 specifying how to handle missing data
#'   when computing the covariances. The options are 1) "pairwise.complete.obs",
#'   2) "complete.obs", 3) "na.or.complete", 4) "all.obs", or 5) "everything".
#'   See details of \code{\link{cov}}.
#'
#' @param stats character vector specifying the additional statistical
#'   information you could like related to cronbach's alpha. Options are: 1)
#'   "std.alpha" = cronbach's alpha of the standardized variables/items, 2)
#'   "G6(smc)" = Guttman's Lambda 6 reliability, 3) "average_r" = mean
#'   correlation between the variables/items, 4) "median_r" = median correlation
#'   between the variables/items, 5) "mean" = mean of the the score from
#'   averaging the variables/items together, 6) "sd" = standard deviation of the
#'   scores from averaging the variables/items together, 7) "nvrb" = number of
#'   variables/items. The default is "average_r" and "nvrb".
#'
#' @param R integer vector of length 1 specifying the number of bootstrapped
#'   resamples to do. Only used when \code{ci.type} = "boot".
#'
#' @param boot.ci.type character vector of length 1 specifying the type of
#'   bootstrapped confidence interval to compute. The options are 1) "perc" for
#'   the regular percentile method, 2) "bca" for bias-corrected and accelerated
#'   percentile method, 3) "norm" for the normal method that uses the
#'   bootstrapped standard error to construct symmetrical confidence intervals
#'   with the classic formula around the bias-corrected estimate, and 4) "basic"
#'   for the basic method. Note, "stud" for the studentized method is NOT an
#'   option. See \code{\link[boot]{boot.ci}} as well as
#'   \code{\link{confint2.boot}} for details.
#'
#' @return double vector containing Cronbach's alpha, it's standard error, and
#'   it's confidence interval, followed by any statistics requested via the
#'   \code{stats} argument.
#'
#' @seealso
#'    \code{\link{cronbachs}}
#'    \code{\link{composite}}
#'
#' @references
#'
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for
#' coefficient alpha. Applied Psychological Measurement (11) 93-103.
#'
#' Duhachek, A. and Iacobucci, D. (2004). Alpha's standard error (ase): An accurate
#' and precise confidence interval estimate. Journal of Applied Psychology, 89(5):792-808.
#'
#' @examples
#'
#' tmp_nm <- c("A2","A3","A4","A5")
#' psych::alpha(psych::bfi[tmp_nm])[["total"]]
#' a <- suppressMessages(psych::alpha(attitude))[["total"]]["raw_alpha"]
#' a.ci <- psych::alpha.ci(a, n.obs = 30,
#'    n.var = 7, digits = 7) # n.var is optional and only needed to find r.bar
#' cronbach(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"), ci.type = "classic")
#' cronbach(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"), ci.type = "delta")
#' cronbach(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"), ci.type = "boot")
#' cronbach(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"), stats = NULL)
#'
#' \dontrun{
#' cronbach(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"), ci.type = "boot",
#'    boot.ci.type = "bca") # will automatically convert to "perc" when "bca" fails
#' }
#'
#' @export
cronbach <- function(data, vrb.nm, ci.type = "delta", level = 0.95, use = "pairwise.complete.obs",
   stats = c("average_r","nvrb"), R = 200L, boot.ci.type = "perc") {

   # estimates
   data_vrb <- data[vrb.nm]
   alpha_obj <- psych::alpha(x = data_vrb, use = use, na.rm = TRUE, check.keys = FALSE)
   alpha_total <- str2str::d2v(alpha_obj[["total"]]) # the total slot is a one row data.frame
   alpha_raw <- unname(alpha_total["raw_alpha"])
   stats_all <- c(alpha_total[alpha_total != "raw_alpha"], "nvrb" = alpha_obj[["nvar"]])
   stats_user <- stats_all[stats]

   # confidence interval
   if (ci.type == "classic") { # copied from psych::alpha.ci
      n <- length(complete.cases(data_vrb))
      alpha_hi <- 1 - (1 - alpha_raw) * qf(p = (1 - level) / 2, df1 = n - 1, df2 = Inf)
      alpha_lo <- 1 - (1 - alpha_raw) * qf(p = 1 - ((1 - level) / 2), df1 = n - 1, df2 = Inf)
      alpha_ci <- c("lwr" = alpha_lo, "upr" = alpha_hi)
      alpha_se <- NA_real_
      alpha_all <- c("est" = alpha_raw, "se" = alpha_se, alpha_ci)
   }
   if (ci.type == "delta") {
      n <- length(complete.cases(data_vrb))
      alpha_se <- unname(stats_all["ase"])
      tmp <- confint2.default(obj = alpha_raw, se = alpha_se, df = n - 1, level = level)
      alpha_all <- str2str::d2v(tmp)
   }
   if (ci.type == "boot") {
      boot_obj <- boot::boot(data = data_vrb, statistic = .cronbach, R = R, stype = "i",
         use = use) # this is how you specify the `use` argument in .cronbach()
      tmp <- confint2.boot(boot_obj, boot.ci.type = boot.ci.type, level = level)
      alpha_all <- str2str::d2v(tmp)
   }

   # return object
   rtn <- c(alpha_all, stats_user)
   return(rtn)
}

# cronbachs #

#' Cronbach's Alpha for Multiple Sets of Variables/Items
#'
#' \code{cronbachs} computes Cronbach's alpha for multiple sets of
#' variables/items as an estimate of reliability for multiple scores. There are
#' three different options for confidence intervals. Missing data can be handled
#' by either pairwise deletion (\code{use} = "pairwise.complete.obs") or
#' listwise deletion (\code{use} = "complete.obs"). \code{cronbachs} is a
#' wrapper for the \code{\link[psych]{alpha}} function in the \code{psych}
#' package.
#'
#' When \code{ci.type} = "classic" the confidence interval is based on the mean
#' covariance. It is the same as the confidence interval used by
#' \code{\link[psych]{alpha.ci}} (Feldt, Woodruff, & Salih, 1987). When
#' \code{ci.type} = "delta" the confidence interval is based on the delta method
#' of the covariance matrix. It is based on the standard error returned by
#' \code{\link[psych]{alpha}} (Duhachek & Iacobucci, 2004).
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm.list list of character vectors specifying the sets of
#'   variables/items. Each element of \code{vrb.nm.list} provides the colnames
#'   of \code{data} for that set of variables/items.
#'
#' @param ci.type character vector of length 1 specifying the type of confidence
#'   interval to compute. The options are 1) "classic" = the Feldt et al. (1987)
#'   procedure using only the mean covariance, 2) "delta" = the Duhhacheck &
#'   Iacobucci (2004) procedure using the delta method of the covariance matrix,
#'   or 3) "boot" = bootstrapped confidence intervals with the method specified
#'   by \code{boot.ci.type}.
#'
#' @param level double vector of length 1 with a value between 0 and 1
#'   specifying what confidence level to use.
#'
#' @param use character vector of length 1 specifying how to handle missing data
#'   when computing the covariances. The options are 1) "pairwise.complete.obs",
#'   2) "complete.obs", 3) "na.or.complete", 4) "all.obs", or 5) "everything".
#'   See details of \code{\link{cov}}.
#'
#' @param stats character vector specifying the additional statistical
#'   information you could like related to cronbach's alpha. Options are: 1)
#'   "std.alpha" = cronbach's alpha of the standardized variables/items, 2)
#'   "G6(smc)" = Guttman's Lambda 6 reliability, 3) "average_r" = mean
#'   correlation between the variables/items, 4) "median_r" = median correlation
#'   between the variables/items, 5) "mean" = mean of the the scores from
#'   averaging the variables/items together, 6) "sd" = standard deviation of the
#'   scores from averaging the variables/items together, 7) "nvrb" = number of
#'   variables/items. The default is "average_r" and "nvrb".
#'
#' @param R integer vector of length 1 specifying the number of bootstrapped
#'   resamples to do. Only used when \code{ci.type} = "boot".
#'
#' @param boot.ci.type character vector of length 1 specifying the type of
#'   bootstrapped confidence interval to compute. The options are 1) "perc" for
#'   the regular percentile method, 2) "bca" for bias-corrected and accelerated
#'   percentile method, 3) "norm" for the normal method that uses the
#'   bootstrapped standard error to construct symmetrical confidence intervals
#'   with the classic formula around the bias-corrected estimate, and 4) "basic"
#'   for the basic method. Note, "stud" for the studentized method is NOT an
#'   option. See \code{\link[boot]{boot.ci}} as well as
#'   \code{\link{confint2.boot}} for details.
#'
#' @return data.frame containing the following columns:
#'
#' \describe{
#'    \item{est}{Cronbach's alpha itself}
#'    \item{se}{standard error for Cronbach's alpha}
#'    \item{lwr}{lower bound of the confidence interval of Cronbach's alpha}
#'    \item{upr}{upper bound for the confidence interval of Cronbach's alpha},
#'    \item{???}{any statistics requested via the \code{stats} argument}
#' }
#'
#' @seealso
#'    \code{\link{cronbach}}
#'    \code{\link{composites}}
#'
#' @references
#'
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for
#' coefficient alpha. Applied Psychological Measurement (11) 93-103.
#'
#' Duhachek, A. and Iacobucci, D. (2004). Alpha's standard error (ase): An accurate
#' and precise confidence interval estimate. Journal of Applied Psychology, 89(5):792-808.
#'
#' @examples
#'
#' dat0 <- psych::bfi
#' dat1 <- str2str::pick(x = dat0, val = c("A1","C4","C5","E1","E2","O2","O5",
#'    "gender","education","age"), not = TRUE, nm = TRUE)
#' vrb_nm_list <- lapply(X = str2str::sn(c("E","N","C","A","O")), FUN = function(nm) {
#'    str2str::pick(x = names(dat1), val = nm, pat = TRUE)})
#' cronbachs(data = dat1, vrb.nm.list = vrb_nm_list, ci.type = "classic")
#' cronbachs(data = dat1, vrb.nm.list = vrb_nm_list, ci.type = "delta")
#' cronbachs(data = dat1, vrb.nm.list = vrb_nm_list, ci.type = "boot")
#' suppressMessages(cronbachs(data = attitude, vrb.nm.list =
#'    list(names(attitude)))) # also works with only one set of variables/items
#'
#' @export
cronbachs <- function(data, vrb.nm.list, ci.type = "delta", level = 0.95,
   use = "pairwise.complete.obs", stats = c("average_r","nvrb"), R = 200L,
   boot.ci.type = "perc") {

   if (ci.type != "boot") {
      cronbach_obj <- lapply(X = vrb.nm.list, FUN = function(vrb.nm) {
         cronbach(data = data, vrb.nm = vrb.nm, ci.type = ci.type, level = level,
            use = use, stats = stats)
      })
      rtn <- str2str::lv2d(cronbach_obj, along = 1, fill = TRUE) # fill = TRUE due to str2str bug
   }
   if (ci.type == "boot") {
      boot_obj <- boot::boot(data = data, statistic = .cronbachs, R = R, stype = "i",
         nm.list = vrb.nm.list, use = use) # this is how you specify the `use` argument in .cronbach()
      alpha_all <- confint2.boot(boot_obj, boot.ci.type = boot.ci.type, level = level)
      cronbach_obj <- lapply(X = vrb.nm.list, FUN = function(vrb.nm) {
         cronbach(data = data, vrb.nm = vrb.nm, ci.type = "classic", level = level,
            use = use, stats = stats)
      })
      tmp <- str2str::lv2d(cronbach_obj, along = 1, fill = TRUE) # fill = TRUE due to str2str bug
      stats_user <- tmp[stats]
      rtn <- cbind(alpha_all, stats_user) # cbind.data.frame
   }
   return(rtn)
}

# .gtheory #

#' Bootstrap Function for \code{gtheory()} Function
#'
#' \code{.gtheory} is the function used by the \code{\link[boot]{boot}} function
#' within the \code{\link{gtheory}} function. It is primarily created to
#' increase the computational efficiency of bootstrap confidence intervals
#' within the \code{gtheory} function by doing only the minimal computations
#' needed to compute the generalizability theory coefficient.
#'
#' @param dat data.frame with only the variables/items you wish to include in
#'   the generalizability theory coefficient and no other variables/items.
#'
#' @param i integer vector of length = \code{nrow(dat)} specifying which rows
#'   should be included in the computation. When used by the \code{boot::boot}
#'   function this argument will change with every new bootstrapped resample.
#'
#' @param cross.vrb logical vector of length 1 specifying whether the
#'   variables/items should be crossed when computing the generalizability
#'   theory coefficient. If TRUE, then only the covariance structure of the
#'   variables/items will be incorperated into the estimate of reliability. If
#'   FALSE, then the mean structure of the variables/items will be incorperated.
#'
#' @return double vector of length 1 providing the generalizability theory
#'   coefficient.
#'
#' @seealso
#'    \code{\link{.gtheorys}}
#'    \code{\link{gtheory}}
#'
#' @examples
#' .gtheory(dat = attitude,
#'    i = sample(x = 1:nrow(attitude), size = nrow(attitude), replace = TRUE),
#'    cross.vrb = TRUE)
#' .gtheory(dat = attitude,
#'    i = sample(x = 1:nrow(attitude), size = nrow(attitude), replace = TRUE),
#'    cross.vrb = FALSE)
#' @export
.gtheory <- function(dat, i, cross.vrb) {
   dat_boot <- dat[i, ]
   icc_obj <- psych::ICC(x = dat_boot, missing = FALSE, lmer = TRUE,
      check.keys = FALSE) # just use psych::ICC() function for now and change for computational efficiency later
   icc_coef <- icc_obj[["results"]] # this list element is a data.frame
   if (cross.vrb) type <- "ICC3k" else type <- "ICC2k"
   rtn <- icc_coef[icc_coef$"type" == type, "ICC"]
   return(rtn)
}

# .gtheorys #

#' Bootstrap Function for \code{gtheorys()} Function
#'
#' \code{.gtheorys} is the function used by the \code{\link[boot]{boot}}
#' function within the \code{\link{gtheorys}} function. It is primarily created
#' to increase the computational efficiency of bootstrap confidence intervals
#' within the \code{gtheorys} function by doing only the minimal computations
#' needed to compute the generalizability theory coefficient.
#'
#' @param dat data.frame of data. It can contain variables other than those used
#'   for generalizability theory coefficient calculation.
#'
#' @param i integer vector of length = \code{nrow(dat)} specifying which rows
#'   should be included in the computation. When used by the \code{boot::boot}
#'   function this argument will change with every new bootstrapped resample.
#'
#' @param nm.list list of character vectors specifying the sets of
#'   variables/items associated with each of the generalizability theory
#'   coefficient calculations.
#'
#' @param cross.vrb logical vector of length 1 specifying whether the
#'   variables/items should be crossed when computing the generalizability
#'   theory coefficient. If TRUE, then only the covariance structure of the
#'   variables/items will be incorperated into the estimate of reliability. If
#'   FALSE, then the mean structure of the variables/items will be incorperated.
#'
#' @return double vector of length = \code{length(nm.list)} providing the
#'   generalizability theory coefficients.
#'
#' @seealso
#'    \code{\link{.gtheory}}
#'    \code{\link{gtheorys}}
#'
#' @examples
#' dat0 <- psych::bfi[1:250, ]
#' dat1 <- str2str::pick(x = dat0, val = c("A1","C4","C5","E1","E2","O2","O5",
#'    "gender","education","age"), not = TRUE, nm = TRUE)
#' vrb_nm_list <- lapply(X = str2str::sn(c("E","N","C","A","O")), FUN = function(nm) {
#'    str2str::pick(x = names(dat1), val = nm, pat = TRUE)})
#' .gtheorys(dat = dat1,
#'    i = sample(x = 1:nrow(dat1), size = nrow(dat1), replace = TRUE),
#'    nm.list = vrb_nm_list, cross.vrb = TRUE)
#' .gtheorys(dat = dat1,
#'    i = sample(x = 1:nrow(dat1), size = nrow(dat1), replace = TRUE),
#'    nm.list = vrb_nm_list, cross.vrb = FALSE)
#' @export
.gtheorys <- function(dat, i, nm.list, cross.vrb) {

   tmp <- lapply(X = nm.list, FUN = function(nm) {
      dat_boot <- dat[i, nm]
      icc_obj <- psych::ICC(x = dat_boot, missing = FALSE, lmer = TRUE,
         check.keys = FALSE) # just use psych::ICC() function for now and change for computational efficiency later
      icc_coef <- icc_obj[["results"]] # this list element is a data.frame
      if (cross.vrb) type <- "ICC3k" else type <- "ICC2k"
      rtn <- icc_coef[icc_coef$"type" == type, "ICC"]
      return(rtn)
   })
   unlist(tmp)
}

# gtheory #

#' Generalizability Theory Reliability of a Score
#'
#' \code{gtheory} uses generalizability theory to compute the reliability
#' coefficient of a score. It assumes single-level data where the rows are cases
#' and the columns are variables/items. Generaliability theory coefficients in
#' this case are the same as intraclass correlations (ICC). The default computes
#' ICC(3,k), which is identical to cronbach's alpha, from \code{cross.vrb} =
#' TRUE. When \code{cross.vrb} is FALSE, ICC(2,k) is computed, which takes mean
#' differences between variables/items into account. \code{gtheory} is a wrapper
#' function for \code{\link[psych]{ICC}}.
#'
#' When \code{ci.type} = "classic" the confidence intervals are computed
#' according to the formulas laid out by McGraw, Kenneth, and Wong, (1996).
#' These are taken from the \code{\link[psych]{ICC}} function in the
#' \code{psych} package. They are appropriately non-symmetrical given ICCs are
#' not unbounded and range from 0 to 1. Therefore, there is no standard error
#' associated with the coefficient. Note, they differ from the confidence
#' intervals available in the \code{\link{cronbach}} function. When
#' \code{ci.type} = "boot" the standard deviation of the empirical sampling
#' distribution is returned as the standard error, which may or may not be
#' trustworthy depending on the value of the ICC and sample size.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables/items.
#'
#' @param ci.type character vector of length = 1 specifying the type of
#'   confidence interval to compute. There are currently two options: 1)
#'   "classic" = traditional ICC-based confidence intervals (see details), 2)
#'   "boot" = bootstrapped confidence intervals.
#'
#' @param level double vector of length 1 specifying the confidence level from 0
#'   to 1.
#'
#' @param cross.vrb logical vector of length 1 specifying whether the
#'   variables/items should be crossed when computing the generalizability
#'   theory coefficient. If TRUE, then only the covariance structure of the
#'   variables/items will be incorperated into the estimate of reliability. If
#'   FALSE, then the mean structure of the variables/items will be incorperated.
#'
#' @param R integer vector of length 1 specifying the number of bootstrapped
#'   resamples to use. Only used if \code{ci.type} = "boot".
#'
#' @param boot.ci.type character vector of length 1 specifying the type of
#'   bootstrapped confidence interval to compute. The options are 1) "perc" for
#'   the regular percentile method, 2) "bca" for bias-corrected and accelerated
#'   percentile method, 3) "norm" for the normal method that uses the
#'   bootstrapped standard error to construct symmetrical confidence intervals
#'   with the classic formula around the bias-corrected estimate, and 4) "basic"
#'   for the basic method. Note, "stud" for the studentized method is NOT an
#'   option. See \code{\link[boot]{boot.ci}} as well as
#'   \code{\link{confint2.boot}} for details.
#'
#' @return double vector containing the generalizability theory coefficient,
#'   it's standard error (if \code{ci.type} = "boot"), and it's confidence
#'   interval.
#'
#' @seealso
#'    \code{\link{gtheorys}}
#'    \code{\link{gtheory_ml}}
#'    \code{\link{cronbach}}
#'
#' @references
#'
#' McGraw, Kenneth O. and Wong, S. P. (1996), Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1, 30-46. + errata on page 390.
#'
#' @examples
#'
#' gtheory(attitude, vrb.nm = names(attitude), ci.type = "classic")
#' \dontrun{
#' gtheory(attitude, vrb.nm = names(attitude), ci.type = "boot")
#' gtheory(attitude, vrb.nm = names(attitude), ci.type = "boot",
#'    R = 250L, boot.ci.type = "bca")
#' }
#'
#' # comparison to cronbach's alpha:
#' gtheory(attitude, names(attitude))
#' gtheory(attitude, names(attitude), cross.vrb = FALSE)
#' a <- suppressMessages(psych::alpha(attitude)[["total"]]["raw_alpha"])
#' psych::alpha.ci(a, n.obs = 30, n.var = 7, digits = 7) # slightly different confidence interval
#'
#' @export
gtheory <- function(data, vrb.nm, ci.type = "classic", level = 0.95, cross.vrb = TRUE,
   R = 200L, boot.ci.type = "perc") {

   lme4_pkg <- requireNamespace("lme4", quietly = TRUE)
   if (!lme4_pkg) stop("The `lme4` package is required to use this function, but is not installed on this device")
   data_vrb <- data[vrb.nm]

   if (ci.type == "classic") {
      icc_obj <- psych::ICC(x = data_vrb, missing = FALSE, alpha = 1 - level,
         lmer = TRUE, check.keys = FALSE)
      icc_coef <- icc_obj[["results"]] # this list element is a data.frame
      if (cross.vrb) type <- "ICC3k" else type <- "ICC2k"
      tmp <- icc_coef[icc_coef$"type" == type, c("ICC","lower bound", "upper bound")]
      rtn <- str2str::d2v(tmp)
      names(rtn) <- c("est","lwr","upr")
      str2str::append(rtn, after = 1, nm = "se") <- NA_real_
   }
   if (ci.type == "boot") {
      boot_obj <- boot::boot(data = data_vrb, statistic = .gtheory, R = R, stype = "i",
         cross.vrb = cross.vrb) # this is how you specify the `use` argument in .cronbach()
      tmp <- confint2.boot(boot_obj, boot.ci.type = boot.ci.type, level = level)
      rtn <- str2str::d2v(tmp)
   }
   return(rtn)
}

# gtheorys #

#' Generalizability Theory Reliability of Multiple Scores
#'
#' \code{gtheorys} uses generalizability theory to compute the reliability
#' coefficient of multiple scores. It assumes single-level data where the rows
#' are cases and the columns are variables/items. Generaliability theory
#' coefficients in this case are the same as intraclass correlations (ICC). The
#' default computes ICC(3,k), which is identical to cronbach's alpha, from
#' \code{cross.vrb} = TRUE. When \code{cross.vrb} is FALSE, ICC(2,k) is
#' computed, which takes mean differences between variables/items into account.
#' \code{gtheorys} is a wrapper function for \code{\link[psych]{ICC}}.
#'
#' When \code{ci.type} = "classic" the confidence intervals are computed
#' according to the formulas laid out by McGraw, Kenneth and Wong (1996). These
#' are taken from the \code{\link[psych]{ICC}} function in the \code{psych}
#' package. They are appropriately non-symmetrical given ICCs are not unbounded
#' and range from 0 to 1. Therefore, there is no standard error associated with
#' the coefficient. Note, they differ from the confidence intervals available in
#' the \code{\link{cronbachs}} function. When \code{ci.type} = "boot" the
#' standard deviation of the empirical sampling distribution is returned as the
#' standard error, which may or may not be trustworthy depending on the value of
#' the ICC and sample size.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm.list list of character vectors containing colnames from
#'   \code{data} specifying each set of variables/items.
#'
#' @param ci.type character vector of length = 1 specifying the type of
#'   confidence interval to compute. There are currently two options: 1)
#'   "classic" = traditional ICC-based confidence intervals (see details), 2)
#'   "boot" = bootstrapped confidence intervals.
#'
#' @param level double vector of length 1 specifying the confidence level from 0
#'   to 1.
#'
#' @param cross.vrb logical vector of length 1 specifying whether the
#'   variables/items should be crossed when computing the generalizability
#'   theory coefficients. If TRUE, then only the covariance structure of the
#'   variables/items will be incorperated into the estimates of reliability. If
#'   FALSE, then the mean structure of the variables/items will be incorperated.
#'
#' @param R integer vector of length 1 specifying the number of bootstrapped
#'   resamples to use. Only used if \code{ci.type} = "boot".
#'
#' @param boot.ci.type character vector of length 1 specifying the type of
#'   bootstrapped confidence interval to compute. The options are 1) "perc" for
#'   the regular percentile method, 2) "bca" for bias-corrected and accelerated
#'   percentile method, 3) "norm" for the normal method that uses the
#'   bootstrapped standard error to construct symmetrical confidence intervals
#'   with the classic formula around the bias-corrected estimate, and 4) "basic"
#'   for the basic method. Note, "stud" for the studentized method is NOT an
#'   option. See \code{\link[boot]{boot.ci}} as well as
#'   \code{\link{confint2.boot}} for details.
#'
#' @return data.frame containing the generalizability theory statistical information.
#' The columns are as follows:
#'
#' \describe{
#'    \item{est}{the generalizability theory coefficient itself}
#'    \item{se}{standard error of the reliability coefficient}
#'    \item{lwr}{lower bound of the confidence interval for the reliability coefficient}
#'    \item{lwr}{lower bound of the confidence interval for the reliability coefficient}
#' }
#'
#' @seealso
#'    \code{\link{gtheory}}
#'    \code{\link{gtheorys_ml}}
#'    \code{\link{cronbachs}}
#'
#' @references
#'
#' McGraw, Kenneth O. and Wong, S. P. (1996), Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1, 30-46. + errata on page 390.
#'
#' @examples
#'
#' dat0 <- psych::bfi[1:100, ] # reduce number of rows
#'    # to reduce computational time of boot examples
#' dat1 <- str2str::pick(x = dat0, val = c("A1","C4","C5","E1","E2","O2","O5",
#'    "gender","education","age"), not = TRUE, nm = TRUE)
#' vrb_nm_list <- lapply(X = str2str::sn(c("E","N","C","A","O")), FUN = function(nm) {
#'    str2str::pick(x = names(dat1), val = nm, pat = TRUE)})
#' gtheorys(data = dat1, vrb.nm.list = vrb_nm_list)
#' \dontrun{
#' gtheorys(data = dat1, vrb.nm.list = vrb_nm_list, ci.type = "boot") # singular messages
#' gtheorys(data = dat1, vrb.nm.list = vrb_nm_list, ci.type = "boot",
#'    R = 250L, boot.ci.type = "bca")
#' }
#' gtheorys(data = attitude,
#'    vrb.nm.list = list(names(attitude))) # also works with only one set of variables/items
#'
#' @export
gtheorys <- function(data, vrb.nm.list, ci.type = "classic", level = 0.95, cross.vrb = TRUE,
   R = 200L, boot.ci.type = "perc") {

   lme4_pkg <- requireNamespace("lme4", quietly = TRUE)
   if (!lme4_pkg) stop("The `lme4` package is required to use this function, but is not installed on this device")

   if (ci.type == "classic") {
      tmp <- lapply(X = vrb.nm.list, FUN = function(vrb.nm) {
         gtheory(data = data, vrb.nm = vrb.nm, ci.type = ci.type, level = level,
            cross.vrb = cross.vrb, R = R, boot.ci.type = boot.ci.type)
      })
      rtn <- str2str::lv2d(tmp, along = 1, fill = TRUE)
   }
   if (ci.type == "boot") {
      boot_obj <- boot::boot(data = data, statistic = .gtheorys, R = R, stype = "i",
         nm.list = vrb.nm.list, cross.vrb = cross.vrb) # this is how you specify the `cross.vrb` argument in .gtheorys()
      rtn <- confint2.boot(boot_obj, boot.ci.type = boot.ci.type, level = level)
   }
   return(rtn)
}

# composite #

#' Composite Reliability of a Score
#'
#' \code{composite} computes the composite reliability coefficient (sometimes
#' referred to as omega) for a set of variables/items. The composite reliability
#' computed in \code{composite} assumes a undimensional factor model with no
#' error covariances. In addition to the coefficient itself, its standard error
#' and confidence interval are returned, the average standardized factor loading
#' from the factor model and number of variables/items, and (optional) model fit
#' indices of the factor model. Note, any reverse coded items need to be recoded
#' ahead of time so that all variables/items are keyed in the same direction.
#'
#' The factor model is estimated using the R package \code{lavaan}. The
#' reliability coefficients are calculated based on the square of the sum of the
#' factor loadings divided by the sum of the square of the sum of the factors
#' loadings and the sum of the error variances (Raykov, 2001).
#'
#' \code{composite} is only able to use the "ML" estimator at the moment and
#' cannot model items as categorical/ordinal. However, different versions of
#' standard errors and test statistics are possible. For example, the "MLM"
#' estimator can be specified by \code{se} = "robust.sem" and \code{test} =
#' "satorra.bentler"; the "MLR" estimator can be specified by \code{se} =
#' "robust.huber.white" and \code{test} = "yuan.bentler.mplus". See
#' \code{\link[lavaan]{lavOptions}} and scroll down to Estimation options.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames in \code{data} specifying the set
#'   of variables/items.
#'
#' @param std logical element of length 1 specifying if the composite
#'   reliability should be computed for the standardized version of the
#'   variables \code{data[vrb.nm]}.
#'
#' @param level double vector of length 1 with a value between 0 and 1
#'   specifying what confidence level to use.
#'
#' @param ci.type character vector of length 1 specifying which type of
#'   confidence interval to compute. The "delta" option uses the delta method to
#'   compute a standard error and a symmetrical confidence interval. The "boot"
#'   option uses bootstrapping to compute an asymmetrical confidence interval as
#'   well as a (pseudo) standard error.
#'
#' @param boot.ci.type character vector of length 1 specifying which type of
#'   bootstrapped confidence interval to compute. The options are: 1) "norm", 2)
#'   "basic", 3) "perc", 4) "bca.simple". Only used if \code{ci.type} = "boot".
#'   See \code{\link[lavaan]{parameterEstimates}} and its \code{boot.ci.type}
#'   argument for details.
#'
#' @param R integer vector of length 1 specifying how many bootstrapped
#'   resamples to compute. Note, as the number of bootstrapped resamples
#'   increases, the computation time will increase. Only used if \code{ci.type}
#'   is "boot".
#'
#' @param fit.measures character vector specifying which model fit indices to
#'   include in the return object. The default option includes the chi-square
#'   test statistic ("chisq"), degrees of freedom ("df"), tucker-lewis index
#'   ("tli"), comparative fit index ("cfi"), root mean square error of
#'   approximation ("rmsea"), and standardized root mean residual ("srmr"). If
#'   NULL, then no model fit indices are included in the return object. See
#'   \code{\link[lavaan]{fitMeasures}} for details.
#'
#' @param se character vector of length 1 specifying which type of standard
#'   errors to compute. If ci.type = "boot", then the input value is ignored and
#'   set to "bootstrap". See \code{\link[lavaan]{lavOptions}} and its \code{se}
#'   argument for details.
#'
#' @param test character vector of length 1 specifying which type of test
#'   statistic to compute. If ci.type = "boot", then the input value is ignored
#'   and set to "bootstrap". See \code{\link[lavaan]{lavOptions}} and its
#'   \code{test} argument for details.
#'
#' @param missing character vector of length 1 specifying how to handle missing
#'   data. The default is "fiml" for full information maximum likelihood). See
#'   \code{\link[lavaan]{lavOptions}} and its \code{missing} argument for
#'   details.
#'
#' @param ... other arguments passed to \code{\link[lavaan]{cfa}}. Use at your
#'   own peril as some argument values could cause the function to break.
#'
#' @return double vector where the first element is the composite reliability
#'   coefficient ("est") followed by its standard error ("se"), then its
#'   confidence interval ("lwr" and "upr"), the average standardized factor
#'   loading of the factor model ("average_l") and number of variables ("nvrb"),
#'   and finally any of the \code{fit.measures} requested.
#'
#' @seealso
#'    \code{\link{composites}}
#'    \code{\link{cronbach}}
#'
#' @references
#'
#' Raykov, T. (2001). Estimation of congeneric scale reliability using covariance
#' structure analysis with nonlinear constraints. British Journal of Mathematical
#' and Statistical Psychology, 54(2), 315–323.
#'
#' @examples
#'
#' # data
#' dat <- psych::bfi[1:250, 2:5] # the first item is reverse coded
#'
#' # delta method CI
#' composite(data = dat, vrb.nm = names(dat), ci.type = "delta")
#' composite(data = dat, vrb.nm = names(dat), ci.type = "delta", level = 0.99)
#' composite(data = dat, vrb.nm = names(dat), ci.type = "delta", std = TRUE)
#' composite(data = dat, vrb.nm = names(dat), ci.type = "delta", fit.measures = NULL)
#' composite(data = dat, vrb.nm = names(dat), ci.type = "delta",
#'    se = "robust.sem", test = "satorra.bentler", missing = "listwise") # MLM estimator
#' composite(data = dat, vrb.nm = names(dat), ci.type = "delta",
#'    se = "robust.huber.white", test = "yuan.bentler.mplus", missing = "fiml") # MLR estimator
#'
#' \dontrun{
#' # bootstrapped CI
#' composite(data = dat, vrb.nm = names(dat), level = 0.95,
#'    ci.type = "boot") # slightly different estimate for some reason...
#' composite(data = dat, vrb.nm = names(dat), level = 0.95, ci.type = "boot",
#'    boot.ci.type = "perc", R = 250L) # probably want to use more resamples - this is just an example
#' }
#'
#' # compare to semTools::reliability
#' psymet_obj <- composite(data = dat, vrb.nm = names(dat))
#' psymet_est <- unname(psymet_obj["est"])
#' lavaan_obj <- lavaan::cfa(model = make.latent(names(dat)), data = dat,
#'    std.lv = TRUE, missing = "fiml")
#' semTools_obj <- semTools::reliability(lavaan_obj)
#' semTools_est <- semTools_obj["omega", "latent"]
#' all.equal(psymet_est, semTools_est)
#'
#' @export
composite <- function(data, vrb.nm, level = 0.95, std = FALSE,
   ci.type = "delta", boot.ci.type = "bca.simple", R = 200L,
   fit.measures = c("chisq","df","tli","cfi","rmsea","srmr"),
   se = "standard", test = "standard", missing = "fiml", ...) { # can't figure out how to use switch or multiple if statements in default arguments so that "bootstrap" is default for `ci.type` = "boot"

   # model syntax
   load_labels <- paste0(vrb.nm, "_l") # didn't use make.latent() because needed to add in the user-defined parameter
   load_syntax <- paste0("latent", " =~ ", paste0(load_labels, "*", vrb.nm, collapse = " + "))
   error_labels <- paste0(vrb.nm, "_e")
   error_syntax <- paste0(vrb.nm, " ~~ ", error_labels, "*", vrb.nm, collapse = " \n ")
   def_load <- paste0("(", paste0(load_labels, collapse = " + "), ") ^ 2") # square the sum of the loadings
   def_error <- paste0("(", paste0(error_labels, collapse = " + "), ")") # sum up error variances (and 2 times error covariances)
   def_syntax <- paste0(".composite. := ", def_load, " / ", "(", paste(def_load, def_error, sep = " + "), ")")
   model_syntax <- paste(load_syntax, error_syntax, def_syntax, sep = " \n ")

   # estimation
   if (!std) data_vrb <- data[vrb.nm]
   if (std) data_vrb <- as.data.frame(scale(data[vrb.nm]))
   if (ci.type == "delta") {
      lavaan_obj <- lavaan::cfa(model = model_syntax, data = data_vrb, std.lv = TRUE, # the factor must be standardized to calculate composite reliability
         estimator = "ML", se = se, test = test, missing = missing, ...)
   }
   if (ci.type == "boot") {
      lavaan_obj <- lavaan::cfa(model = model_syntax, data = data_vrb, std.lv = TRUE, # the factor must be standardized to calculate composite reliability
         estimator = "ML", se = "bootstrap", test = "bootstrap", bootstrap = R, ...)
   }

   # reliability
   parEst_obj <- lavaan::parameterEstimates(lavaan_obj, level = level, ci = TRUE,
      boot.ci.type = boot.ci.type, standardized = TRUE, output = "data.frame")
   def_row <- parEst_obj[parEst_obj$"op" == ":=", ]
   est <- str2str::d2v(def_row[, c("est","se","ci.lower","ci.upper")])
   sub_set <- is.element(el = names(est), set = c("ci.lower","ci.upper"))
   names(est)[sub_set] <- c("lwr","upr")

   # standardized factor loadings
   load_row <- parEst_obj[parEst_obj$"op" == "=~", ]
   load_avg <- mean(load_row[["std.all"]])

   # model fit
   modFit_obj <- lavaan::fitMeasures(lavaan_obj, output = "vector")
   mfi <- modFit_obj[fit.measures] # returns character(0) if fit.measures = NULL

   # return object
   rtn <- c(est, "average_l" = load_avg, "nvrb" = length(vrb.nm), mfi)
   return(rtn)
}

# composites #

#' Composite Reliability of Multiple Scores
#'
#' \code{composites} computes the composite reliability coefficient (sometimes
#' referred to as omega) for multiple sets of variables/items. The composite
#' reliability computed in \code{composites} assumes a undimensional factor
#' model for each set of variables/items with no error covariances. In addition
#' to the coefficients themselves, their standard errors and confidence
#' intervals are returned, the average standardized factor loading from the
#' factor models and number of variables/items in each set, and (optional) model
#' fit indices of the factor models. Note, any reverse coded items need to be
#' recoded ahead of time so that all items are keyed in the same direction for
#' each set of variables/items.
#'
#' The factor models are estimated using the R package \code{lavaan}. The
#' reliability coefficients are calculated based on the square of the sum of the
#' factor loadings divided by the sum of the square of the sum of the factors
#' loadings and the sum of the error variances (Raykov, 2001).
#'
#' \code{composites} is only able to use the "ML" estimator at the moment and
#' cannot model items as categorical/ordinal. However, different versions of
#' standard errors and test statistics are possible. For example, the "MLM"
#' estimator can be specified by \code{se} = "robust.sem" and \code{test} =
#' "satorra.bentler"; the "MLR" estimator can be specified by \code{se} =
#' "robust.huber.white" and \code{test} = "yuan.bentler.mplus". See
#' \code{\link[lavaan]{lavOptions}} and scroll down to Estimation options for
#' details.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm.list list of character vectors containing colnames in
#'   \code{data} specifying the multiple sets of variables/items.
#'
#' @param std logical element of length 1 specifying if the composite
#'   reliability should be computed for the standardized version of the
#'   variables/items \code{data[unlist(vrb.nm.list)]}.
#'
#' @param level double vector of length 1 with a value between 0 and 1
#'   specifying what confidence level to use.
#'
#' @param ci.type character vector of length 1 specifying which type of
#'   confidence interval to compute. The "delta" option uses the delta method to
#'   compute a standard error and a symmetrical confidence interval. The "boot"
#'   option uses bootstrapping to compute an asymmetrical confidence interval as
#'   well as a (pseudo) standard error.
#'
#' @param boot.ci.type character vector of length 1 specifying which type of
#'   bootstrapped confidence interval to compute. The options are: 1) "norm", 2)
#'   "basic", 3) "perc", 4) "bca.simple". Only used if \code{ci.type} = "boot".
#'   See \code{\link[lavaan]{parameterEstimates}} and its \code{boot.ci.type}
#'   argument for details.
#'
#' @param R integer vector of length 1 specifying how many bootstrapped
#'   resamples to compute. Note, as the number of bootstrapped resamples
#'   increases, the computation time will increase. Only used if \code{ci.type}
#'   is "boot".
#'
#' @param fit.measures character vector specifying which model fit indices to
#'   include in the return object. The default option includes the chi-square
#'   test statistic ("chisq"), degrees of freedom ("df"), tucker-lewis index
#'   ("tli"), comparative fit index ("cfi"), root mean square error of
#'   approximation ("rmsea"), and standardized root mean residual ("srmr"). If
#'   NULL, then no model fit indices are included in the return object. See
#'   \code{\link[lavaan]{fitMeasures}} for details.
#'
#' @param se character vector of length 1 specifying which type of standard
#'   errors to compute. If ci.type = "boot", then the input value is ignored and
#'   implicitly set to "bootstrap". See \code{\link[lavaan]{lavOptions}} and its
#'   \code{se} argument for details.
#'
#' @param test character vector of length 1 specifying which type of test
#'   statistic to compute. If ci.type = "boot", then the input value is ignored
#'   and implicitly set to "bootstrap". See \code{\link[lavaan]{lavOptions}} and
#'   its \code{test} argument for details.
#'
#' @param missing character vector of length 1 specifying how to handle missing
#'   data. The default is "fiml" for full information maximum likelihood. See
#'   \code{\link[lavaan]{lavOptions}} and its \code{missing} argument for
#'   details.
#'
#' @param ... other arguments passed to \code{\link[lavaan]{cfa}}. Use at your
#'   own peril as some argument values could cause the function to break.
#'
#' @return data.frame containing the composite reliability of each set of variables/items.
#'
#' \describe{
#'    \item{est}{estimate of the reliability coefficient}
#'    \item{se}{standard error of the reliability coefficient}
#'    \item{lwr}{lower bound of the confidence interval of the reliability coefficient}
#'    \item{upr}{upper bound of the confidence interval of the reliability coefficient}
#'    \item{average_l}{average standardized factor loading from the factor model}
#'    \item{nvrb}{number of variables/items}
#'    \item{???}{any model fit indices requested by the \code{fit.measures} argument}
#' }
#'
#' @seealso
#'    \code{\link{composite}}
#'    \code{\link{cronbachs}}
#'
#' @references
#'
#' Raykov, T. (2001). Estimation of congeneric scale reliability using covariance
#' structure analysis with nonlinear constraints. British Journal of Mathematical
#' and Statistical Psychology, 54(2), 315–323.
#'
#' @examples
#'
#' dat0 <- psych::bfi[1:250, ]
#' dat1 <- str2str::pick(x = dat0, val = c("A1","C4","C5","E1","E2","O2","O5",
#'    "gender","education","age"), not = TRUE, nm = TRUE)
#' vrb_nm_list <- lapply(X = str2str::sn(c("E","N","C","A","O")), FUN = function(nm) {
#'    str2str::pick(x = names(dat1), val = nm, pat = TRUE)})
#' composites(data = dat1, vrb.nm.list = vrb_nm_list)
#' \dontrun{
#' start_time <- Sys.time()
#' composites(data = dat1, vrb.nm.list = vrb_nm_list, ci.type = "boot",
#'    R = 5000L) # the function is not optimized for speed at the moment
#'    # since it will bootstrap separately for each set of variables/items
#' end_time <- Sys.time()
#' print(end_time - start_time) # takes 10 minutes on my laptop
#' }
#' composites(data = attitude,
#'    vrb.nm.list = list(names(attitude))) # also works with only one set of variables/items
#'
#' @export
composites <- function(data, vrb.nm.list, level = 0.95, std = FALSE,
   ci.type = "delta", boot.ci.type = "bca.simple", R = 200L,
   fit.measures = c("chisq","df","tli","cfi","rmsea","srmr"),
   se = "standard", test = "standard", missing = "fiml", ...) { # can't figure out how to use switch or multiple if statements in default arguments so that "bootstrap" is default for `ci.type` = "boot"

   tmp <- lapply(X = vrb.nm.list, FUN = function(vrb.nm) {
      composite(data = data, vrb.nm = vrb.nm, level = level, std = std,
         ci.type = ci.type, boot.ci.type = boot.ci.type, R = R,
         fit.measures = fit.measures,
         se = se, test = test, missing = missing, ...)

   })
   rtn <- str2str::lv2d(tmp, along = 1, fill = TRUE) # need fill = TRUE due to a str2str bug
   return(rtn)
}

# MULTILEVEL ####

# gtheory_ml #

#' Generalizability Theory Reliability of a Multilevel Score
#'
#' \code{gtheory_ml} uses generalizability theory to compute the reliability
#' coefficients of a multilevel score. It computes a within-group coefficient
#' that assesses the reliability of the group-deviated score (e.g., after
#' calling \code{\link{center_by}}) and a between-group coefficient that assess
#' the reliability of the mean aggregate score (e.g., after calling
#' \code{\link{agg}}). It assumes two-level data where the rows are in long
#' format and the columns are the variables/items of the score. Generaliability
#' theory coefficients with multilevel data are analagous to intraclass
#' correlations (ICC), but add an additional grouping variable. The default
#' computes a multilevel version of ICC(3,k) from \code{cross.obs} = TRUE. When
#' \code{cross.obs} = FALSE, a multilevel version of ICC(2,k) is computed, which
#' takes mean differences between variables/items into account.
#' \code{gtheory_ml} is a wrapper function for \code{\link[psych]{mlr}}. Note,
#' this function can take several minutes to run if you have a moderate to large
#' dataset.
#'
#' \code{gtheory_ml} uses \code{\link[psych]{mlr}}, which is based on the
#' formulas in Shrout, Patrick, and Lane (2012). When \code{cross.obs} = TRUE,
#' the within-group coefficient is Rc and the between-group coefficient is RkF.
#' When \code{cross.obs} = FALSE, the within-group coefficient is Rcn and the
#' between-group coefficient is RkRn.
#'
#' \code{gtheory_ml} does not currently have standard errors or confidence
#' intervals. I am not aware of mathematical formulas for analytical confidence
#' intervals, and because the generaliability theory coefficients can take
#' several minutes to estimate, bootstraped confidence intervals seem too
#' time-intensive to be useful at the moment.
#'
#' \code{gtheory_ml} does not work with a single variable/item. You can still
#' use generalizability theory to estimate between-group reliability in that
#' instance though. To do so, reshape the variable/item from long to wide (e.g.,
#' \code{\link[str2str]{unstack2}}) so that you have a column for each
#' observation of that single variable/item and the rows are the groups. Then
#' you can use \code{gtheory} and treat each observation as a "different"
#' variable/item.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables/items.
#'
#' @param grp.nm character vector of length 1 with colname from \code{data}
#'   specifying the grouping variable. Because \code{gtheory_ml} is specific to
#'   two-level data, this can only be one variable.
#'
#' @param obs.nm character vector of of length 1 with colname from \code{data}
#'   specifying the observation variable. In this context, observation refers to
#'   comparable cases across groups. In a longitudinal study, the groups are
#'   people and the observations are timepoints. For example, each person has a
#'   timepoint 1, timepoint 2, timepoint 3, etc. In an school study, the groups
#'   are classrooms and the observations are students. For example, each
#'   classroom has a student 1, student 2, student 3, etc. While longitudinal
#'   studies often have a time variable in their data, school studies don't
#'   always have a student variable. You would then have to create a student
#'   variable to be able to use \code{gtheory_ml}.
#'
#' @param cross.obs logical vector of length 1 specifying whether the
#'   observations should be crossed when computing the generalizability theory
#'   coefficient. If TRUE, the observations are treated as fixed; if FALSE, they
#'   are treated as random. See details.
#'
#' @return list with two elements. The first is named "within" and refers to the
#'   within-group reliability. The second is named "between" and refers to the
#'   between-group reliability. Each contains a double vector where the first
#'   element is named "est" and contains the generalizability theory coefficient
#'   itself. The second element is named "average_r" and contains the average
#'   correlation at that level of the data based on \code{\link{cor_ml}} (which
#'   is a wrapper for \code{\link[psych]{statsBy}}). The third element is named
#'   "nvrb" and contains the number of variables/items. These later two elements
#'   are included because even though the reliability coefficients are
#'   calculated from variance components, they are indirectly based on the
#'   average correlation and number of variables/items, similar to Cronbach's
#'   alpha.
#'
#' @seealso
#'    \code{\link{gtheorys_ml}}
#'    \code{\link{gtheory}}
#'
#' @references
#'
#' Shrout, Patrick and Lane, Sean P (2012), Psychometrics. In M.R. Mehl and T.S.
#' Conner (eds) Handbook of research methods for studying daily life, (p 302-320)
#' New York. Guilford Press
#'
#' @examples
#'
#' shrout <- structure(list(Person = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L,
#'    5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L), Time = c(1L, 1L,
#'       1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L,
#'       4L, 4L), Item1 = c(2L, 3L, 6L, 3L, 7L, 3L, 5L, 6L, 3L, 8L, 4L,
#'          4L, 7L, 5L, 6L, 1L, 5L, 8L, 8L, 6L), Item2 = c(3L, 4L, 6L, 4L,
#'             8L, 3L, 7L, 7L, 5L, 8L, 2L, 6L, 8L, 6L, 7L, 3L, 9L, 9L, 7L, 8L
#'          ), Item3 = c(6L, 4L, 5L, 3L, 7L, 4L, 7L, 8L, 9L, 9L, 5L, 7L,
#'             9L, 7L, 8L, 4L, 7L, 9L, 9L, 6L)), .Names = c("Person", "Time",
#'                "Item1", "Item2", "Item3"), class = "data.frame", row.names = c(NA,
#'                   -20L))
#' mlr_obj <- psych::mlr(x = shrout, grp = "Person", Time = "Time",
#'    items = c("Item1", "Item2", "Item3"),
#'    alpha = FALSE, icc = FALSE, aov = FALSE, lmer = TRUE, lme = FALSE,
#'    long = FALSE, plot = FALSE)
#' gtheory_ml(data = shrout, vrb.nm = c("Item1", "Item2", "Item3"),
#'    grp.nm = "Person", obs.nm = "Time", cross.obs = TRUE) # crossed time
#' gtheory_ml(data = shrout, vrb.nm = c("Item1", "Item2", "Item3"),
#'    grp.nm = "Person", obs.nm = "Time", cross.obs = FALSE) # nested time
#'
#' @export
gtheory_ml <- function(data, vrb.nm, grp.nm, obs.nm, cross.obs = TRUE) {

   lme4_pkg <- requireNamespace("lme4", quietly = TRUE)
   if (!lme4_pkg) stop("The `lme4` package is required to use this function, but is not installed on this device")
   mlr_obj <- psych::mlr(x = data, grp = grp.nm, Time = obs.nm, items = vrb.nm,
      alpha = FALSE, icc = FALSE, aov = FALSE, lmer = TRUE, lme = FALSE,
      long = FALSE, plot = FALSE)
   if (cross.obs) {
      coef_wth <- mlr_obj[["Rc"]]
      coef_btw <- mlr_obj[["RkF"]]
   }
   if (!cross.obs) {
      coef_wth <- mlr_obj[["Rcn"]]
      coef_btw <- mlr_obj[["RkRn"]]
   }
   cor_list <- cor_ml(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm)
   cor_avg <- lapply(X = cor_list, FUN = function(mat) mean(mat[lower.tri(mat)]))
   nvrb <- length(vrb.nm)
   rtn <- list("within" = c("est" = coef_wth, "average_r" = cor_avg[["within"]], "nvrb" = nvrb),
      "between" = c("est" = coef_btw, "average_r" = cor_avg[["between"]], "nvrb" = nvrb))
   return(rtn)
}

# gtheorys_ml #

#' Generalizability Theory Reliability of Multiple Multilevel Scores
#'
#' \code{gtheorys_ml} uses generalizability theory to compute the reliability
#' coefficients of multiple multilevel score. It computes within-group
#' coefficients that assess the reliability of the group-deviated scores (e.g.,
#' after calling \code{\link{centers_by}}) and between-group coefficients that
#' assess the reliability of the mean aggregate scores (e.g., after calling
#' \code{\link{aggs}}). It assumes two-level data where the rows are in long
#' format and the columns are the variables/items of the score. Generaliability
#' theory coefficients with multilevel data are analagous to intraclass
#' correlations (ICC), but add an additional grouping variable. The default
#' computes a multilevel version of ICC(3,k) from \code{cross.obs} = TRUE. When
#' \code{cross.obs} = FALSE, a multilevel version of ICC(2,k) is computed, which
#' takes mean differences between variables/items into account.
#' \code{gtheorys_ml} is a wrapper function for \code{\link[psych]{mlr}}. Note,
#' this function can take several minutes to run if you have a moderate to large
#' dataset.
#'
#' \code{gtheorys_ml} uses \code{\link[psych]{mlr}}, which is based on the
#' formulas in Shrout, Patrick, and Lane (2012). When \code{cross.obs} = TRUE,
#' the within-group coefficient is Rc and the between-group coefficient is RkF.
#' When \code{cross.obs} = FALSE, the within-group coefficient is Rcn and the
#' between-group coefficient is RkRn.
#'
#' \code{gtheorys_ml} does not currently have standard errors or confidence
#' intervals. I am not aware of mathematical formulas for analytical confidence
#' intervals, and because the generaliability theory coefficients can take
#' several minutes to estimate, bootstraped confidence intervals seem too
#' time-intensive to be useful at the moment.
#'
#' \code{gtheorys_ml} does not work with multiple single variable/item scores.
#' You can still use generalizability theory to estimate between-group
#' reliability in that instance though. To do so, reshape the multiple single
#' variables/items from long to wide (e.g., \code{\link{long2wide}}) so that you
#' have a column for each observation of that single variable/item and the rows
#' are the groups. Then you can use \code{gtheorys} and treat each observation as
#' a "different" variable/item.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm.list list of character vectors of colnames from \code{data}
#' specifying the sets of variables/items.
#'
#' @param grp.nm character vector of length 1 with colname from \code{data}
#' specifying the grouping variable. Because \code{gtheorys_ml} is specific to
#' two-level data, this can only be one variable.
#'
#' @param obs.nm character vector of of length 1 with colname from \code{data}
#' specifying the observation variable. In this context, observation refers to
#' comparable cases across groups. In a longitudinal study, the groups are
#' people and the observations are timepoints. For example, each person has a
#' timepoint 1, timepoint 2, timepoint 3, etc. In an school study, the groups
#' are classrooms and the observations are students. For example, each
#' classroom has a student 1, student 2, student 3, etc. While longitudinal
#' studies often have a time variable in their data, school studies don't have
#' always a student variable. You would then have to create a student variable
#' to be able to use this function.
#'
#' @param cross.obs logical vector of length 1 specifying whether the
#' observations should be crossed when computing the generalizability theory
#' coefficients. If TRUE, the observations are treated as fixed; if FALSE, they
#' are treated as random. See details.
#'
#' @return list with two elements. The first is named "within" and refers to the
#' within-group reliability. The second is named "between" and refers to the
#' between-group reliability. Each contains a data.frame with the following columns:
#'
#' \describe{
#'    \item{est}{generalizability theory reliability coefficient itself}
#'    \item{average_r}{the average correlation at each level of the data based on
#'       \code{\link{cor_ml}} (which is a wrapper for \code{\link[psych]{statsBy}})}
#'    \item{nvrb}{number of variables/items that make up that score}
#' }
#'
#' The later two columns are included because even though the reliability coefficients
#' are calculated from variance components, they are indirectly based on the average
#' correlation and number of variables/items similar to Cronbach's alpha.
#'
#' @seealso
#'    \code{\link{gtheory_ml}}
#'    \code{\link{gtheorys}}
#'
#' @references
#'
#' Shrout, Patrick and Lane, Sean P (2012), Psychometrics. In M.R. Mehl and T.S.
#' Conner (eds) Handbook of research methods for studying daily life, (p 302-320)
#' New York. Guilford Press
#'
#' @examples
#'
#' dat <- psychTools::sai[psychTools::sai$"study" == "VALE", ] # 4 timepoints
#' vrb_nm_list <- list("positive_affect" = c("calm","secure","at.ease","rested",
#'    "comfortable","confident"), # extra: "relaxed","content","joyful"
#'    "negative_affect" = c("tense","regretful","upset","worrying","anxious",
#'       "nervous")) # extra: "jittery","high.strung","worried","rattled"
#' suppressMessages(gtheorys_ml(data = dat, vrb.nm.list = vrb_nm_list, grp.nm = "id",
#'    obs.nm = "time", cross.obs = TRUE))
#' suppressMessages(gtheorys_ml(data = dat, vrb.nm.list = vrb_nm_list, grp.nm = "id",
#'    obs.nm = "time", cross.obs = FALSE))
#' gtheorys_ml(data = dat, vrb.nm.list = vrb_nm_list["positive_affect"], grp.nm = "id",
#'    obs.nm = "time") # also works with only one set of variables/items
#'
#' @export
gtheorys_ml <- function(data, vrb.nm.list, grp.nm, obs.nm, cross.obs = TRUE) {

   lme4_pkg <- requireNamespace("lme4", quietly = TRUE)
   if (!lme4_pkg) stop("The `lme4` package is required to use this function, but is not installed on this device")
   tmp <- lapply(X = vrb.nm.list, FUN = function(vrb.nm) {
      gtheory_ml(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm, obs.nm = obs.nm,
         cross.obs = cross.obs)
   })
   if (1 == length(vrb.nm.list)) {
      tmp2 <- tmp[[1]]
      rtn <- lapply(X = tmp2, FUN = str2str::v2d, along = 1, rtn.dim.nm = names(vrb.nm.list))
   } else {
      tmp2 <- str2str::t_list(tmp)
      rtn <- lapply(X = tmp2, FUN = str2str::lv2d, along = 1)
   }
   return(rtn)
}
