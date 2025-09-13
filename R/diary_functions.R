# DIARY ####

#' @include quest_functions.R psymet_functions.R describes_functions.R
NULL

# N_BY ####

# length_by #

#' Length of a (Atomic) Vector by Group
#'
#' \code{length_by} computes the the length of a (atomic) vector by group. The
#' argument \code{na.rm} can be used to include (FALSE) or exclude (TRUE)
#' missing values.
#'
#' @param x atomic vector.
#'
#' @param grp atomic vector or list of atomic vectors (e.g., data.frame)
#'   specifying the groups. The atomic vector(s) must be the length of x or else
#'   an error is returned.
#'
#' @param na.rm logical vector of length 1 specifying whether to include (FALSE)
#'   or exclude (TRUE) missing values.
#'
#' @param sep character vector of length 1 specifying what string should
#'   separate different group values when naming the return object. This
#'   argument is only used if grp is a list of atomic vectors (e.g.,
#'   data.frame).
#'
#' @return integer vector of length = \code{length(levels(interaction(grp)))}
#'   with names = \code{length(levels(interaction(grp)))} providing the number
#'   of elements (excluding missing values if \code{na.rm} = TRUE) in each
#'   group.
#'
#' @seealso
#'    \code{\link{lengths_by}}
#'    \code{\link{length}}
#'    \code{\link{agg}}
#'
#' @examples
#'
#' length_by(x = mtcars$"mpg", grp = mtcars$"gear")
#' length_by(x = airquality$"Ozone", grp = airquality$"Month", na.rm = FALSE)
#' length_by(x = airquality$"Ozone", grp = airquality$"Month", na.rm = TRUE)
#'
#' @export
length_by <- function(x, grp, na.rm = FALSE, sep = ".") {

   if (!na.rm) # na.rm = FALSE
      .fun <- na.pass
   else # na.rm = TRUE
      .fun <- na.omit
   agg(x = x, grp = grp, rep = FALSE, rtn.grp = FALSE, sep = sep,
      fun = function(x_by) length(.fun(x_by)))
}

# lengths_by

#' Length of Data Columns by Group
#'
#' \code{lengths_by} computes the the length of multiple columns in a data.frame
#' by group. The argument \code{na.rm} can be used to include (FALSE) or exclude
#' (TRUE) missing values. Through the use of \code{na.rm} = TRUE, the number of
#' observed values for each variable by each group can be computed.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param na.rm logical vector of length 1 specifying whether to include (FALSE)
#'   or exclude (TRUE) missing values.
#'
#' @param sep character vector of length 1 specifying what string should
#'   separate different group values when naming the return object. This
#'   argument is only used if grp is a list of atomic vectors (e.g.,
#'   data.frame).
#'
#' @return data.frame with colnames = \code{vrb.nm} and rownames =
#'   \code{length(levels(interaction(grp)))} providing the number of elements
#'   (excluding missing values if \code{na.rm} = TRUE) in each column by group.
#'
#' @seealso
#'    \code{\link{length_by}}
#'    \code{\link{length}}
#'    \code{\link{colNA}}
#'
#' @examples
#'
#' lengths_by(mtcars, vrb.nm = c("mpg","cyl","disp"), grp = "gear")
#' lengths_by(mtcars, vrb.nm = c("mpg","cyl","disp"),
#'    grp = c("gear","vs")) # can handle multiple grouping variables
#' lengths_by(mtcars, vrb.nm = c("mpg","cyl","disp"),
#'    grp = c("gear","am")) # can handle zero lengths
#' lengths_by(airquality, c("Ozone","Solar.R","Wind"), grp = "Month",
#'    na.rm = FALSE) # include missing values
#' lengths_by(airquality, c("Ozone","Solar.R","Wind"), grp = "Month",
#'    na.rm = TRUE) # exclude missing values
#'
#' @export
lengths_by <- function(data, vrb.nm, grp.nm, na.rm = FALSE, sep = ".") {

   grp <- data[grp.nm]
   data_by <- split(x = data[vrb.nm], f = grp, sep = sep) # split.data.frame
   if (!na.rm)
      Ns <- lapply(X = data_by, FUN = lengths, use.names = TRUE)
   if (na.rm)
      Ns <- lapply(X = data_by, FUN = colNA, prop = FALSE, ov = TRUE)
   rtn <- str2str::lv2d(lv = Ns, along = 1L)
   return(rtn)
}

# nrow_by #

#' Number of Rows in Data by Group
#'
#' \code{nrow_by} computes the nrow of a data.frame by group. \code{nrow_by} is
#' simply a wrapper for \code{nrow} + \code{agg_dfm}.
#'
#' @param data data.frame of data.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   grouping variables.
#'
#' @param sep character vector of length 1 specifying what string to use to
#'   separate the groups when naming the return object. \code{sep} is only used
#'   if \code{grp.nm} has length > 1 (aka multiple grouping variables)
#'
#' @return atomic vector with names = \code{unique(interaction(data[grp.nm], sep
#'   = sep))} and length = \code{length(unique(interaction(data[grp.nm], sep =
#'   sep)))} providing the nrow for each group.
#'
#' @seealso
#'    \code{\link{ncases_by}}
#'    \code{\link{nrow}}
#'    \code{\link{agg_dfm}}
#'
#' @examples
#'
#' # one grouping variables
#' tmp_nm <- c("outcome","case","session","trt_time")
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp_nm]
#' stats_by <- psych::statsBy(dat,
#'    group = "case") # requires you to include "case" column in dat
#' nrow_by(data = dat, grp.nm = "case")
#' dat2 <- as.data.frame(ChickWeight)
#' nrow_by(data = dat2, grp.nm = "Chick")
#'
#' # two grouping variables
#' tmp <- reshape(psych::bfi[1:10, ], varying = 1:25, timevar = "item",
#'    ids = row.names(psych::bfi)[1:10], direction = "long", sep = "")
#' tmp_nm <- c("id","item","N","E","C","A","O") # Roxygen runs the whole script
#' dat3 <- str2str::stack2(tmp[tmp_nm], select.nm = c("N","E","C","A","O"),
#'    keep.nm = c("id","item"))
#' nrow_by(dat3, grp.nm = c("id","vrb_names"))
#'
#' @export
nrow_by <- function(data, grp.nm, sep = ".") {

   agg_dfm(data = data, vrb.nm = names(data), grp.nm = grp.nm, rep = FALSE, rtn.grp = FALSE,
      fun = nrow)
}

# ncases_by #

#' Number of Cases in Data by Group
#'
#' \code{ncases_by} computes the ncases of a data.frame by group. Through the
#' use of the \code{ov.min}, \code{prop}, and \code{inclusive} arguments, the
#' user can specify how many missing values are allowed in a row for it to be
#' counted. \code{ncases_by} is simply a wrapper for \code{ncases} +
#' \code{agg_dfm}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   set of variables to base the ncases on.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   grouping variables.
#'
#' @param sep character vector of length 1 specifying what string to use to
#'   separate the groups when naming the return object. \code{sep} is only used
#'   if \code{grp.nm} has length > 1 (aka multiple grouping variables)
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{length(vrb.nm)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the case
#'   should be included if the frequency of observed values in a row is exactly
#'   equal to \code{ov.min}.
#'
#' @return atomic vector with names = \code{unique(interaction(data[grp.nm], sep
#'   = sep))} and length = \code{length(unique(interaction(data[grp.nm], sep =
#'   sep)))} providing the ncases for each group.
#'
#' @seealso
#'    \code{\link{nrow_by}}
#'    \code{\link{ncases}}
#'    \code{\link{agg_dfm}}
#'
#' @examples
#'
#' # one grouping variables
#' tmp_nm <- c("outcome","case","session","trt_time")
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp_nm]
#' stats_by <- psych::statsBy(dat,
#'    group = "case") # requires you to include "case" column in dat
#' ncases_by(data = dat, grp.nm = "case")
#' dat2 <- as.data.frame(ChickWeight)
#' ncases_by(data = dat2, grp.nm = "Chick")
#'
#' # two grouping variables
#' tmp <- reshape(psych::bfi[1:10, ], varying = 1:25, timevar = "item",
#'    ids = row.names(psych::bfi)[1:10], direction = "long", sep = "")
#' tmp_nm <- c("id","item","N","E","C","A","O") # Roxygen runs the whole script
#' dat3 <- str2str::stack2(tmp[tmp_nm], select.nm = c("N","E","C","A","O"),
#'    keep.nm = c("id","item"))
#' ncases_by(dat3, grp.nm = c("id","vrb_names"))
#'
#' @export
ncases_by <- function(data, vrb.nm = str2str::pick(names(data), val = grp.nm, not = TRUE),
   grp.nm, sep = ".", ov.min = 1L, prop = TRUE, inclusive = TRUE) {

   agg_dfm(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm, rep = FALSE, rtn.grp = FALSE,
      fun = ncases, ov.min = ov.min, prop = prop, inclusive = inclusive)
}

# ncases_desc #

#' Describe Number of Cases in Data by Group
#'
#' \code{ncases_desc} computes descriptive statistics about the number of cases
#' by group in a data.frame. This is often done in diary studies to obtain
#' information about compliance for the sample. Through the use of the
#' \code{ov.min}, \code{prop}, and \code{inclusive} arguments, the user can
#' specify how many missing values are allowed in a row for it to be counted.
#' \code{ncases_desc} is simply \code{ncases_by} + \code{psych::describe}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   set of variables to base the ncases on.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   grouping variables.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{length(vrb.nm)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the case
#'   should be included if the frequency of observed values in a row is exactly
#'   equal to \code{ov.min}.
#'
#' @param interp logical vector of length 1 specifying whether the median should
#'   be standard (FALSE) or interpolated (TRUE).
#'
#' @param skew logical vector of length 1 specifying whether skewness and
#'   kurtosis should be calculated (TRUE) or not (FALSE).
#'
#' @param ranges logical vector of length 1 specifying whether the minimum,
#'   maximum, and range (i.e., maximum - minimum) should be calculated (TRUE) or
#'   not (FALSE). Note, if \code{ranges} = FALSE, the trimmed mean and median
#'   absolute deviation is also not computed as per the \code{psych::describe}
#'   function behavior.
#'
#' @param trim numeric vector of length 1 specifying the top and bottom
#'   quantiles of data that are to be excluded when calculating the trimmed
#'   mean. For example, the default value of 0.1 means that only data within the
#'   10th - 90th quantiles are used for calculating the trimmed mean.
#'
#' @param type numeric vector of length 1 specifying the type of skewness and
#'   kurtosis coefficients to compute. See the details of
#'   \code{psych::describe}. The options are 1, 2, or 3.
#'
#' @param quant numeric vector specifying the quantiles to compute. Foe example,
#'   the default value of c(0.25, 0.75) computes the 25th and 75th quantiles of
#'   the group number of cases. If \code{quant} = NULL, then no quantiles are
#'   returned.
#'
#' @param IQR logical vector of length 1 specifying whether to compute the
#'   Interquartile Range (TRUE) or not (FALSE), which is simply the 75th quantil
#'   - 25th quantile.
#'
#' @return numeric vector containing descriptive statistics about number of cases by group.
#' Note, which elements are returned depends on the arguments. See each argument's description.
#'
#' \describe{
#'    \item{n}{number of groups}
#'    \item{mean}{mean}
#'    \item{sd}{standard deviation}
#'    \item{median}{median (standard if \code{interp} = FALSE, interpolated if \code{interp} = TRUE)}
#'    \item{trimmed}{trimmed mean based on \code{trim}}
#'    \item{mad}{median absolute difference}
#'    \item{min}{minimum}
#'    \item{max}{maximum}
#'    \item{range}{maximum - minumum}
#'    \item{skew}{skewness}
#'    \item{kurtosis}{kurtosis}
#'    \item{se}{standard error of the mean}
#'    \item{IQR}{75th quantile - 25th quantile}
#'    \item{QX.XX}{quantiles, which are named by \code{quant} (e.g., 0.25 = "Q0.25")}
#' }
#'
#' @seealso
#'    \code{\link{ncases_by}}
#'    \code{\link[psych]{describe}}
#'
#' @examples
#' tmp_nm <- c("outcome","case","session","trt_time")
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp_nm]
#' stats_by <- psych::statsBy(dat, group = "case") # doesn't include everything you want
#' ncases_desc(data = dat, grp.nm = "case")
#' dat2 <- as.data.frame(ChickWeight)
#' ncases_desc(data = dat2, grp.nm = "Chick")
#' ncases_desc(data = dat2, grp.nm = "Chick", trim = .05)
#' ncases_desc(data = dat2, grp.nm = "Chick", ranges = FALSE)
#' ncases_desc(data = dat2, grp.nm = "Chick", quant = NULL)
#' ncases_desc(data = dat2, grp.nm = "Chick", IQR = TRUE)
#' @export
ncases_desc <- function(data, vrb.nm = str2str::pick(names(data), val = grp.nm, not = TRUE),
   grp.nm, ov.min = 1, prop = TRUE, inclusive = TRUE,
   interp = FALSE, skew = TRUE, ranges = TRUE, trim = 0.1, type = 3,
   quant = c(0.25, 0.75), IQR = FALSE) {

   n_by <- ncases_by(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm,
      ov.min = ov.min, prop = prop, inclusive = inclusive)
   n_des <- psych::describe(n_by, na.rm = TRUE, check = FALSE,
      interp = interp, skew = skew, ranges = ranges, trim = trim, type = type,
      quant = quant, IQR = IQR)
   rtn <- str2str::d2v(n_des, use.dimnames = TRUE)[-1]
   return(rtn)
}

# N_ML ####

# ngrp #

#' Number of Groups in Data
#'
#' \code{ngrp} computes the number of groups in data given one or more grouping
#' variables. This is simply a combination of \code{unique.data.frame} +
#' \code{nrow}.
#'
#' @param data data.frame of data.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   grouping variables.
#'
#' @return integer vector of length 1 specifying the number of groups.
#'
#' @seealso
#'    \code{\link{nrow_ml}}
#'    \code{\link{ncases_ml}}
#'    \code{\link{nrow_by}}
#'    \code{\link{ncases_by}}
#'
#' @examples
#'
#' # one grouping variable
#' Orthodont2 <- as.data.frame(nlme::Orthodont)
#' ngrp(Orthodont2, grp.nm = "Subject")
#' length(unique(Orthodont2$"Subject"))
#'
#' # two grouping variable
#' co2 <- as.data.frame(CO2)
#' ngrp(co2, grp.nm = c("Plant"))
#' grp_nm <- c("Type","Treatment")
#' ngrp(co2, grp.nm = grp_nm)
#' unique.data.frame(co2[grp_nm])
#'
#' # factor level combinations with no cases are NOT counted
#' logi_vec <- !(co2$"Type" == "Quebec" & co2$"Treatment" == "chilled")
#' co2_3combo <- co2[logi_vec, ]
#' grp_nm <- c("Type","Treatment")
#' ngrp(co2_3combo, grp.nm = grp_nm)
#' unique.data.frame(co2_3combo[grp_nm])
#'
#' @export
ngrp <- function(data, grp.nm) {
   nrow(unique.data.frame(data[grp.nm])) # [] instead of [[]] so can allow for multiple grouping variables
}

# nrow_ml #

#' Multilevel Number of Rows
#'
#' \code{nrow_ml} computes the number rows in the data as well as the number of
#' groups in the data. This corresponds to the within-group sample size and
#' between-group sample size (ignoring any missing data). This is simply a
#' combination of \code{nrow} + \code{ngrp}.
#'
#' @param data data.frame of data.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   grouping variables.
#'
#' @return list with two elements providing the sample sizes (ignoring missing
#'   data). The first element is named "within" and contains the number of rows
#'   in the data. The second element is named "between" and contains the number
#'   of groups in the data.
#'
#' @seealso
#'    \code{\link{ncases_ml}}
#'    \code{\link{nrow_by}}
#'    \code{\link{ncases_by}}
#'    \code{\link{ngrp}}
#'
#' @examples
#'
#' # one grouping variable
#' nrow_ml(data = as.data.frame(ChickWeight), grp.nm = "Chick")
#'
#' # multiple grouping variables
#' nrow_ml(data = mtcars, grp.nm = c("vs","am"))
#'
#' @export
nrow_ml <- function(data, grp.nm) {

   n_wth <- nrow(data)
   n_btw <- ngrp(data = data, grp.nm = grp.nm)
   rtn <- list("within" = n_wth, "between" = n_btw)
   return(rtn)
}

# ncases_ml #

#' Multilevel Number of Cases
#'
#' \code{ncases_ml} computes the number cases and number of groups in the data
#' that are at least partially observed, given a specified frequency of observed
#' values across a set of columns. \code{ncases_ml} allows the user to specify
#' the frequency of columns that need to be observed in order to count the case.
#' Groups can be excluded if no rows in the data for a group have enough
#' observed values to be counted as cases. This is simply a combination of
#' \code{partial.cases} + \code{nrow_ml}. Note, \code{ncases_ml} is essentially
#' a version of \code{\link{nrow_ml}} that accounts for missing data.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm a character vector of colnames from \code{data} specifying the
#'   variables which will be used to determine the partially observed cases.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   grouping variables.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{length(vrb.nm)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the case
#'   should be included if the frequency of observed values in a row is exactly
#'   equal to \code{ov.min}.
#'
#' @return list with two elements providing the sample sizes (accouning for
#'   missing data). The first element is named "within" and contains the number
#'   of cases in the data. The second element is named "between" and contains
#'   the number of groups in the data. Cases are counted if if the frequency of
#'   observed values is greater than (or equal to, if \code{inclusive} = TRUE).
#'
#' @seealso
#'    \code{\link{nrow_ml}}
#'    \code{\link{ncases_by}}
#'    \code{\link{partial.cases}}
#'
#' @examples
#'
#' # NO MISSING DATA
#'
#' # one grouping variable
#' ncases_ml(data = as.data.frame(ChickWeight), grp.nm = "Chick")
#'
#' # multiple grouping variables
#' ncases_ml(data = mtcars, grp.nm = c("vs","am"))
#'
#' # YES MISSING DATA
#'
#' # only within
#' nrow_ml(data = airquality, grp.nm = "Month")
#' ncases_ml(data = airquality, grp.nm = "Month")
#'
#' # both within and between
#' airquality2 <- airquality
#' airquality2[airquality2$"Month" == 6, "Ozone"] <- NA
#' nrow_ml(data = airquality2, grp.nm = "Month")
#' ncases_ml(data = airquality2, grp.nm = "Month")
#'
#' @export
ncases_ml <- function(data, vrb.nm = str2str::pick(names(data), val = grp.nm, not = TRUE),
   grp.nm, ov.min = 1L, prop = TRUE, inclusive = TRUE) {

   keep_cases <- partial.cases(data = data, vrb.nm = vrb.nm, ov.min = ov.min,
      prop = prop, inclusive = inclusive)
   data_keep <- data[keep_cases, ]
   rtn <- nrow_ml(data = data_keep, grp.nm = grp.nm)
   return(rtn)
}

# MULTILEVEL ####

# icc_11 #

#' Intraclass Correlation for Multilevel Analysis: ICC(1,1)
#'
#' \code{icc_11} computes the intraclass correlation (ICC) based on a single
#' rater with a single dimension, aka ICC(1,1). Traditionally, this is the type
#' of ICC used for multilevel analysis where the value is interpreted as the
#' proportion of variance accounted for by group membership. In other words,
#' ICC(1,1) = the proportion of between-group variance; 1 - ICC(1,1) = the
#' proportion of within-group variance.
#'
#' @param x numeric vector.
#'
#' @param grp atomic vector the same length as \code{x} providing the grouping
#'   variable.
#'
#' @param how character vector of length 1 specifying how the ICC(1,1) should be
#'   calculated. There are four options: 1) "lme" uses a linear mixed effects
#'   model with the function \code{\link[nlme]{lme}} from the package
#'   \code{nlme}, 2) "lmer" uses a linear mixed effects modeling with the
#'   function \code{\link[lme4]{lmer}} from the package \code{lme4}, 3) "aov"
#'   uses a one-way analysis of variance with the function
#'   \code{\link[stats]{aov}}, and 4) "raw" uses the observed variances, which
#'   provides a biased estimate of the ICC(1,1) and is not recommended (It is
#'   only included for teaching purposes).
#'
#' @param REML logical vector of length 1 specifying whether restricted maximum
#'   likelihood estimation (TRUE) should be used rather than traditional maximum
#'   likelihood estimation (FALSE). Only used for linear mixed effects models if
#'   how = "lme" or how = "lmer".
#'
#' @return numeric vector of length 1 providing ICC(1,1) and computed based on
#'   the \code{how} argument.
#'
#' @seealso
#'    \code{\link{iccs_11}} # ICC(1,1) for multiple variables,
#'    \code{\link{icc_all_by}} # all six types of ICCs by group,
#'    \code{\link[nlme]{lme}} # how = "lme" function,
#'    \code{\link[lme4]{lmer}} # how = "lmer" function,
#'    \code{\link[stats]{aov}} # how = "aov" function,
#'
#' @examples
#'
#' # BALANCED DATA (how = "aov" and "lme"/"lmer" do YES provide the same value)
#'
#' str(InsectSprays)
#' icc_11(x = InsectSprays$"count", grp = InsectSprays$"spray", how = "aov")
#' icc_11(x = InsectSprays$"count", grp = InsectSprays$"spray", how = "lme")
#' icc_11(x = InsectSprays$"count", grp = InsectSprays$"spray", how = "lmer")
#' icc_11(x = InsectSprays$"count", grp = InsectSprays$"spray",
#'    how = "raw") # biased estimator and not recommended. Only available for teaching purposes.
#'
#' # UN-BALANCED DATA (how = "aov" and "lme"/"lmer" do NOT provide the same value)
#'
#' dat <- as.data.frame(lmeInfo::Bryant2016)
#' icc_11(x = dat$"outcome", grp = dat$"case", how = "aov")
#' icc_11(x = dat$"outcome", grp = dat$"case", how = "lme")
#' icc_11(x = dat$"outcome", grp = dat$"case", how = "lmer")
#' icc_11(x = dat$"outcome", grp = dat$"case", how = "lme", REML = FALSE)
#' icc_11(x = dat$"outcome", grp = dat$"case", how = "lmer", REML = FALSE)
#'
#' # how = "lme" does not account for any correlation structure
#' lme_obj <- nlme::lme(outcome ~ 1, random = ~ 1 | case,
#'    data = dat, na.action = na.exclude,
#'    correlation = nlme::corAR1(form = ~ 1 | case), method = "REML")
#' var_corr <- nlme::VarCorr(lme_obj) # VarCorr.lme
#' vars <- as.double(var_corr[, "Variance"])
#' btw <- vars[1]
#' wth <- vars[2]
#' btw / (btw + wth)
#'
#' @export
icc_11 <- function(x, grp, how = "lme", REML = TRUE) {

   if (length(x) != length(grp))
      stop("`x` and `grp` must both have the same length (and be atomic vectors")
   dat <- data.frame("x" = x, "grp" = grp)
   dat$"grp" <- as.factor(dat$"grp")
   how <- match.arg(how, choices = c("lme","lmer","aov","raw"))
   checkmate::assertLogical(REML, any.missing = FALSE, len = 1)
   if (how == "raw") {
      dat_wth <- center_by(x = x, grp = grp)
      dat_btw <- agg(x = x, grp = grp, fun = mean, na.rm = TRUE)
      var_wth <- var(dat_wth, na.rm = TRUE)
      var_btw <- var(dat_btw, na.rm = TRUE)
      rtn <- var_btw / (var_btw + var_wth)
   }
   if (how == "aov") {
      aov_obj <- aov(formula = x ~ grp, data = dat)
      smry_obj <- summary(aov_obj) # summary.aov
      rtn <- multilevel::ICC1(aov_obj)
   }
   if (how == "lme") {
      method <- ifelse(REML, yes = "REML", no = "ML")
      lme_obj <- nlme::lme(fixed = x ~ 1, random = ~ 1 | grp, data = dat,
         correlation = NULL, method = method, na.action = na.exclude)
      var_corr <- nlme::VarCorr(lme_obj) # VarCorr.lme
      vars <- as.double(var_corr[, "Variance"])
      btw <- vars[1]
      wth <- vars[2]
      rtn <- btw / (btw + wth)
   }
   if (how == "lmer") {
      lmer_obj <- lme4::lmer(formula = x ~ 1 + (1 | grp), data = dat,
         REML = REML, na.action = na.exclude)
      var_corr <- lme4::VarCorr(lmer_obj) # VarCorr.merMod
      btw <- var_corr[["grp"]]["(Intercept)","(Intercept)"]
      wth <- attr(x = var_corr, which = "sc") ^ 2
      rtn <- btw / (btw + wth)
   }
   return(rtn)
}

# iccs_11 #

#' Intraclass Correlation for Multiple Variables for Multilevel Analysis:
#' ICC(1,1)
#'
#' \code{iccs_11} computes the intraclass correlation (ICC) for multiple
#' variables based on a single rater with a single dimension, aka ICC(1,1).
#' Traditionally, this is the type of ICC used for multilevel analysis where the
#' value is interpreted as the proportion of variance accounted for by group
#' membership. In other words, ICC(1,1) = the proportion of between-group
#' variance; 1 - ICC(1,1) = the proportion of within-group variance.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variable columns.
#'
#' @param grp.nm character vector of length 1 of a colname from \code{data}
#'   specifying the grouping column.
#'
#' @param how character vector of length 1 specifying how the ICC(1,1) should be
#'   calculated. There are four options: 1) "lme" uses a linear mixed effects
#'   model with the function \code{\link[nlme]{lme}} from the package
#'   \code{nlme}, 2) "lmer" uses a linear mixed effects modeling with the
#'   function \code{\link[lme4]{lmer}} from the package \code{lme4}, 3) "aov"
#'   uses a one-way analysis of variance with the function
#'   \code{\link[stats]{aov}}, and 4) "raw" uses the observed variances, which
#'   provides a biased estimate of the ICC(1,1) and is not recommended (It is
#'   only included for teaching purposes).
#'
#' @param REML logical vector of length 1 specifying whether restricted maximum
#'   likelihood estimation (TRUE) should be used rather than traditional maximum
#'   likelihood (FALSE). This is only applicable to linear mixed effects models
#'   when \code{how} is "lme" or "lmer".
#'
#' @return double vector containing ICC(1, 1) of the \code{vrb.nm} columns in
#'   \code{data} with names of the return object equal to \code{vrb.nm}.
#'
#' @seealso
#'    \code{icc_11} # ICC(1,1) for a single variable,
#'    \code{\link{icc_all_by}} # all six types of ICCs by group,
#'    \code{\link[nlme]{lme}} # how = "lme" function,
#'    \code{\link[lme4]{lmer}} # how = "lmer" function,
#'    \code{\link[stats]{aov}} # how = "aov" function,
#'
#' @examples
#'
#' tmp_nm <- c("outcome","case","session","trt_time")
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp_nm]
#' stats_by <- psych::statsBy(dat,
#'    group = "case") # requires you to include "case" column in dat
#' iccs_11(data = dat, vrb.nm = c("outcome","session","trt_time"), grp.nm = "case")
#'
#' @export
iccs_11 <- function(data, vrb.nm, grp.nm, how = "lme", REML = FALSE) {

   tmp <- lapply(X = data[vrb.nm], FUN = icc_11,
      grp = data[[grp.nm]], how = how, REML = REML)
   rtn <- unlist(tmp)
   return(rtn)
}

# deff #

#' Design Effect from Multilevel Numeric Vector
#'
#' \code{deff} computes the design effect for a multilevel numeric vector.
#' Design effects summarize how much larger sampling variances (i.e., squared
#' standard errors) are due to the multilevel structure of the data. By taking
#' the square root, the value summarizes how much larger standard errors are due
#' to the multilevel structure of the data.
#'
#' Design effects are a function of both the intraclass correlation (ICC) and
#' the average group size. Design effects can be large due to large ICCs and
#' small group sizes or small ICCs and large group sizes. For example, with an
#' ICC = .01 and average group size of 100, the design effect would be 2.0,
#' whose square root is 1.41. For more information, see myths 1 and 2 in
#' Huang (2018).
#'
#' @param x numeric vector.
#'
#' @param grp atomic vector the same length as \code{x} providing the grouping
#'   variable.
#'
#' @param how character vector of length 1 specifying how the ICC(1,1) should be
#'   calculated. There are four options: 1) "lme" uses a linear mixed effects
#'   model with the function \code{\link[nlme]{lme}} from the package
#'   \code{nlme}, 2) "lmer" uses a linear mixed effects modeling with the
#'   function \code{\link[lme4]{lmer}} from the package \code{lme4}, 3) "aov"
#'   uses a one-way analysis of variance with the function
#'   \code{\link[stats]{aov}}, and 4) "raw" uses the observed variances, which
#'   provides a biased estimate of the ICC(1,1) and is not recommended (It is
#'   only included for teaching purposes).
#'
#' @param REML logical vector of length 1 specifying whether restricted maximum
#'   likelihood estimation (TRUE) should be used rather than traditional maximum
#'   likelihood estimation (FALSE). Only used for linear mixed effects models if
#'   how = "lme" or how = "lmer".
#'
#' @return double vector of lenght 1 providing the design effect.
#'
#' @seealso
#'    \code{\link{icc_11}}
#'    \code{\link{deffs}}
#'
#' @references
#'
#' Huang, F. L. (2018). Multilevel modeling myths School Psychology Quarterly,
#' 33(3), 492-499.
#'
#' @examples
#'
#' icc_11(x = airquality$"Ozone", grp = airquality$"Month")
#' length_by(x = airquality$"Ozone", grp = airquality$"Month", na.rm = TRUE)
#' deff(x = airquality$"Ozone", grp = airquality$"Month")
#' sqrt(deff(x = airquality$"Ozone", grp = airquality$"Month")) # how much SE inflated
#'
#' @export
deff <- function(x, grp, how = "lme", REML = TRUE) {

   x_by <- split(x = x, f = grp) # split.default
   n_by <- length_by(x = x, grp = grp, na.rm = TRUE)
   nc <- psych::harmonic.mean(n_by,
      na.rm = FALSE, zero = FALSE) # there should not be any NAs or 0s in n_by so these arguments are for error or unexpected rtn obj checking
   icc <- icc_11(x = x, grp = grp, how = how, REML = REML)
   rtn <- 1 + (icc * (nc - 1))
   return(rtn)
}

# deffs #

#' Design Effects from Multilevel Numeric Data
#'
#' \code{deffs} computes the design effects for multilevel numeric data. Design
#' effects summarize how much larger sampling variances (i.e., squared standard
#' errors) are due to the multilevel structure of the data. By taking the square
#' root, the value summarizes how much larger standard errors are due to the
#' multilevel structure of the data.
#'
#' Design effects are a function of both the intraclass correlation (ICC) and
#' the average group size. Design effects can be large due to large ICCs and
#' small group sizes or small ICCs and large group sizes. For example, with an
#' ICC = .01 and average group size of 100, the design effect would be 2.0,
#' whose square root is 1.41. For more information, see myths 1 and 2 in
#' Huang (2018).
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variable columns.
#'
#' @param grp.nm character vector of length 1 of a colname from \code{data}
#'   specifying the grouping column.
#'
#' @param how character vector of length 1 specifying how the ICC(1,1) should be
#'   calculated. There are four options: 1) "lme" uses a linear mixed effects
#'   model with the function \code{\link[nlme]{lme}} from the package
#'   \code{nlme}, 2) "lmer" uses a linear mixed effects modeling with the
#'   function \code{\link[lme4]{lmer}} from the package \code{lme4}, 3) "aov"
#'   uses a one-way analysis of variance with the function
#'   \code{\link[stats]{aov}}, and 4) "raw" uses the observed variances, which
#'   provides a biased estimate of the ICC(1,1) and is not recommended (It is
#'   only included for teaching purposes).
#'
#' @param REML logical vector of length 1 specifying whether restricted maximum
#'   likelihood estimation (TRUE) should be used rather than traditional maximum
#'   likelihood estimation (FALSE). Only used for linear mixed effects models if
#'   how = "lme" or how = "lmer".
#'
#' @return double vector providing the design effects with names =
#'   \code{vrb.nm}.
#'
#' @seealso
#'    \code{\link{iccs_11}}
#'    \code{\link{deff}}
#'
#' @references
#'
#' Huang, F. L. (2018). Multilevel modeling myths School Psychology Quarterly,
#' 33(3), 492-499.
#'
#' @examples
#'
#' iccs_11(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month")
#' lengths_by(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month", na.rm = TRUE)
#' deffs(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month")
#'
#' @export
deffs <- function(data, vrb.nm, grp.nm, how = "lme", REML = FALSE) {

   tmp <- lapply(X = data[vrb.nm], FUN = deff,
      grp = data[[grp.nm]], how = how, REML = REML)
   rtn <- unlist(tmp)
   return(rtn)
}

# describe_ml #

#' Multilevel Descriptive Statistics
#'
#' \code{describe_ml} decomposes descriptive statistics from multilevel data
#' into within-group and between-group descriptives. The data is first separated
#' out into within-group components via \code{centers_by} and between-group
#' components via \code{aggs}. Then the \code{psych} function
#' \code{\link[psych]{describe}} is applied to both.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variable columns.
#'
#' @param grp.nm character vector of length 1 of a colname from \code{data}
#'   specifying the grouping column.
#'
#' @param na.rm logical vector of length 1 specifying whether missing values
#'   should be removed before calculating the descriptive statistics. See
#'   \code{psych::describe}.
#'
#' @param interp logical vector of length 1 specifying whether the median should
#'   be standard (FALSE) or interpolated (TRUE).
#'
#' @param skew logical vector of length 1 specifying whether skewness and
#'   kurtosis should be calculated (TRUE) or not (FALSE).
#'
#' @param ranges logical vector of length 1 specifying whether the minimum,
#'   maximum, and range (i.e., maximum - minimum) should be calculated (TRUE) or
#'   not (FALSE). Note, if \code{ranges} = FALSE, the trimmed mean and median
#'   absolute deviation is also not computed as per the \code{psych::describe}
#'   function behavior.
#'
#' @param trim numeric vector of length 1 specifying the top and bottom
#'   quantiles of data that are to be excluded when calculating the trimmed
#'   mean. For example, the default value of 0.1 means that only data within the
#'   10th - 90th quantiles are used for calculating the trimmed mean.
#'
#' @param type numeric vector of length 1 specifying the type of skewness and
#'   kurtosis coefficients to compute. See the details of
#'   \code{psych::describe}. The options are 1, 2, or 3.
#'
#' @param quant numeric vector specifying the quantiles to compute. Foe example,
#'   the default value of c(0.25, 0.75) computes the 25th and 75th quantiles of
#'   the group number of cases. If \code{quant} = NULL, then no quantiles are
#'   returned.
#'
#' @param IQR logical vector of length 1 specifying whether to compute the
#'   Interquartile Range (TRUE) or not (FALSE), which is simply the 75th quantil
#'   - 25th quantile.
#'
#' @return list of two elements each containing a data.frame of descriptive
#'   statistics, the first for the within-person components ("within") and the
#'   second for the between-person components ("between").
#'
#' @seealso
#'    \code{\link[psych]{describe}}
#'
#' @examples
#'
#' tmp_nm <- c("outcome","case","session","trt_time")
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp_nm]
#' stats_by <- psych::statsBy(dat, group = "case") # requires you to include "case" column in dat
#' describe_ml(data = dat, vrb.nm = c("outcome","session","trt_time"), grp.nm = "case")
#'
#' @export
describe_ml <- function(data, vrb.nm, grp.nm, na.rm = TRUE, interp = FALSE,
   skew = TRUE, ranges = TRUE, trim = 0.1, type = 3, quant = NULL, IQR = FALSE) {

   data_wth <- centers_by(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm, suffix = "")
   data_btw <- aggs(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm, rep = FALSE,
      rtn.grp = FALSE, suffix = "", fun = mean, na.rm = TRUE)
   rtn <- lapply(X = list("within" = data_wth, "between" = data_btw), FUN = psych::describe,
      na.rm = na.rm, interp = interp, skew = skew, ranges = ranges, trim = trim,
      type = type, quant = quant, IQR = IQR)
   return(rtn)
}

# cor_ml #

#' Multilevel Correlation Matrices
#'
#' \code{cor_ml} decomposes correlations from multilevel data into within-group
#' and between-group correlations. The workhorse of the function is
#' \code{\link[psych]{statsBy}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variable columns.
#'
#' @param grp.nm character vector of length 1 of a colname from \code{data}
#'   specifying the grouping column.
#'
#' @param use character vector of length 1 specifying how to handle missing
#'   values when computing the correlations. The options are: 1.
#'   "pairwise.complete.obs" which uses pairwise deletion, 2. "complete.obs"
#'   which uses listwise deletion, and 3. "everything" which uses all cases and
#'   returns NA for any correlations from columns in \code{data[vrb.nm]} with
#'   missing values.
#'
#' @param method character vector of length 1 specifying which type of
#'   correlations to compute. The options are: 1. "pearson" for traditional
#'   Pearson product-moment correlations, 2. "kendall" for Kendall rank
#'   correlations, and 3. "spearman" for Spearman rank correlations.
#'
#' @return list with two elements named "within" and "between" each containing a
#'   numeric matrix. The first "within" matrix is the within-group correlation
#'   matrix and the second "between" matrix is the between-group correlation
#'   matrix. The rownames and colnames of each numeric matrix are \code{vrb.nm}.
#'
#' @seealso
#'    \code{\link{corp_ml}} for multilevel correlations with significance symbols,
#'    \code{\link{cor_by}} for correlation matrices by group,
#'    \code{\link[stats]{cor}} for traditional, single-level correlation matrices,
#'    \code{\link[psych]{statsBy}} the workhorse for the \code{cor_ml} function,
#'
#' @examples
#'
#' # traditional use
#' tmp <- c("outcome","case","session","trt_time") # roxygen2 does not like c() inside []
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp]
#' stats_by <- psych::statsBy(dat, group = "case") # requires you to include "case" column in dat
#' cor_ml(data = dat, vrb.nm = c("outcome","session","trt_time"), grp.nm = "case")
#'
#' # varying the \code{use} and \code{method} arguments
#' cor_ml(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind","Temp"), grp.nm = "Month",
#'    use = "pairwise", method = "pearson")
#' cor_ml(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind","Temp"), grp.nm = "Month",
#'    use = "complete", method = "kendall")
#' cor_ml(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind","Temp"), grp.nm = "Month",
#'    use = "everything", method = "spearman")
#'
#' @export
cor_ml <- function(data, vrb.nm, grp.nm, use = "pairwise.complete.obs", method = "pearson") {

   statsBy_obj <- psych::statsBy(data[c(grp.nm, vrb.nm)], group = grp.nm,
      method = method, use = use)
   cor_wth <- statsBy_obj[["rwg"]]
   dimnames(cor_wth) <- list(vrb.nm, vrb.nm)
   cor_btw <- statsBy_obj[["rbg"]]
   dimnames(cor_btw) <- list(vrb.nm, vrb.nm)
   rtn <- list("within" = cor_wth, "between" = cor_btw)
   return(rtn)
}

# corp_ml #

#' Multilevel Correlation Matrices with Significant Symbols
#'
#' \code{corp_ml} decomposes correlations from multilevel data into within-group
#' and between-group correlations as well as adds significance symbols to the
#' end of each value. The workhorse of the function is
#' \code{\link[psych]{statsBy}}. \code{corp_ml} is simply a combination of
#' \code{\link{cor_ml}} and \code{\link{add_sig_cor}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variable columns.
#'
#' @param grp.nm character vector of length 1 of a colname from \code{data}
#'   specifying the grouping column.
#'
#' @param use character vector of length 1 specifying how to handle missing
#'   values when computing the correlations. The options are: 1)
#'   "pairwise.complete.obs" which uses pairwise deletion, 2) "complete.obs"
#'   which uses listwise deletion, and 3) "everything" which uses all cases and
#'   returns NA for any correlations from columns in \code{data[vrb.nm]} with
#'   missing values.
#'
#' @param method character vector of length 1 specifying which type of
#'   correlations to compute. The options are: 1) "pearson" for traditional
#'   Pearson product-moment correlations, 2) "kendall" for Kendall rank
#'   correlations, and 3) "spearman" for Spearman rank correlations.
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
#' @return list of two elements that are data.frames with names "within" and
#'   "between". The first data.frame has the within-group correlations with
#'   their significance symbols at the end of the statistically significant
#'   correlations based on their associated p-value. The second data.frame has
#'   the between-group correlations with their significance symbols at the end
#'   of the statistically significant correlations based on their associated
#'   p-values. The rownames and colnames of each dataframe are \code{vrb.nm}.
#'   The formatting of the two data.frames depends on several of the arguments.
#'
#' @seealso
#'    \code{\link{cor_ml}} for multilevel correlations without significance symbols,
#'    \code{\link{corp_by}} for correlations with significance symbols by group,
#'    \code{\link[psych]{statsBy}} the workhorse for the \code{corp_ml} function,
#'    \code{\link{add_sig_cor}} for adding significant symbols to correlation matrices,
#'
#' @examples
#'
#' # traditional use
#' tmp <- c("outcome","case","session","trt_time") # roxygen2 does not like c() inside []
#' dat <- as.data.frame(lmeInfo::Bryant2016)[tmp]
#' stats_by <- psych::statsBy(dat, group = "case") # requires you to include "case" column in dat
#' corp_ml(data = dat, vrb.nm = c("outcome","session","trt_time"), grp.nm = "case")
#'
#' # varying the `use` and `method` arguments
#' corp_ml(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind","Temp"), grp.nm = "Month",
#'    use = "pairwise", method = "pearson")
#' corp_ml(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind","Temp"), grp.nm = "Month",
#'    use = "complete", method = "kendall")
#' corp_ml(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind","Temp"), grp.nm = "Month",
#'    use = "everything", method = "spearman")
#'
#' @export
corp_ml <- function(data, vrb.nm, grp.nm, use = "pairwise.complete.obs", method = "pearson",
   digits = 3L, p.10 = "", p.05 = "*", p.01 = "**", p.001 = "***",
   lead.zero = FALSE, trail.zero = TRUE, plus = FALSE,
   diags = FALSE, lower = TRUE, upper = FALSE) {

   # statsBy
   statsBy_obj <- psych::statsBy(data[c(grp.nm, vrb.nm)], group = grp.nm,
      method = method, use = use)

   # r
   r_wth <- statsBy_obj[["rwg"]]
   dimnames(r_wth) <- list(vrb.nm, vrb.nm)
   r_btw <- statsBy_obj[["rbg"]]
   dimnames(r_btw) <- list(vrb.nm, vrb.nm)
   r_ml <- list("within" = r_wth, "between" = r_btw)

   # p
   p_wth <- statsBy_obj[["pwg"]]
   dimnames(p_wth) <- list(vrb.nm, vrb.nm)
   p_btw <- statsBy_obj[["pbg"]]
   dimnames(p_btw) <- list(vrb.nm, vrb.nm)
   p_ml <- list("within" = p_wth, "between" = p_btw)

   # return object
   rtn <- Map(r = r_ml, p = p_ml, f = add_sig_cor,
      digits = digits, p.10 = p.10, p.05 = p.05, p.01 = p.01, p.001 = p.001,
      lead.zero = lead.zero, trail.zero = trail.zero, plus = plus,
      diags = diags, lower = lower, upper = upper)
   return(rtn)
}

# auto_by #

#' Autoregressive Coefficient by Group
#'
#' \code{auto_by} computes the autoregressive coefficient by group for
#' longitudinal data where each observation within the group represents a
#' different timepoint. The function assumes the data are already sorted by
#' time.
#'
#' There are several different ways to estimate the autoregressive parameter.
#' This function offers a variety of ways with the \code{how} and \code{cw}
#' arguments. Note, that a recent simulation suggests that group-mean centering
#' via \code{cw} is the best approach when using linear mixed effects modeling
#' via \code{how} = "lme" or "lmer" (Hamaker  & Grasman, 2015).
#'
#' @param x numeric vector.
#'
#' @param grp list of atomic vector(s) and/or factor(s) (e.g., data.frame),
#'   which each have same length as \code{x}. It can also be an atomic vector or
#'   factor, which will then be made the first element of a list.
#'
#' @param n integer vector with length 1. Specifies the direction and magnitude
#'   of the shift. See \code{shift} for details. The default is -1L, which is a
#'   one-lag autoregressive coefficient' +2L would be a two-lead autoregressive
#'   coefficient. The sign of \code{n} only affects the results for \code{how} =
#'   "lm", "lme", or "lmer".
#'
#' @param how character vector of length 1 specifying how to compute the
#'   autoregressive coefficients. The options are 1) "cor" for correlation with
#'   the \code{cor} function, 2) "cov" for covariance with the \code{cov}
#'   function, 3) "lm" for the linear regression slope with the \code{lm}
#'   function, 4) "lme" for empirical Bayes estimates from a linear mixed
#'   effects model with the \code{\link[nlme]{lme}} function, 5) "lmer" for
#'   empirical Bayes estimates from a linear mixed effects model with the
#'   \code{\link[lme4]{lmer}} function.
#'
#' @param cw logical vector of length 1 specifying whether the shifted vector
#'   should be group-mean centered (TRUE) or not (FALSE). This only affects the
#'   results for \code{how} = "lme" or "lmer".
#'
#' @param method character vector of length 1 specifying the type of correlation
#'   or covariance to compute. Only used when \code{how} = "cor" or "cov". See
#'   \code{\link[stats]{cor}} for details.
#'
#' @param use character vector of length 1 specifying how to handle missing
#'   data. Only used when \code{how} = "cor" or "cov". See
#'   \code{\link[stats]{cor}} for details.
#'
#' @param REML logical vector of length 1 specifying whether to use restricted
#'   estimated maximum liklihood (TRUE) rather than traditional maximum
#'   likelihood (FALSE). Only used when \code{how} = "lme" or "lmer".
#'
#' @param control list of control parameters for \code{lme} or \code{lmer} when
#'   \code{how} = "lme" or "lmer", respectively. See
#'   \code{\link[nlme]{lmeControl}} and \code{\link[lme4]{lmerControl}} for
#'   details.
#'
#' @param sep character vector of length 1 specifying what string should
#'   separate different group values when naming the return object. This
#'   argument is only used if \code{grp} is a list of atomic vectors (e.g.,
#'   data.frame).
#'
#' @return numeric vector of autoregressive coefficients with length =
#'   \code{length(levels(interaction(grp)))} and names = pasteing of the
#'   grouping value(s) together separated by \code{sep}.
#'
#' @references
#'
#' Hamaker, E. L., & Grasman, R. P. (2015). To center or not to center?  Investigating
#' inertia with a multilevel autoregressive model. Frontiers in Psychology, 5, 1492.
#'
#' @examples
#'
#' # cor
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month", how = "cor")
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month",
#'    n = -2L, how = "cor") # lag across 2 timepoints
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month",
#'    n = +1L, how = "cor") # lag and lead identical for cor
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month", how = "cor",
#'    cw = FALSE) # centering within-person identical for cor
#'
#' # cov
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month", how = "cov")
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month",
#'    n = -2L, how = "cov") # lag across 2 timepoints
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month",
#'    n = +1L, how = "cov") # lag and lead identical for cov
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month", how = "cov",
#'    cw = FALSE) # centering within-person identical for cov
#'
#' # lm
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month", how = "lm")
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month",
#'    n = -2L, how = "lm") # lag across 2 timepoints
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month",
#'    n = +1L, how = "lm") # lag and lead NOT identical for lm
#' auto_by(x = airquality$"Ozone", grp = airquality$"Month", how = "lm",
#'    cw = FALSE) # centering within-person identical for lm
#'
#' # lme
#' chick_weight <- as.data.frame(ChickWeight)
#' auto_by(x = chick_weight$"weight", grp = chick_weight$"Chick", how = "lme")
#' control_lme <- nlme::lmeControl(maxIter = 250L, msMaxIter = 250L,
#'    tolerance = 1e-3, msTol = 1e-3) # custom controls
#' auto_by(x = chick_weight$"weight", grp = chick_weight$"Chick", how = "lme",
#'    control = control_lme)
#' auto_by(x = chick_weight$"weight", grp = chick_weight$"Chick",
#'    n = -2L, how = "lme") # lag across 2 timepoints
#' auto_by(x = chick_weight$"weight", grp = chick_weight$"Chick",
#'    n = +1L, how = "lme") # lag and lead NOT identical for lme
#' auto_by(x = chick_weight$"weight", grp = chick_weight$"Chick", how = "lme",
#'    cw = FALSE) # centering within-person NOT identical for lme
#'
#' # lmer
#' bryant_2016 <- as.data.frame(lmeInfo::Bryant2016)
#' \dontrun{
#' auto_by(x = bryant_2016$"outcome", grp = bryant_2016$"case", how = "lmer")
#' control_lmer <- lme4::lmerControl(check.conv.grad = lme4::.makeCC("stop",
#'    tol = 2e-3, relTol = NULL), check.conv.singular = lme4::.makeCC("stop",
#'    tol = formals(lme4::isSingular)$"tol"), check.conv.hess = lme4::.makeCC(action = "stop",
#'    tol = 1e-6)) # custom controls
#' auto_by(x = bryant_2016$"outcome", grp = bryant_2016$"case", how = "lmer",
#'    control = control_lmer) # TODO: for some reason lmer doesn't like this
#'    # and is not taking into account the custom controls
#' auto_by(x = bryant_2016$"outcome", grp = bryant_2016$"case",
#'    n = -2L, how = "lmer") # lag across 2 timepoints
#' auto_by(x = bryant_2016$"outcome", grp = bryant_2016$"case",
#'    n = +1L, how = "lmer") # lag and lead NOT identical for lmer
#' auto_by(x = bryant_2016$"outcome", grp = bryant_2016$"case", how = "lmer",
#'    cw = FALSE) # centering within-person NOT identical for lmer
#' }
#'
#' @export
auto_by <- function(x, grp, n = -1L, how = "cor", cw = TRUE, method = "pearson",
   use = "na.or.complete", REML = TRUE, control = NULL, sep = ".") {

   if (cw) {
      x_cw <- center_by(x = x, grp = grp)
      x_shift <- shift_by(x = x_cw, grp = grp, n = n)
   } else x_shift <- shift_by(x = x, grp = grp, n = n)
   if (is.list(grp)) grp_fct <- interaction(grp, sep = sep) else grp_fct <- grp
   dat <- data.frame("x" = x, "x_shift" = x_shift, "grp_fct" = grp_fct)
   if (how == "cor") {
      tmp <- lapply(X = split(x = dat, f = dat$"grp_fct"), FUN = function(dfm) # split.data.frame
         cor(x = dfm$"x", y = dfm$"x_shift", method = method, use = use))
      rtn <- str2str::lv2v(tmp, use.listnames = TRUE, use.vecnames = FALSE)
   }
   if (how == "cov") {
      tmp <- lapply(X = split(x = dat, f = dat$"grp_fct"), FUN = function(dfm) # split.data.frame
         cov(x = dfm$"x", y = dfm$"x_shift", method = method, use = use))
      rtn <- str2str::lv2v(tmp, use.listnames = TRUE, use.vecnames = FALSE)
   }
   if (how == "lm") {
      tmp <- lapply(X = split(x = dat, f = dat$"grp_fct"), FUN = function(dfm) { # split.data.frame
         form_lm <- as.formula("x ~ x_shift")
         lm_obj <- lm(formula = form_lm, data = dfm)
         rtn <- coef(lm_obj)["x_shift"] # coef.lm
      })
      rtn <- str2str::lv2v(lapply(X = tmp, FUN = unname), use.listnames = TRUE, use.vecnames = FALSE)
   }
   if (how == "lme") {
      form_fix <- as.formula("x ~ x_shift")
      form_ran <- as.formula("~ 1 + x_shift | grp_fct")
      method_lme <- ifelse(REML, yes = "REML", no = "ML")
      control_lme <- ifelse(is.null(control), yes = nlme::lmeControl(), no = control)
      lme_obj <- nlme::lme(fixed = form_fix, random = form_ran, data = dat,
         correlation = NULL, method = method_lme, na.action = na.omit,
         control = control_lme)
      tmp <- coef(lme_obj)["x_shift"] # coef.lme
      rtn <- str2str::d2v(tmp)
   }
   if (how == "lmer") {
      form_lmer <- as.formula("x ~ x_shift + (1 + x_shift | grp_fct)")
      control_lmer <- ifelse(is.null(control), yes = lme4::lmerControl(), no = control)
      lmer_obj <- lme4::lmer(formula = form_lmer, data = dat, REML = REML,
         na.action = na.omit, control = control_lmer)
      tmp <- coef(lmer_obj)[["grp_fct"]]["x_shift"] # coef.merMod
      rtn <- str2str::d2v(tmp)
   }
   return(rtn)
}

# icc_all_by #

#' All Six Intraclass Correlations by Group
#'
#' \code{icc_all_by} computes each of the six intraclass correlations (ICC) in
#' Shrout & Fleiss (1979) by group. The ICCs differ by whether they treat
#' dimensions as fixed or random and whether they are for a single variable in
#' \code{data[vrb.nm]} of the set of variables \code{data[vrb.nm]}.
#' \code{icc_all_by} also returns information about the linear mixed effects
#' modeling (using \code{\link[lme4]{lmer}}) used to compute the ICCs as well as
#' any warning or error messages by group. For an understanding of the six
#' different ICCs, see the following blogpost:
#' \url{http://www.daviddisabato.com/blog/2021/10/1/the-six-different-types-of-intraclass-correlations-iccs}.
#' \code{icc_all_by} is a combination of \code{\link{by2}} +
#' \code{\link[str2str]{try_fun}} + \code{\link[psych]{ICC}}
#' (\code{\link[psych]{ICC}} calls \code{\link[lme4]{lmer}} internally).
#'
#' \code{icc_all_by} internally suppresses any messages, warnings, or errors
#' returned by \code{\link[lme4]{lmer}} (e.g., "boundary (singular) fit: see
#' ?isSingular") because that information is provided in the returned
#' data.frame.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param ci.level double vector of length 1 specifying the confidence level. It
#'   must range from 0 to 1.
#'
#' @param check logical vector of length 1 specifying whether to check the
#'   structure of the input arguments. For example, check whether
#'   \code{data[vrb.nm]} are all typeof numeric. This argument is available to
#'   allow flexibility in whether the user values informative error messages
#'   (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame containing the unique combinations of the grouping variables
#' \code{data[grp.nm]} and each group's intraclass correlations (ICCs), their confidence intervals,
#' information about the \code{merMod} object from the linear mixed effects model,
#' and any warning or error messages from \code{\link[lme4]{lmer}}. For an understanding of the
#' six different ICCs, see the following blogpost:
#' \url{http://www.daviddisabato.com/blog/2021/10/1/the-six-different-types-of-intraclass-correlations-iccs}.
#' The first columns are always \code{unique.data.frame(data[vrb.nm])}. All other columns are in the
#' following order with the following colnames:
#'
#' \describe{
#'    \item{icc11_est}{ICC(1,1) parameter estimate}
#'    \item{icc11_lwr}{ICC(1,1) lower bound of the confidence interval}
#'    \item{icc11_upr}{ICC(1,1) lower bound of the confidence interval}
#'    \item{icc21_est}{ICC(2,1) parameter estimate}
#'    \item{icc21_lwr}{ICC(2,1) lower bound of the confidence interval}
#'    \item{icc21_upr}{ICC(2,1) lower bound of the confidence interval}
#'    \item{icc31_est}{ICC(3,1) parameter estimate}
#'    \item{icc31_lwr}{ICC(3,1) lower bound of the confidence interval}
#'    \item{icc31_upr}{ICC(3,1) lower bound of the confidence interval}
#'    \item{icc1k_est}{ICC(1,k) parameter estimate}
#'    \item{icc1k_lwr}{ICC(1,k) lower bound of the confidence interval}
#'    \item{icc1k_upr}{ICC(1,k) lower bound of the confidence interval}
#'    \item{icc2k_est}{ICC(2,k) parameter estimate}
#'    \item{icc2k_lwr}{ICC(2,k) lower bound of the confidence interval}
#'    \item{icc2k_upr}{ICC(2,k) lower bound of the confidence interval}
#'    \item{icc3k_est}{ICC(3,k) parameter estimate}
#'    \item{icc3k_lwr}{ICC(3,k) lower bound of the confidence interval}
#'    \item{icc3k_upr}{ICC(3,k) lower bound of the confidence interval}
#'    \item{lmer_nobs}{number of observations used for the linear mixed effects model.
#'    Note, this is the number of (non-missing) rows after \code{data[vrb.nm]}
#'    has been stacked together via \code{\link[utils]{stack}}.}
#'    \item{lmer_ngrps}{number of groups used for the linear mixed effects model.
#'    This is the number of unique combinations of the grouping variables after \code{data[grp.nm]}.}
#'    \item{lmer_logLik}{logLik of the linear mixed effects model}
#'    \item{lmer_sing}{binary variable where 1 = the linear mixed effects model had
#'    a singularity in the random effects covariance matrix or 0 = it did not}
#'    \item{lmer_warn}{binary variable where 1 = the linear mixed effects model
#'    returned a warning or 0 = it did not}
#'    \item{lmer_err}{binary variable where 1 = the linear mixed effects model
#'    returned an error or 0 = it did not}
#'    \item{warn_mssg}{character vector providing the warning messages for any warnings.
#'    If a group did not generate a warning, then the value is NA}
#'    \item{err_mssg}{character vector providing the error messages for any warnings.
#'    If a group did not generate an error, then the value is NA}
#' }
#'
#' @references
#'
#' Shrout, P.E., & Fleiss, J.L. (1979). Intraclass correlations: Uses in assessing rater reliability.
#'    Psychological Bulletin, 86(2), 420-428.
#'
#' @seealso
#'    \code{\link[psych]{ICC}}
#'    \code{\link[lme4]{lmer}}
#'
#' @examples
#'
#' # one grouping variable
#' x <- icc_all_by(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"),
#'    grp.nm = "gender")
#'
#' # two grouping variables
#' y <- icc_all_by(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"),
#'    grp.nm = c("gender","education"))
#'
#' # with errors
#' z <- icc_all_by(data = psych::bfi, vrb.nm = c("A2","A3","A4","A5"),
#'    grp.nm = c("age")) # NA for all ICC columns when there is an error
#'
#' @export
icc_all_by <- function(data, vrb.nm, grp.nm, ci.level = 0.95, check = TRUE) {

   # errors
   if (check) {
      checkmate::assertDataFrame(data)
      checkmate::assertCharacter(vrb.nm, any.missing = FALSE, unique = TRUE)
      checkmate::assertCharacter(grp.nm, any.missing = FALSE, unique = TRUE)
      checkmate::assertNames(vrb.nm, subset.of = names(data), what = "names")
      checkmate::assertNames(grp.nm, subset.of = names(data), what = "names")
      checkmate::assertDataFrame(data[vrb.nm],
         types = c("integer","integerish","double","numeric"))
      checkmate::assertNumeric(ci.level, lower = 0, upper = 1, any.missing = FALSE, len = 1)
   }

   # by
   ICC_try <- str2str::try_fun(psych::ICC)
   ICC_by <- suppressMessages(by(data[vrb.nm], INDICES = data[grp.nm], FUN = ICC_try, # suppress the "boundary (singular) fit: see ?isSingular" messages
      missing = FALSE, alpha = 1 - ci.level, lmer = TRUE, check.keys = FALSE,
      simplify = FALSE)) # returns a list array
   grp_dimnames <- dimnames(ICC_by)

   # extract stat info
   info_by <- lapply(X = ICC_by, FUN = function(try_obj) {

      coef_nm <- c(
         "icc11_est","icc11_lwr","icc11_upr", # Single_raters_absolute
         "icc21_est","icc21_lwr","icc21_upr", # Single_random_raters
         "icc31_est","icc31_lwr","icc31_upr", # Single_fixed_raters
         "icc1k_est","icc1k_lwr","icc1k_upr", # Average_raters_absolute
         "icc2k_est","icc2k_lwr","icc2k_upr", # Average_random_raters
         "icc3k_est","icc3k_lwr","icc3k_upr") # Average_fixed_raters
      lmer_nm <- c("lmer_nobs","lmer_ngrps","lmer_logLik","lmer_sing")
      try_result <- try_obj[["result"]]
      if (!(is.null(try_result))) {
         coef_ICC <- try_result[["results"]]
         tmp <- as.matrix(coef_ICC[c("ICC","lower bound","upper bound")])
         coef_rtn <- setNames(unlist(as.data.frame(t(tmp))), # t() so goes in order of coef_nm
            nm = coef_nm)
         lmer_ICC <- try_result[["summary"]]
         lmer_rtn <- setNames(c(nobs(lmer_ICC), lme4::ngrps(lmer_ICC)["id"], logLik(lmer_ICC),
            ifelse(lme4::isSingular(lmer_ICC), yes = 1L, no = 0L)),
            nm = lmer_nm)
      }
      if (is.null(try_result)) {
         coef_rtn <- setNames(rep.int(x = NA_real_, times = 18), nm = coef_nm)
         lmer_rtn <- setNames(rep.int(x = NA_real_, times = 4), nm = lmer_nm)
      }
      try_warn <- try_obj[["warning"]]
      warn_num <- setNames(ifelse(!(is.null(try_warn)), yes = 1, no = 0),
         nm = "lmer_warn")
      try_err <- try_obj[["error"]]
      err_num <- setNames(ifelse(!(is.null(try_err)), yes = 1, no = 0),
         nm = "lmer_err")
      all_num <- str2str::v2d(c(coef_rtn, lmer_rtn, warn_num, err_num),
         along = 1) # create data.frame of all numeric values first
      warn_mssg <- setNames(ifelse(!(is.null(try_warn)), yes = try_warn, no = NA_character_),
         nm = "warn_mssg")
      err_mssg <- setNames(ifelse(!(is.null(try_err)), yes = try_err, no = NA_character_),
         nm = "err_mssg")
      all_mssg <- str2str::v2d(c(warn_mssg, err_mssg),
         along = 1) # create data.frame of all character values second
      all_rtn <- cbind.data.frame(all_num, all_mssg)
      return(all_rtn)
   })

   # return object
   info_dfm <- str2str::ld2d(ld = info_by, along = 1,
      rtn.listnames.nm = NULL, rtn.rownames.nm = NULL)
   grp_dfm <- setNames(expand.grid(grp_dimnames, stringsAsFactors = FALSE),
      nm = grp.nm)
   rtn <- cbind(grp_dfm, info_dfm) # cbind.data.frame
   return(rtn)
}
