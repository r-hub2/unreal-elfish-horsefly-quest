# MIA ####

#' @include quest_functions.R
NULL

# amd_uni #

#' Amount of Missing Data - Univariate
#'
#' \code{amd_uni} by default computes the proportion of missing data for
#' variables in a data.frame, with arguments to allow for counts instead of
#' proportions (i.e., \code{prop}) or observed data rather than missing data
#' (i.e., \code{ov}). It is univariate in that each variable is treated in
#' isolation. \code{amd_uni} is a simple wrapper for \code{\link{colNA}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of the colnames from \code{data} specifying
#'   the variables.
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return numeric vector of length = \code{length(vrb.nm)} and names =
#'   \code{vrb.nm} providing the frequency of missing (or observed if \code{ov}
#'   = TRUE) values per variable. If \code{prop} = TRUE, the values will range
#'   from 0 to 1. If \code{prop} = FALSE, the values will range from 0 to
#'   \code{nrow(data)}.
#'
#' @seealso
#'    \code{\link{amd_bi}}
#'    \code{\link{amd_multi}}
#'
#' @examples
#'
#' amd_uni(data = airquality, vrb.nm = names(airquality)) # proportion of missing data
#' amd_uni(data = airquality, vrb.nm = names(airquality),
#'    ov = TRUE) # proportion of observed data
#' amd_uni(data = airquality, vrb.nm = names(airquality),
#'    prop = FALSE) # count of missing data
#' amd_uni(data = airquality, vrb.nm = names(airquality),
#'    prop = FALSE, ov = TRUE) # count of observed data
#'
#' @export
amd_uni <- function(data, vrb.nm, prop = TRUE, ov = FALSE) {
   colNA(x = as.matrix(data[vrb.nm]), prop = prop, ov = ov)
}

# amd_bi #

#' Amount of Missing Data - Bivariate (Pairwise Deletion)
#'
#' \code{amd_bi} by default computes the proportion of missing data for pairs of
#' variables in a data.frame, with arguments to allow for counts instead of
#' proportions (i.e., \code{prop}) or observed data rather than missing data
#' (i.e., \code{ov}). It is bivariate in that each pair of variables is treated
#' in isolation.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of the colnames from \code{data} specifying
#'   the variables.
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return data.frame of nrow = ncol = \code{length(vrb.nm)} and rowames =
#'   colnames = \code{vrb.nm} providing the frequency of missing (or observed if
#'   \code{ov} = TRUE) values per pair of variables. If \code{prop} = TRUE, the
#'   values will range from 0 to 1. If \code{prop} = FALSE, the values will
#'   range from 0 to \code{nrow(data)}.
#'
#' @seealso
#'    \code{\link{amd_bi}}
#'    \code{\link{amd_multi}}
#'
#' @examples
#'
#' amd_bi(data = airquality, vrb.nm = names(airquality)) # proportion of missing data
#' amd_bi(data = airquality, vrb.nm = names(airquality),
#'    ov = TRUE) # proportion of observed data
#' amd_bi(data = airquality, vrb.nm = names(airquality),
#'    prop = FALSE) # count of missing data
#' amd_bi(data = airquality, vrb.nm = names(airquality),
#'    prop = FALSE, ov = TRUE) # count of observed data
#'
#' @export
amd_bi <- function(data, vrb.nm, prop = TRUE, ov = FALSE) {

   data_vrb <- data[vrb.nm]
   n <- t(!(is.na(data_vrb))) %*% !(is.na(data_vrb)) # from psych::corr.test() - don't add as.numeric() b/c converts to atomic vector
   # have to use !() and then deal with ov = FALSE b/c 0 times any number is 0!
   N <- nrow(data)
   if (!ov) n <- N - n
   if (prop)
      tmp <- n / N
   else
      tmp <- n
   rtn <- as.data.frame(tmp)
   return(rtn)
}

# amd_multi #

#' Amount of Missing Data - Multivariate (Listwise Deletion)
#'
#' \code{amd_multi} by default computes the proportion of missing data from
#' listwise deletion for a set of variables in a data.frame, with arguments to
#' allow for counts instead of proportions (i.e., \code{prop}) or observed data
#' rather than missing data (i.e., \code{ov}). It is multivariate in that the
#' variables are treated together as a set.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of the colnames from \code{data} specifying
#'   the variables.
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return numeric vector of length 1 providing the frequency of missing (or
#'   observed if \code{ov} = TRUE) rows from listwise deletion for the set of
#'   variables \code{vrb.nm}. If \code{prop} = TRUE, the value will range from 0
#'   to 1. If \code{prop} = FALSE, the value will range from 0 to
#'   \code{nrow(data)}.
#'
#' @seealso
#'    \code{\link{amd_uni}}
#'    \code{\link{amd_bi}}
#'
#' @examples
#'
#' amd_multi(airquality, vrb.nm = names(airquality)) # proportion of missing data
#' amd_multi(airquality, vrb.nm = names(airquality),
#'    ov = TRUE) # proportion of observed data
#' amd_multi(airquality, vrb.nm = names(airquality),
#'    prop = FALSE) # count of missing data
#' amd_multi(airquality, vrb.nm = names(airquality),
#'    prop = FALSE, ov = TRUE) # count of observed data
#'
#' @export
amd_multi <- function(data, vrb.nm, prop = TRUE, ov = FALSE) {

   n <- sum(complete.cases(data[vrb.nm]))
   N <- nrow(data)
   if (!ov) n <- N - n
   if (prop)
      rtn <- n / N
   else
      rtn <- n
   return(rtn)
}
