# QUEST ####

#' Pre-processing Questionnaire Data
#'
#' @description \code{quest} is a package for pre-processing questionnaire data
#'   to get it ready for statistical modeling. It contains functions for
#'   investigating missing data (e.g., \code{\link{rowNA}}), reshaping data
#'   (e.g., \code{\link{wide2long}}), validating responses (e.g.,
#'   \code{\link{revalids}}), recoding variables (e.g., \code{\link{recodes}}),
#'   scoring (e.g., \code{\link{scores}}), centering (e.g.,
#'   \code{\link{centers}}), aggregating (e.g., \code{\link{aggs}}), shifting
#'   (e.g., \code{\link{shifts}}), etc. Functions whose first phrases end with
#'   an \code{s} are vectorized versions of their functions without an \code{s}
#'   at the end of the first phrase. For example, \code{center} inputs a
#'   (atomic) vector and outputs a atomic vector to center and/or scale a single
#'   variable; \code{centers} inputs a data.frame and outputs a data.frame to
#'   center and/or scale multiple variables. Functions that end in \code{_by}
#'   are calculated by group. For example, \code{center} does grand-mean
#'   centering while \code{center_by} does group-mean centering. Putting the two
#'   together, \code{centers_by} inputs a data.frame and outputs a data.frame to
#'   center and/or scale multiple variables by group. Functions that end in
#'   \code{_ml} calculate a "multilevel" result with a within-group result and
#'   between-group result. Functions that end in \code{_if} are calculated
#'   dependent on the frequency of observed values (aka amount of missing data).
#'   The \code{quest} package uses the \code{str2str} package internally to
#'   convert R objects from one structure to another. See
#'   \code{\link[str2str]{str2str}} for details.
#'
#' @section Types of functions: There are three main types of functions. 1)
#'   Helper functions that primarily exist to save a few lines of code and are
#'   primarily for convenience (e.g., \code{\link{vecNA}}). 2) Functions for
#'   wrangling questionnaire data (e.g., \code{\link{nom2dum}},
#'   \code{\link{reverses}}). 3) Functions for preliminary statistical
#'   calculation (e.g., \code{\link{means_diff}}, \code{\link{corp_by}}).
#'
#' @section Abbreviations: See the table below
#'
#' \describe{
#'    \item{vrb}{variable}
#'    \item{grp}{group}
#'    \item{nm}{names}
#'    \item{NA}{missing values}
#'    \item{ov}{observed values}
#'    \item{prop}{proportion}
#'    \item{sep}{separator}
#'    \item{cor}{correlations}
#'    \item{id}{identifier}
#'    \item{rtn}{return}
#'    \item{fun}{function}
#'    \item{dfm}{data.frame}
#'    \item{fct}{factor}
#'    \item{nom}{nominal variable}
#'    \item{bin}{binary variable}
#'    \item{dum}{dummy variable}
#'    \item{pomp}{percentage of maximum possible}
#'    \item{std}{standardize}
#'    \item{wth}{within-groups}
#'    \item{btw}{between-groups}
#' }
#'
#' @import datasets stats utils methods
#'
"_PACKAGE"

# MISC ####

# by2 #

#' Apply a Function to Data by Group
#'
#' \code{by2} applies a function to data by group and is an alternative to the
#' base R function \code{\link{by}}. The function is apart of the
#' split-apply-combine type of function discussed in the \code{plyr} R package
#' and is very similar to \code{\link[plyr]{dlply}}. It splits up one data.frame
#' \code{.data[.vrb.nm]}into a data.frame for each group in
#' \code{.data[.grp.nm]}, applies a function \code{.fun} to each data.frame, and
#' then returns the results as a list with names equal to the group values
#' \code{unique(interaction(.data[.grp.nm], sep = .sep))}. \code{by2} is simply
#' \code{split.data.frame} + \code{lapply}. Similar to \code{dlply}, The
#' arguments all start with \code{.} so that they do not conflict with arguments
#' from the function \code{.fun}. If you want to apply a function a (atomic)
#' vector rather than data.frame, then use \code{\link{tapply2}}.
#'
#' @param .data data.frame of data.
#'
#' @param .vrb.nm character vector specifying the colnames of \code{.data} to
#'   select the set of variables to apply \code{.fun} to.
#'
#' @param .grp.nm character vector specifying the colnames of \code{.data} to
#'   select the grouping variables.
#'
#' @param .sep character vector of length 1 specifying the string to combine the
#'   group values together with. \code{.sep} is only used if there are multiple
#'   grouping variables (i.e., \code{length(.grp.nm)} > 1).
#'
#' @param .fun function to apply to the set of variables \code{.data[.vrb.nm]}
#'   for each group.
#'
#' @param ... additional named arguments to pass to \code{.fun}.
#'
#' @return list of objects containing the return object of \code{.fun} for each
#'   group. The names are the unique combinations of the grouping variables
#'   (i.e., \code{unique(interaction(.data[.grp.nm], sep = .sep))}).
#'
#' @seealso
#'    \code{\link{by}}
#'    \code{\link{tapply2}}
#'    \code{\link[plyr]{dlply}}
#'
#' @examples
#'
#' # one grouping variable
#' by2(mtcars, .vrb.nm = c("mpg","cyl","disp"), .grp.nm = "vs",
#'    .fun = cov, use = "complete.obs")
#'
#' # two grouping variables
#' x <- by2(mtcars, .vrb.nm = c("mpg","cyl","disp"), .grp.nm = c("vs","am"),
#'    .fun = cov, use = "complete.obs")
#' print(x)
#' str(x)
#'
#' # compare to by
#' vrb_nm <- c("mpg","cyl","disp") # Roxygen runs the whole script if I put a c() in a []
#' grp_nm <- c("vs","am") # Roxygen runs the whole script if I put a c() in a []
#' y <- by(mtcars[vrb_nm], INDICES = mtcars[grp_nm],
#'    FUN = cov, use = "complete.obs", simplify = FALSE)
#' str(y) # has dimnames rather than names
#' @export
by2 <- function(.data, .vrb.nm, .grp.nm, .sep = ".", .fun, ...) {

   lapply(X = split.data.frame(x = .data[.vrb.nm], f = .data[.grp.nm], sep = .sep),
      FUN = .fun, ...)
}

# tapply2 #

#' Apply a Function to a (Atomic) Vector by Group
#'
#' \code{tapply2} applies a function to a (atomic) vector by group and is an
#' alternative to the base R function \code{\link{tapply}}. The function is
#' apart of the split-apply-combine type of function discussed in the
#' \code{plyr} R package and is somewhat similar to \code{\link[plyr]{dlply}}.
#' It splits up one (atomic) vector \code{.x}into a (atomic) vector for each
#' group in \code{.grp}, applies a function \code{.fun} to each (atomic) vector,
#' and then returns the results as a list with names equal to the group values
#' \code{unique(interaction(.grp.nm, sep = .sep))}. \code{tapply2} is simply
#' \code{split.default} + \code{lapply}. Similar to \code{dlply}, The arguments
#' all start with \code{.} so that they do not conflict with arguments from the
#' function \code{.fun}. If you want to apply a function a data.frame rather
#' than a (atomic) vector, then use \code{\link{by2}}.
#'
#' @param .x atomic vector
#'
#' @param .grp list of atomic vector(s) and/or factor(s) (e.g., data.frame)
#'   containing the groups. They should each have same length as \code{.x}. It
#'   can also be an atomic vector or factor, which will then be made the first
#'   element of a list internally.
#'
#' @param .sep character vector of length 1 specifying the string to combine the
#'   group values together with. \code{.sep} is only used if there are multiple
#'   grouping variables (i.e., \code{.grp} is a list with multiple elements).
#'
#' @param .fun function to apply to \code{.x} for each group.
#'
#' @param ... additional named arguments to pass to \code{.fun}.
#'
#' @return list of objects containing the return object of \code{.fun} for each
#'   group. The names are the unique combinations of the grouping variables
#'   (i.e., \code{unique(interaction(.grp, sep = .sep))}).
#'
#' @seealso
#'    \code{\link{tapply}}
#'    \code{\link{by2}}
#'    \code{\link[plyr]{dlply}}
#'
#' @examples
#'
#' # one grouping variable
#' tapply2(mtcars$"cyl", .grp = mtcars$"vs", .fun = median, na.rm = TRUE)
#'
#' # two grouping variables
#' grp_nm <- c("vs","am") # Roxygen runs the whole script if I put a c() in a []
#' x <- tapply2(mtcars$"cyl", .grp = mtcars[grp_nm], .fun = median, na.rm = TRUE)
#' print(x)
#' str(x)
#'
#' # compare to tapply
#' grp_nm <- c("vs","am") # Roxygen runs the whole script if I put a c() in a []
#' y <- tapply(mtcars$"cyl", INDEX = mtcars[grp_nm],
#'    FUN = median, na.rm = TRUE, simplify = FALSE)
#' print(y)
#' str(y) # has dimnames rather than names
#' @export
tapply2 <- function(.x, .grp, .sep = ".", .fun, ...) {

   lapply(X = split.default(x = .x, f = .grp, sep = .sep),
      FUN = .fun, ...)
}

# wide2long #

#' Reshape Multiple Sets of Variables From Wide to Long
#'
#' \code{wide2long} reshapes data from wide to long. This if often necessary to
#' do with multilevel data where multiple sets of variables in the wide format
#' seek to be reshaped to multiple rows in the long format. If only one set of
#' variables needs to be reshaped, then you can use
#' \code{\link[str2str]{stack2}} or \code{\link[reshape]{melt.data.frame}} - but that
#' does not work for *multiple* sets of variables. See details for more
#' information.
#'
#' \code{wide2long} uses \code{reshape(direction = "long")} to reshape the data.
#' It attempts to streamline the task of reshaping wide to long as the
#' \code{reshape} arguments can be confusing because the same arguments are used
#' for wide vs. long reshaping. See \code{\link[stats]{reshape}} if you are
#' curious.
#'
#' IF \code{vrb.nm.list} IS A LIST OF CHARACTER VECTORS: The conventional use of
#' \code{vrb.nm.list} is to provide a list of character vectors, which specify
#' each set of variables to be reshaped. For example, if \code{data} contains
#' data from a longitudinal panel study with the same scores at different waves,
#' then there might be a column for each score at each wave. \code{vrb.nm.list}
#' would then contain an element for each score with each element containing a
#' character vector of the colnames for that score at each wave (see examples).
#' The names of the list elements would then be the colnames in the return
#' object for those scores.
#'
#' IF \code{vrb.nm.list} IS A CHARACTER VECTOR: The advanced use of
#' \code{vrb.nm.list} is to provide a single character vector, which specify the
#' variables to be reshaped (not organized by sets). In this case (i.e., if
#' \code{vrb.nm.list} is not a list), then \code{wide2long} (really
#' \code{\link[stats]{reshape}}) will attempt to guess which colnames go
#' together as a set. It is assumed the following column naming scheme has been
#' used: 1) have the same name prefix for columns within a set, 2) have the same
#' number suffixes for each set of columns, 3) use, *and only use*, \code{sep}
#' in the colnames to separate the name prefix and the number suffix. For
#' example, the name prefixes might be "predictor" and "outcome" while the
#' number suffixes might be "0", "1", and "2", and the separator might be ".",
#' resulting in column names such as "outcome.1". The name prefix could include
#' separators other than \code{sep} (e.g., "outcome_item.1"), but it cannot
#' include \code{sep} (e.g., "outcome.item.1"). So "outcome_item1.1" could be
#' acceptable, but "outcome.item1.1" would not.
#'
#' @param data data.frame of multilevel data in the wide format.
#'
#' @param vrb.nm.list A unique argument for the \code{quest} package such that
#'   it can take on different types of inputs. The conventional use is to
#'   provide a list of character vectors specifying each set of colnames to be
#'   reshaped. In longitudinal panel data, each list element would contain a
#'   score with multiple timepoints. The advanced use is to provide a single
#'   character vector specifying the colnames to be reshaped (not organized by
#'   sets). See details.
#'
#' @param grp.nm character vector specifying the colnames in \code{data}
#'   corresponding to the groups. Because \code{data} is in the wide format,
#'   \code{data[grp.nm]} must have unique rows (aka groups); if this is not the
#'   case, an error is returned. \code{grp.nm} can be NULL, in which case the
#'   rownames of \code{data} will be used. In longitudinal panel data this
#'   variable would be the participant ID variable.
#'
#' @param sep character vector of length 1 specifying the string in the column
#'   names provided by \code{vrb.nm.list} that separates out the name prefix
#'   from the number suffix. If \code{sep} = "", then that implies there is no
#'   string separating the name prefix and the number suffix (e.g., "outcome1").
#'
#' @param rtn.obs.nm character vector of length 1 specifying the new colname in
#'   the return object indicating which observation within each group the row
#'   refers to. In longitudinal panel data, this would be the returned time
#'   variable.
#'
#' @param order.by.grp logical vector of length 1 specifying whether to sort the
#'   return object first by \code{grp.nm} and then \code{obs.nm} (TRUE) or by
#'   \code{obs.nm} and then \code{grp.nm} (FALSE).
#'
#' @param keep.attr logical vector of length 1 specifying whether to keep the
#'   "reshapeLong" attribute (from \code{\link[stats]{reshape}}) in the return
#'   object.
#'
#' @return data.frame with nrow equal to \code{nrow(data) *
#'   length(vrb.nm.list[[1]])} if \code{vrb.nm.list} is a list (i.e.,
#'   conventional use) or \code{nrow(data)} * number of unique number suffixes
#'   in \code{vrb.nm.list} if \code{vrb.nm.list} is not a list (i.e., advanced
#'   use). The columns will be in the following order: 1) \code{grp.nm} of the
#'   groups, 2) \code{rtn.obs.nm} of the observation labels, 3) the reshaped
#'   columns, 4) the additional columns that were not reshaped and instead
#'   repeated. How the returned data.frame is sorted depends on
#'   \code{order.by.grp}.
#'
#' @seealso
#'    \code{\link{long2wide}}
#'    \code{\link[stats]{reshape}}
#'    \code{\link[str2str]{stack2}}
#'
#' @examples
#'
#' # SINGLE GROUPING VARIABLE
#' dat_wide <- data.frame(
#'    x_1.1 = runif(5L),
#'    x_2.1 = runif(5L),
#'    x_3.1 = runif(5L),
#'    x_4.1 = runif(5L),
#'    x_1.2 = runif(5L),
#'    x_2.2 = runif(5L),
#'    x_3.2 = runif(5L),
#'    x_4.2 = runif(5L),
#'    x_1.3 = runif(5L),
#'    x_2.3 = runif(5L),
#'    x_3.3 = runif(5L),
#'    x_4.3 = runif(5L),
#'    y_1.1 = runif(5L),
#'    y_2.1 = runif(5L),
#'    y_1.2 = runif(5L),
#'    y_2.2 = runif(5L),
#'    y_1.3 = runif(5L),
#'    y_2.3 = runif(5L))
#' row.names(dat_wide) <- letters[1:5]
#' print(dat_wide)
#'
#' # vrb.nm.list = list of character vectors (conventional use)
#' vrb_pat <- c("x_1","x_2","x_3","x_4","y_1","y_2")
#' vrb_nm_list <- lapply(X = setNames(vrb_pat, nm = vrb_pat), FUN = function(pat) {
#'    str2str::pick(x = names(dat_wide), val = pat, pat = TRUE)})
#' # without `grp.nm`
#' z1 <- wide2long(dat_wide, vrb.nm = vrb_nm_list)
#' # with `grp.nm`
#' dat_wide$"ID" <- letters[1:5]
#' z2 <- wide2long(dat_wide, vrb.nm = vrb_nm_list, grp.nm = "ID")
#' dat_wide$"ID" <- NULL
#'
#' # vrb.nm.list = character vector + guessing (advanced use)
#' vrb_nm <- str2str::pick(x = names(dat_wide), val = "ID", not = TRUE)
#' # without `grp.nm`
#' z3 <- wide2long(dat_wide, vrb.nm.list = vrb_nm)
#' # with `grp.nm`
#' dat_wide$"ID" <- letters[1:5]
#' z4 <- wide2long(dat_wide, vrb.nm = vrb_nm, grp.nm = "ID")
#' dat_wide$"ID" <- NULL
#'
#' # comparisons
#' head(z1); head(z3); head(z2); head(z4)
#' all.equal(z1, z3)
#' all.equal(z2, z4)
#' # keeping the reshapeLong attributes
#' z7 <- wide2long(dat_wide, vrb.nm = vrb_nm_list, keep.attr = TRUE)
#' attributes(z7)
#'
#' # MULTIPLE GROUPING VARIABLES
#' bfi2 <- psych::bfi
#' bfi2$"person" <- unlist(lapply(X = 1:400, FUN = rep.int, times = 7))
#' bfi2$"day" <- rep.int(1:7, times = 400L)
#' head(bfi2, n = 15)
#'
#' # vrb.nm.list = list of character vectors (conventional use)
#' vrb_pat <- c("A","C","E","N","O")
#' vrb_nm_list <- lapply(X = setNames(vrb_pat, nm = vrb_pat), FUN = function(pat) {
#'    str2str::pick(x = names(bfi2), val = pat, pat = TRUE)})
#' z5 <- wide2long(bfi2, vrb.nm.list = vrb_nm_list, grp = c("person","day"),
#'    rtn.obs.nm = "item")
#'
#' # vrb.nm.list = character vector + guessing (advanced use)
#' vrb_nm <- str2str::pick(x = names(bfi2),
#'    val = c("person","day","gender","education","age"), not = TRUE)
#' z6 <- wide2long(bfi2, vrb.nm.list = vrb_nm, grp = c("person","day"),
#'    sep = "", rtn.obs.nm = "item") # need sep = "" because no character separating
#'    # scale name and item number
#' all.equal(z5, z6)
#'
#' @export
wide2long <- function(data, vrb.nm.list, grp.nm = NULL, sep = ".", rtn.obs.nm = "obs",
   order.by.grp = TRUE, keep.attr = FALSE) {

   if (is.null(grp.nm)) {
      data[["Row.names"]] <- row.names(data)
      grp.nm <- "Row.names"
   }
   if (length(grp.nm) > 1)
      grp.el <- do.call(what = `interaction`, args = data[grp.nm])
   else
      grp.el <- data[[grp.nm]]
   if (any(duplicated(grp.el))) # duplicated.default
      stop("`data`[`grp.nm`] must have all unique values")
   if (is.list(vrb.nm.list)) {
      rtn <- reshape(data = data, varying = vrb.nm.list, v.names = names(vrb.nm.list),
         direction = "long", sep = sep, idvar = grp.nm, timevar = rtn.obs.nm)
   } else {
      rtn <- reshape(data = data, varying = vrb.nm.list, # error if explicitly call v.names = NULL
         direction = "long", sep = sep, idvar = grp.nm, timevar = rtn.obs.nm)
   }
   reshapeLong_attr <- attr(rtn, which = "reshapeLong")
   keep_nm <- str2str::pick(x = names(data), val = c(grp.nm, unlist(vrb.nm.list)), not = TRUE)
   reshaped_nm <- str2str::pick(x = names(rtn), val = c(grp.nm, rtn.obs.nm, keep_nm), not = TRUE)
   rtn <- rtn[c(grp.nm, rtn.obs.nm, reshaped_nm, keep_nm)]
   if (order.by.grp) rtn <- rtn[do.call(what = `order`, args = rtn[c(grp.nm, rtn.obs.nm)]), ]
   if (keep.attr) attr(rtn, "reshapeLong") <- reshapeLong_attr
   row.names(rtn) <- seq.int(from = 1L, to = nrow(rtn), by = 1L)
   return(rtn)
}

# long2wide

#' Reshape Multiple Scores From Long to Wide
#'
#' \code{long2wide} reshapes data from long to wide. This if often necessary to
#' do with multilevel data where variables in the long format seek to be
#' reshaped to multiple sets of variables in the wide format. If only one column
#' needs to be reshaped, then you can use \code{\link[str2str]{unstack2}} or
#' \code{\link[reshape]{cast}} - but that does not work for *multiple* columns.
#'
#' \code{long2wide} uses \code{reshape(direction = "wide")} to reshape the data.
#' It attempts to streamline the task of reshaping long to wide as the
#' \code{reshape} arguments can be confusing because the same arguments are used
#' for wide vs. long reshaping. See \code{\link[stats]{reshape}} if you are
#' curious.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables to be reshaped. In longitudinal panel data, this would be the
#'   scores.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups. In longitudnal panel data, this would be the participant ID
#'   variable.
#'
#' @param obs.nm character vector of length 1 with a colname from \code{data}
#'   specifying the observation within each group. In longitudinal panel data,
#'   this would be the time variable.
#'
#' @param sep character vector of length 1 specifying the string that separates
#'   the name prefix (e.g., score) from it's number suffix (e.g., timepoint). If
#'   \code{sep} = "", then that implies there is no string separating the name
#'   prefix and the number suffix (e.g., "outcome1").
#'
#' @param colnames.by.obs logical vector of length 1 specifying whether to sort
#'   the return object colnames by the observation label (TRUE) or by the order
#'   of \code{vrb.nm}. See the example at the end of the "MULTIPLE GROUPING
#'   VARIABLES" section of the examples.
#'
#' @param keep.attr logical vector of length 1 specifying whether to keep the
#'   "reshapeWide" attribute (from \code{reshape}) in the return object.
#'
#' @seealso
#'    \code{\link{wide2long}}
#'    \code{\link[stats]{reshape}}
#'    \code{\link[str2str]{unstack2}}
#'
#' @return data.frame with nrow equal to \code{nrow(unique(data[grp.nm]))} and
#'   number of reshaped columns equal to \code{length(vrb.nm) *
#'   unique(data[[obs.nm]])}. The colnames will have the structure
#'   \code{paste0(vrb.nm, sep, unique(data[[obs.nm]]))}. The reshaped colnames
#'   are sorted by the observation labels if \code{colnames.by.obs} = TRUE and
#'   sorted by \code{vrb.nm} if \code{colnames.by.obs} = FALSE. Overall, the
#'   columns are in the following order: 1) \code{grp.nm} of the groups, 2)
#'   reshaped columns, 3) additional columns that were not reshaped.
#'
#' @examples
#'
#' # SINGLE GROUPING VARIABLE
#' dat_long <- as.data.frame(ChickWeight) # b/c groupedData class does weird things...
#' w1 <- long2wide(data = dat_long, vrb.nm = "weight", grp.nm = "Chick",
#'    obs.nm = "Time") # NAs inserted for missing observations in some groups
#' w2 <- long2wide(data = dat_long, vrb.nm = "weight", grp.nm = "Chick",
#'    obs.nm = "Time", sep = "_")
#' head(w1); head(w2)
#' w3 <- long2wide(data = dat_long, vrb.nm = "weight", grp.nm = "Chick",
#'    obs.nm = "Time", sep = "_T", keep.attr = TRUE)
#' attributes(w3)
#'
#' # MULTIPLE GROUPING VARIABLE
#' tmp <- psychTools::sai
#' grps <- interaction(tmp[1:3], drop = TRUE)
#' dups <- duplicated(grps)
#' dat_long <- tmp[!(dups), ] # for some reason there are duplicate groups in the data
#' vrb_nm <- str2str::pick(names(dat_long), val = c("study","time","id"), not = TRUE)
#' w4 <- long2wide(data = dat_long, vrb.nm = vrb_nm, grp.nm = c("study","id"),
#'    obs.nm = "time")
#' w5 <- long2wide(data = dat_long, vrb.nm = vrb_nm, grp.nm = c("study","id"),
#'    obs.nm = "time", colnames.by.obs = FALSE) # colnames sorted by `vrb.nm` instead
#' head(w4); head(w5)
#'
#' @export
long2wide <- function(data, vrb.nm, grp.nm, obs.nm, sep = ".",
   colnames.by.obs = TRUE, keep.attr = FALSE) {

   combo.el <- do.call(what = `interaction`, args = data[c(grp.nm, obs.nm)])
   if (any(duplicated(combo.el))) # duplicated.default
      stop("Some groups have multiple rows with the same observation value")
   rtn <- reshape(data = data, v.names = vrb.nm, timevar = obs.nm, idvar = grp.nm,
      direction = "wide", sep = sep)
   reshapeWide_attr <- attr(rtn, which = "reshapeWide")
   keep_nm <- str2str::pick(x = names(data), val = c(vrb.nm, grp.nm, obs.nm), not = TRUE)
   reshaped_nm <- str2str::pick(x = names(rtn), val = c(grp.nm, keep_nm), not = TRUE)
   if (!colnames.by.obs) {
      tmp <- lapply(X = vrb.nm, FUN = function(vrb_nm)
         str2str::pick(reshaped_nm, val = paste0("^", vrb_nm, sep), pat = TRUE))
      reshaped_nm <- unlist(tmp)
   }
   rtn <- rtn[c(grp.nm, reshaped_nm, keep_nm)]
   if (keep.attr) attr(rtn, "reshapeWide") <- reshapeWide_attr
   return(rtn)
}

# renames #

#' Rename Data Columns from a Codebook
#'
#' \code{renames} renames columns in a data.frame from a codebook. The codebook is
#' assumed to be a list of data.frames containing the old and new column names.
#' See details for how the codebook should be structured. The idea is that the
#' codebook has been imported as an excel workbook with different sets of column
#' renaming information in different workbook sheets. This function is simply a wrapper
#' for \code{plyr::rename}.
#'
#' \code{codebook} is a data.frame or list of data.frames where one column
#' refers to the old names and another column refers to the new names.
#' Therefore, each row of the data.frame(s) refers to a column in \code{data}.
#' If a list of data.frames, the position or names of the columns in the
#' \code{codebook} data.frames that contain the old (i.e., \code{old}) and new
#' (i.e., \code{new}) \code{data} columns must be the same for each data.frame
#' in \code{codebook}.
#'
#' @param data data.frame of data.
#'
#' @param codebook list of data.frames containing the old and new column names.
#'
#' @param old numeric vector or character vector of length 1 specifying the
#' position or name of the column in the \code{codebook} data.frames that
#' contains the old column names present in \code{data}.
#'
#' @param new numeric vector or character vector of length 1 specifying the
#' position or name of the column in the \code{codebook} data.frames that
#' contains the new column names to rename to in \code{data}.
#'
#' @param warn_missing logical vector of length 1 specifying whether \code{renames}
#' should return a warning if any old names in \code{codebook} are not present in
#' \code{data}.
#'
#' @param warn_duplicated logical vector of length 1 specifying whether \code{renames}
#' should return a warning if the renaming process results in duplicate column names
#' in the return object.
#'
#' @return data.frame identical to \code{data} except that the old names in
#' \code{codebook} have been replaced by the new names in \code{codebook}.
#'
#' @seealso
#'    \code{\link[plyr]{rename}}
#'
#' @examples
#'
#' # list of data.frames
#' code_book <- list(
#'    data.frame("old" = c("rating","complaints"), "new" = c("RATING","COMPLAINTS")),
#'    data.frame("old" = c("privileges","learning"), "new" = c("PRIVILEGES","LEARNING"))
#' )
#' renames(data = attitude, codebook = code_book, old = "old", new = "new")
#'
#' # single data.frame
#' code_book <- data.frame("old" = c("rating","complaints"), "new" = c("RATING","COMPLAINTS"))
#' renames(data = attitude, codebook = code_book, old = "old", new = "new")
#'
#' @export
renames <- function(data, codebook, old = 1L, new = 2L,
   warn_missing = TRUE, warn_duplicated = TRUE) {
   if(is.data.frame(codebook))
      codebook <- list(codebook) # so data.frame input works with lapplys below
   tmp_old <- lapply(X = codebook, FUN = `[[`, i = old)
   tmp_new <- lapply(X = codebook, FUN = `[[`, i = new)
   old_names <- unlist(tmp_old)
   new_names <- unlist(tmp_new)
   rtn <- plyr::rename(x = data, replace = setNames(new_names, nm = old_names),
      warn_missing = warn_missing, warn_duplicated = warn_duplicated)
   return(rtn)
}

# MAKE ####

# make.product #

#' Make Product Terms (e.g., interactions)
#'
#' \code{make.product} creates product terms (i.e., interactions) from various
#' components. \code{make.product} uses \code{Center} for the optional of
#' centering and/or scaling the predictors and/or moderators before making the
#' product terms.
#'
#' @param data data.frame of data.
#'
#' @param x.nm character vector of colnames from \code{data} specifying the
#'   predictor columns.
#'
#' @param m.nm character vector of colnames from \code{data} specifying the
#'   moderator columns.
#'
#' @param center.x logical vector of length 1 specifying whether the predictor
#'   columns should be grand-mean centered before making the product terms.
#'
#' @param center.m logical vector of length 1 specifying whether the moderator
#'   columns should be grand-mean centered before making the product terms.
#'
#' @param scale.x logical vector of length 1 specifying whether the predictor
#'   columns should be grand-SD scaled before making the product terms.
#'
#' @param scale.m logical vector of length 1 specifying whether the moderator
#'   columns should be grand-SD scaled before making the product terms.
#'
#' @param suffix.x character vector of length 1 specifying any suffix to add to
#'   the end of the predictor colnames \code{x.nm} when creating the colnames of
#'   the return object.
#'
#' @param suffix.m character vector of length 1 specifying any suffix to add to
#'   the end of the moderator colnames \code{m.nm} when creating the colnames of
#'   the return object.
#'
#' @param sep character vector of length 1 specifying the string to connect
#'   \code{x.nm} and \code{m.nm} when specifying the colnames of the return
#'   object.
#'
#' @param combo logical vector of length 1 specifying whether all combinations
#'   of the predictors and moderators should be calculated or only those in
#'   parallel to each other (i.e., \code{x.nm[i]} and \code{m.nm[i]}). This
#'   argument is only applicable when multiple predictors AND multiple
#'   moderators are given.
#'
#' @return data.frame with product terms (e.g., interactions) as columns. The
#'   colnames are created by \code{paste(paste0(x.nm, suffix.x), paste0(m.nm,
#'   suffix.m), sep = sep)}.
#'
#' @examples
#' make.product(data = attitude, x.nm = c("complaints","privileges"),
#'    m.nm = "learning", center.x = TRUE, center.m = TRUE,
#'    suffix.x = "_c", suffix.m = "_c") # with grand-mean centering
#' make.product(data = attitude, x.nm = c("complaints","privileges"),
#'    m.nm = c("learning","raises"), combo = TRUE) # all possible combinations
#' make.product(data = attitude, x.nm = c("complaints","privileges"),
#'    m.nm = c("learning","raises"), combo = FALSE) # only combinations "in parallel"
#' @export
make.product <- function(data, x.nm, m.nm, center.x = FALSE, center.m = FALSE,
                         scale.x = FALSE, scale.m = FALSE, suffix.x = "", suffix.m = "",
                         sep = ":", combo = TRUE) {

   x_scaled <- centers(data = data, vrb.nm = x.nm, center = center.x, scale = scale.x,
                      suffix = suffix.x)
   m_scaled <- centers(data = data, vrb.nm = m.nm, center = center.m, scale = scale.m,
                      suffix = suffix.m)
   if (!combo) {
      product <- Map(x_vl = x_scaled, m_vl = m_scaled, # Map uses the recycling rule if lengths of `x.nm` and `m.nm` different
                     f = function(x_vl, m_vl) x_vl * m_vl)
      names(product) <- paste(names(x_scaled), names(m_scaled), sep = sep)
   }
   if (combo) {
      combo_names <- expand.grid("x" = names(x_scaled), "m" = names(m_scaled))
      product <- Map(x_nm = combo_names[["x"]], m_nm = combo_names[["m"]],
                     f = function(x_nm, m_nm) x_scaled[[x_nm]] * m_scaled[[m_nm]])
      names(product) <- paste(combo_names[["x"]], combo_names[["m"]], sep = sep)
   }
   output <- data.frame(product, row.names = row.names(data), check.names = FALSE)
   return(output)
}

# make.dummy #

#' Make Dummy Columns
#'
#' \code{make.dummy} creates dummy columns (i.e., dichotomous numeric vectors
#' coded 0 and 1) from logical conditions. If you want to make logical
#' conditions from columns of a data.frame, you will need to call the data.frame
#' and its columns explicitly as this function does not use non-standard
#' evaluation.
#'
#' @param ... logical conditions that evaluate to logical vectors of the same
#'   length. If the logical vectors are not the same length, an error is
#'   returned. The names of the arguments are the colnames in the return object.
#'   If unnamed, then default R data.frame naming is used, which can get ugly.
#'
#' @param rtn.lgl logical vector of length 1 specifying whether the dummy
#'   columns should be logical vectors (TRUE) rather than numeric vectors
#'   (FALSE).
#'
#' @return data.frame of dummy columns based on the logical conditions n
#'   \code{...}. If \code{rtn.lgl} = TRUE, then the columns are logical vectors.
#'   If \code{out.lgl} = FALSE, then the columns are numeric vectors where 0 =
#'   FALSE and 1 = TRUE. The colnames are the names of the arguments in
#'   \code{...}. If not specified, then default data.frame names are created
#'   from the logical conditions themselves (which can get ugly).
#'
#' @seealso
#'    \code{\link{make.dumNA}}
#'
#' @examples
#' make.dummy(attitude$"rating" > 50) # ugly colnames
#' make.dummy("rating_50plus" = attitude$"rating" > 50,
#'    "advance_50minus" = attitude$"advance" < 50)
#' make.dummy("rating_50plus" = attitude$"rating" > 50,
#'    "advance_50minus" = attitude$"advance" < 50, rtn.lgl = TRUE)
#' \dontrun{
#'    make.dummy("rating_50plus" = attitude$"rating" > 50,
#'       "mpg_20plus" = mtcars$"mpg" > 20)
#' }
#' @export
make.dummy <- function(..., rtn.lgl = FALSE) {

   dots_lst <- list(...)
   if (!rtn.lgl) dots_lst <- lapply(X = dots_lst, FUN = as.numeric)
   var_len <- var(lengths(dots_lst)) # will be NA is only one logical condition is given
   if (var_len != 0 && !(is.na(var_len)))
      stop("all logical conditions from `...` must evaluate to logical vectors of the same length")
   output <- data.frame(dots_lst, stringsAsFactors = FALSE)
   return(output)
}

# make.dumNA #

#' Make Dummy Columns For Missing Data.
#'
#' \code{make.dumNA} makes dummy columns (i.e., dichomotous numeric vectors
#' coded 0 and 1) for missing data. Each variable is treated in isolation.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param ov logical vector of length 1 specifying whether the dummy columns
#'   should be reverse coded such that missing values = 0/FALSE and observed
#'   values = 1/TRUE.
#'
#' @param rtn.lgl logical vector of length 1 specifying whether the dummy columns
#'   should be logical vectors (TRUE) rather than numeric vectors (FALSE).
#'
#' @param suffix character vector of length 1 specifying the string that should
#'   be appended to the end of the colnames in the return object.
#'
#' @return data.frame of numeric (logical if \code{rtn.lgl} = TRUE) columns
#'   where missing = 1 and observed = 0 (flipped if \code{ov} = TRUE) for each
#'   variable. The colnames are created by \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{make.dummy}}
#'
#' @examples
#' make.dumNA(data = airquality, vrb.nm = c("Ozone","Solar.R"))
#' make.dumNA(data = airquality, vrb.nm = c("Ozone","Solar.R"),
#'    rtn.lgl = TRUE) # logical vectors returned
#' make.dumNA(data = airquality, vrb.nm = c("Ozone","Solar.R"),
#'    ov = TRUE, suffix = "_o") # 1 = observed value
#' @export
make.dumNA <- function(data, vrb.nm, ov = FALSE, rtn.lgl = FALSE, suffix = "_m") {

   if (!rtn.lgl) fun_type <- match.fun(as.numeric)
   if (rtn.lgl) fun_type <- match.fun(identity)
   if (!ov) fun_code <- match.fun(identity)
   if (ov) fun_code <- match.fun(`!`)
   tmp_miss <- lapply(X = data[vrb.nm], FUN = function(x) fun_type(fun_code(is.na(x))))
   output <- data.frame(tmp_miss, stringsAsFactors = FALSE)
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# NA ####

# vecNA #

#' Frequency of Missing Values in a Vector
#'
#' \code{vecNA} computes the frequency of missing values in an atomic vector.
#' \code{vecNA} is essentially a wrapper for \code{sum} or \code{mean} +
#' \code{is.na} or \code{!is.na} and can be useful for functional programming
#' (e.g., \code{lapply(FUN = vecNA)}). It is also used by other functions in the
#' quest package related to missing values (e.g., \code{\link{mean_if}}).
#'
#' @param x atomic vector or list vector. If not a vector, it will be coerced to
#'   a vector via \code{\link{as.vector}}.
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return numeric vector of length 1 providing the frequency of missing values
#'   (or observed values if \code{ov} = TRUE). If \code{prop} = TRUE, the value
#'   will range from 0 to 1. If \code{prop} = FALSE, the value will range from 1
#'   to \code{length(x)}.
#'
#' @seealso
#'    \code{\link{is.na}}
#'    \code{\link{rowNA}}
#'    \code{\link{colNA}}
#'    \code{\link{rowsNA}}
#'
#' @examples
#' vecNA(airquality[[1]]) # count of missing values
#' vecNA(airquality[[1]], prop = TRUE) # proportion of missing values
#' vecNA(airquality[[1]], ov = TRUE) # count of observed values
#' vecNA(airquality[[1]], prop = TRUE, ov = TRUE) # proportion of observed values
#' @export
vecNA <- function(x, prop = FALSE, ov = FALSE) {

   if (!(str2str::is.avector(x) | is.vector(x))) # to allow for attributes
      vec <- as.vector(x) # methods depends on input
   else
      vec <- x
   if (!ov) fun.ov <- match.fun(identity)
   if (ov) fun.ov <- match.fun(`!`)
   if (!prop) fun.prop <- match.fun(sum)
   if (prop) fun.prop <- match.fun(mean)
   output <- fun.prop(fun.ov(is.na(vec)))
   return(output)
}

# rowNA #

#' Frequency of Missing Values by Row
#'
#' \code{rowNA} compute the frequency of missing values in a matrix by row. This
#' function essentially does \code{apply(X = x, MARGIN = 1, FUN = vecNA)}. It is
#' also used by other functions in the quest package related to missing values
#' (e.g., \code{\link{rowMeans_if}}).
#'
#' @param x matrix with any typeof. If not a matrix, it will be coerced to a
#'   matrix via \code{as.matrix}. The argument \code{rownames.force} is set to
#'   TRUE to allow for rownames to carry over for non-matrix objects (e.g.,
#'   data.frames).
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return numeric vector of length = \code{nrow(x)}, and names =
#'   \code{rownames(x)}, providing the frequency of missing values (or observed
#'   values if \code{ov} = TRUE) per row. If \code{prop} = TRUE, the
#'   values will range from 0 to 1. If \code{prop} = FALSE, the values will
#'   range from 1 to \code{ncol(x)}.
#'
#' @seealso
#'    \code{\link{is.na}}
#'    \code{\link{vecNA}}
#'    \code{\link{colNA}}
#'    \code{\link{rowsNA}}
#'
#' @examples
#' rowNA(as.matrix(airquality)) # count of missing values
#' rowNA(as.data.frame(airquality)) # with rownames
#' rowNA(as.matrix(airquality), prop = TRUE) # proportion of missing values
#' rowNA(as.matrix(airquality), ov = TRUE) # count of observed values
#' rowNA(as.data.frame(airquality), prop = TRUE, ov = TRUE) # proportion of observed values
#' @export
rowNA <- function(x, prop = FALSE, ov = FALSE) {
   if (!(is.matrix(x))) mat <- as.matrix(x, rownames.force = TRUE) # methods depends on input
   else mat <- x
   if (!ov) fun.ov <- match.fun(identity)
   if (ov) fun.ov <- match.fun(`!`)
   if (!prop) fun.prop <- match.fun(sum)
   if (prop) fun.prop <- match.fun(mean)
   funNA <- function(x) fun.prop(fun.ov(is.na(x)))
   output <- apply(X = mat, MARGIN = 1, FUN = funNA)
   return(output)
}

# colNA #

#' Frequency of Missing Values by Column
#'
#' \code{rowNA} compute the frequency of missing values in a matrix by column.
#' This function essentially does \code{apply(X = x, MARGIN = 2, FUN = vecNA)}.
#' It is also used by other functions in the quest package related to missing
#' values (e.g., \code{\link{colMeans_if}}).
#'
#' @param x matrix with any typeof. If not a matrix, it will be coerced to a
#'   matrix via \code{as.matrix}. The function allows for colnames to carry over
#'   for non-matrix objects (e.g., data.frames).
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return numeric vector of length = \code{ncol(x)}, and names =
#'   \code{colnames(x)} providing the frequency of missing values (or observed
#'   values if \code{ov} = TRUE) per column. If \code{prop} = TRUE, the values
#'   will range from 0 to 1. If \code{prop} = FALSE, the values will range from
#'   1 to \code{nrow(x)}.
#'
#' @seealso
#'    \code{\link{is.na}}
#'    \code{\link{vecNA}}
#'    \code{\link{rowNA}}
#'    \code{\link{rowsNA}}
#'
#' @examples
#' colNA(as.matrix(airquality)) # count of missing values
#' colNA(as.matrix(airquality), prop = TRUE) # proportion of missing values
#' colNA(as.matrix(airquality), ov = TRUE) # count of observed values
#' colNA(as.data.frame(airquality), prop = TRUE, ov = TRUE) # proportion of observed values
#' @export
colNA <- function(x, prop = FALSE, ov = FALSE) {
   if (!(is.matrix(x))) mat <- as.matrix(x) # methods depends on input
   else mat <- x
   if (!ov) fun.ov <- match.fun(identity)
   if (ov) fun.ov <- match.fun(`!`)
   if (!prop) fun.prop <- match.fun(sum)
   if (prop) fun.prop <- match.fun(mean)
   funNA <- function(x) fun.prop(fun.ov(is.na(x)))
   output <- apply(X = mat, MARGIN = 2, FUN = funNA)
   return(output)
}

# partial.cases #

#' Find Partial Cases
#'
#' \code{partial.cases} indicates which cases are at least partially observed,
#' given a specified frequency of observed values across a set of columns. This
#' function builds off \code{\link[stats]{complete.cases}}. While
#' \code{complete.cases} requires completely observed cases,
#' \code{partial.cases} allows the user to specify the frequency of columns
#' required to be observed. The default arguments are equal to
#' \code{complete.cases}.
#'
#' @param data data.frame or matrix of data.
#'
#' @param vrb.nm a character vector of colnames from \code{data} specifying the
#'   variables which will be used to determine the partially observed cases.
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
#' @return logical vector of length = \code{nrow(data)} with names =
#'   \code{rownames(data)} specifying if the frequency of observed values is
#'   greater than (or equal to, if \code{inclusive} = TRUE) \code{ov.min}.
#'
#' @seealso
#'    \code{\link[stats]{complete.cases}}
#'    \code{\link{rowNA}}
#'    \code{\link{ncases}}
#'
#' @examples
#' cases2keep <- partial.cases(data = airquality,
#'    vrb.nm = c("Ozone","Solar.R","Wind"), ov.min = .66)
#' airquality2 <- airquality[cases2keep, ] # all cases with 2/3 variables observed
#' cases2keep <- partial.cases(data = airquality,
#'    vrb.nm = c("Ozone","Solar.R","Wind"), ov.min = 1, prop = TRUE, inclusive = TRUE)
#' complete_cases <- complete.cases(airquality)
#' identical(x = unname(cases2keep),
#'    y = complete_cases) # partial.cases(ov.min = 1, prop = TRUE,
#'    # inclusive = TRUE) = complete.cases()
#' @export
partial.cases <- function(data, vrb.nm, ov.min = 1, prop = TRUE, inclusive = TRUE) {

   ov_byrow <- rowNA(x = data[vrb.nm], prop = prop, ov = TRUE)
   if (inclusive) `%fun%` <- `>=` # flipped sign from rowSums_if
   if (!inclusive) `%fun%` <- `>` # flipped sign from rowSums_if
   output <- ifelse(ov_byrow %fun% ov.min, yes = TRUE, no = FALSE)
   return(output)
}

# ncases #

#' Number of Cases in Data
#'
#' \code{ncases} counts how many cases in a data.frame there are that have
#' a specified frequency of observed values across a set of columns. This function
#' is similar to \code{nrow} and is essentially \code{partial.cases} + \code{sum}. The user
#' can have \code{ncases} return the number of complete cases by calling \code{ov.min = 1},
#' \code{prop = TRUE}, and \code{inclusive = TRUE} (the default).
#'
#' @param data data.frame or matrix of data.
#'
#' @param vrb.nm a character vector of colnames from \code{data} specifying the variables.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#' \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop} = FALSE,
#' then this is a integer between 0 and \code{length(vrb.nm)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min} should
#' refer to the proportion of observed values (TRUE) or the count of observed
#' values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the case should
#' be included if the frequency of observed values in a row is exactly equal to \code{ov.min}.
#'
#' @return integer vector of length 1 providing the nrow in \code{data} with the given amount of observed values.
#'
#' @seealso
#'    \code{\link{partial.cases}}
#'    \code{\link{nrow}}
#'
#' @examples
#' vrb_nm <- c("Ozone","Solar.R","Wind")
#' nrow(airquality[vrb_nm]) # number of cases regardless of missing data
#' sum(complete.cases(airquality[vrb_nm])) # number of complete cases
#' ncases(data = airquality, vrb.nm = c("Ozone","Solar.R","Wind"),
#'    ov.min = 2/3) # number of rows with at least 2 of the 3 variables observed
#' @export
ncases <- function(data, vrb.nm = names(data), ov.min = 1, prop = TRUE, inclusive = TRUE) {

   sum(partial.cases(data = data, vrb.nm = vrb.nm, ov.min = ov.min,
      prop = prop, inclusive = inclusive))
}

# rowsNA #

#' Frequency of Multiple Sets of Missing Values by Row
#'
#' \code{rowsNA} computes the frequency of missing values for multiple sets of
#' columns from a data.frame. The arguments \code{prop} and \code{ov} allow the
#' user to specify if they want to sum or mean the missing values as well as
#' compute the frequency of observed values rather than missing values. This
#' function is essentially a vectorized version of \code{rowNA} that inputs and
#' outputs a data.frame.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm.list list where each element is a character vector of colnames
#'   in \code{data} specifying the variables for that set of columns. The names
#'   of \code{vrb.nm.list} will be the colnames of the return object.
#'
#' @param prop logical vector of length 1 specifying whether the frequency of
#'   missing values should be returned as a proportion (TRUE) or a count
#'   (FALSE).
#'
#' @param ov logical vector of length 1 specifying whether the frequency of
#'   observed values (TRUE) should be returned rather than the frequency of
#'   missing values (FALSE).
#'
#' @return data.frame with the frequency of missing values (or observed values
#'   if \code{ov} = TRUE) for each set of variables. The names are specified by
#'   \code{names(vrb.nm.list)}; if \code{vrb.nm.list} does not have any names,
#'   then the first element from \code{vrb.nm.list[[i]]} is used.
#'
#' @seealso
#'    \code{\link{rowNA}}
#'    \code{\link{colNA}}
#'    \code{\link{vecNA}}
#'    \code{\link{is.na}}
#'
#' @examples
#' vrb_list <- lapply(X = c("O","C","E","A","N"), FUN = function(chr) {
#'    tmp <- grepl(pattern = chr, x = names(psych::bfi))
#'    names(psych::bfi)[tmp]
#' })
#' rowsNA(data = psych::bfi,
#'    vrb.nm.list = vrb_list) # names set to first elements in `vrb.nm.list`[[i]]
#' names(vrb_list) <- paste0(c("O","C","E","A","N"), "_m")
#' rowsNA(data = psych::bfi, vrb.nm.list = vrb_list) # names set to names(`vrb.nm.list`)
#' @export
rowsNA <- function(data, vrb.nm.list, prop = FALSE, ov = FALSE) {

   tmp_lst <- lapply(X = vrb.nm.list, FUN = function(nm) {
      data_mat <- as.matrix(x = data[nm], rownames.force = TRUE)
      row_na <- rowNA(x = data_mat, prop = prop, ov = ov)
      return(row_na)
   })
   output <- data.frame(tmp_lst, stringsAsFactors = FALSE)
   row.names(output) <- row.names(data)
   list_names <- names(vrb.nm.list)
   if (is.null(list_names))
      list_names <- as.character(lapply(X = vrb.nm.list, FUN = `[[`, i = 1L))
   names(output) <- list_names
   return(output)
}

# FREQ ####

# freq #

#' Univariate Frequency Table
#'
#' \code{freq} creates univariate frequency tables similar to \code{table}. It
#' differs from \code{table} by allowing for custom sorting by something other
#' than the alphanumerics of the unique values as well as returning an atomic
#' vector rather than a 1D-array.
#'
#' The name for the table element giving the frequency of missing values is
#' "(NA)". This is different from \code{table} where the name is
#' \code{NA_character_}. This change allows for the sorting of tables that
#' include missing values, as subsetting in R is not possible with
#' \code{NA_character_} names. In future versions of the package, this might
#' change as it should be possible to avoid this issue by subetting with a
#' logical vector or integer indices instead of names. However, it is convenient
#' to be able to subset the return object fully by names.
#'
#' @param x atomic vector or list vector. If not a vector, it will be coerced to
#'   a vector via \code{\link{as.vector}}.
#'
#' @param exclude unique values of \code{x} to exclude from the returned table.
#'   If NULL, then missing values are always included in the returned table. See
#'   \code{\link{table}} for documentation on the same argument.
#'
#' @param useNA character vector of length 1 specifying how to handle missing
#'   values (i.e., whether to include NA as an element in the returned table).
#'   There are three options: 1) "no" = don't include missing values in the
#'   table, 2) "ifany" = include missing values if there are any, 3) "always" =
#'   include missing values in the table, regardless of whether there are any or
#'   not. See \code{\link{table}} for documentation on the same argument.
#'
#' @param prop logical vector of length 1 specifying whether the returned table
#'   should include counts (FALSE) or proportions (TRUE). If NAs are excluded
#'   (e.g., useNA = "no" or exclude = c(NA, NaN)), then the proportions will be
#'   based on the number of observed elements.
#'
#' @param sort character vector of length 1 specifying how the returned table
#'   will be sorted. There are three options: 1) "frequency" = the frequency of
#'   the unique values in \code{x}, 2) "position" = the position when each
#'   unique value first appears in \code{x}, 3) "alphanum" = alphanumeric
#'   ordering of the unique values in \code{x} (the sorting used by
#'   \code{table}). When "frequency" is specified and there are ties, then the
#'   ties are sorted alphanumerically.
#'
#' @param decreasing logical vector of length 1 specifying whether the table
#'   should be sorted in decreasing (TRUE) or increasing (FALSE) order.
#'
#' @param na.last logical vector of length 1 specifying whether the table should
#'   have NAs last or in whatever position they end up at. This argument is only
#'   relevant if NAs exist in \code{x} and are included in the table (e.g.,
#'   useNA = "always" or exclude = NULL).
#'
#' @return numeric vector of frequencies as either counts (if \code{prop} =
#'   FALSE) or proportions (if \code{prop} = TRUE) with the unique values of
#'   \code{x} as names (missing values have name = "(NA)"). Note, this is
#'   different from \code{table}, which returns a 1D-array and has class
#'   "table".
#'
#' @seealso
#'    \code{\link{freqs}}
#'    \code{\link{freq_by}}
#'    \code{\link{freqs_by}}
#'    \code{\link{table}}
#'
#' @examples
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = FALSE,
#'    sort = "frequency", decreasing = TRUE, na.last = TRUE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = FALSE,
#'    sort = "frequency", decreasing = TRUE, na.last = FALSE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = TRUE,
#'    sort = "frequency", decreasing = FALSE, na.last = TRUE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = TRUE,
#'    sort = "frequency", decreasing = FALSE, na.last = FALSE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = FALSE,
#'    sort = "position", decreasing = TRUE, na.last = TRUE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = FALSE,
#'    sort = "position", decreasing = TRUE, na.last = FALSE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = TRUE,
#'    sort = "position", decreasing = FALSE, na.last = TRUE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = TRUE,
#'    sort = "position", decreasing = FALSE, na.last = FALSE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = FALSE,
#'    sort = "alphanum", decreasing = TRUE, na.last = TRUE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = FALSE,
#'    sort = "alphanum", decreasing = TRUE, na.last = FALSE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = TRUE,
#'    sort = "alphanum", decreasing = FALSE, na.last = TRUE)
#' freq(c(mtcars$"carb", NA, NA, mtcars$"gear"), prop = TRUE,
#'    sort = "alphanum", decreasing = FALSE, na.last = FALSE)
#' @export
freq <- function(x, exclude = if (useNA == "no") c(NA, NaN),
   useNA = "always", prop = FALSE, sort = "frequency", decreasing = TRUE,
   na.last = TRUE) {

   if (!(str2str::is.avector(x) | is.vector(x))) # to allow for attributes
      x <- as.vector(x) # methods depends on input
   useNA <- match.arg(arg = useNA, choices = c("no","ifany","always"))
   sort <- match.arg(arg = sort, choices = c("alphanum","frequency","position"))
   tmp <- table(x, exclude = exclude, useNA = useNA)
   tab <- setNames(as.vector(tmp), nm = dimnames(tmp)[[1]]) # get rid of the 1D-array
   if (prop) tab <- tab / sum(tab)
   names(tab)[is.na(names(tab))] <- "(NA)" # to deal with not being able to subset by names where there are NA names
   if (sort == "alphanum") output <- tab
   if (sort == "frequency") output <- sort.int(tab)
   if (sort == "position") {
      unique_x <- unique(x)
      unique_r <- ifelse(is.na(unique_x), yes = "(NA)", no = unique_x)
      yes = output <- tab[as.character(unique_r)] # cannot subset by names when there are NA names
   }
   if (decreasing) output <- rev(output)
   names_output <- names(output)
   if (any(names_output == "(NA)") && na.last) {
      ov_nm <- names_output[names_output != "(NA)"] # cannot subset by names when there are NA names
      output <- output[c(ov_nm, "(NA)")]
   }
   return(output)
}

# freq_by

#' Univariate Frequency Table By Group
#'
#' \code{tables_by} creates a frequency table for a set of variables in a
#' data.frame by group. Depending on \code{total}, frequencies for all the
#' variables together can be returned by group. The function probably makes the
#' most sense for sets of variables with similar unique values (e.g., items from
#' a questionnaire with similar response options).
#'
#' \code{tables_by} uses \code{plyr::rbind.fill} to combine the results from
#' \code{table} applied to each variable into a single data.frame for each
#' group. If a variable from \code{data[vrb.nm]} for each group does not have
#' values present in other variables from \code{data[vrb.nm]} for that group,
#' then the frequencies in the return object will be 0.
#'
#' The name for the table element giving the frequency of missing values is
#' "(NA)". This is different from \code{table} where the name is
#' \code{NA_character_}. This change allows for the sorting of tables that
#' include missing values, as subsetting in R is not possible with
#' \code{NA_character_} names. In future versions of the package, this might
#' change as it should be possible to avoid this issue by subetting with a
#' logical vector or integer indices instead of names. However, it is convenient
#' to be able to subset the return object fully by names.
#'
#' @param x atomic vector.
#'
#' @param grp atomic vector or list of atomic vectors (e.g., data.frame)
#'   specifying the groups. The atomic vector(s) must be the length of \code{x}
#'   or else an error is returned.
#'
#' @param exclude unique values of \code{x} to exclude from the returned table.
#'   If NULL, then missing values are always included in the returned table. See
#'   \code{\link{table}} for documentation on the same argument.
#'
#' @param useNA character vector of length 1 specifying how to handle missing
#'   values (i.e., whether to include NA as an element in the returned table).
#'   There are three options: 1) "no" = don't include missing values in the
#'   table, 2) "ifany" = include missing values if there are any, 3) "always" =
#'   include missing values in the table, regardless of whether there are any or
#'   not. See \code{\link{table}} for documentation on the same argument.
#'
#' @param prop logical vector of length 1 specifying whether the returned table
#'   should include counts (FALSE) or proportions (TRUE). If NAs are excluded
#'   (e.g., useNA = "no" or exclude = c(NA, NaN)), then the proportions will be
#'   based on the number of observed elements.
#'
#' @param sort character vector of length 1 specifying how the returned table
#'   will be sorted. There are three options: 1) "frequency" = the frequency of
#'   the unique values in \code{x}, 2) "position" = the position when each
#'   unique value first appears in \code{x}, 3) "alphanum" = alphanumeric
#'   ordering of the unique values in \code{x} (the sorting used by
#'   \code{table}). When "frequency" is specified and there are ties, then the
#'   ties are sorted alphanumerically.
#'
#' @param decreasing logical vector of length 1 specifying whether the table
#'   should be sorted in decreasing (TRUE) or increasing (FALSE) order.
#'
#' @param na.last logical vector of length 1 specifying whether the table should
#'   have NAs last or in whatever position they end up at. This argument is only
#'   relevant if NAs exist in \code{x} and are included in the table (e.g.,
#'   useNA = "always" or exclude = NULL).
#'
#' @return list of numeric vector of frequencies by group. The number of list
#'   elements are the groups specified by \code{unique(interaction(grp, sep =
#'   sep))}. The frequencies either counts (if \code{prop} = FALSE) or
#'   proportions (if \code{prop} = TRUE) with the unique values of \code{x} as
#'   names (missing values have name = "(NA)"). Note, this is different from
#'   \code{table}, which returns a 1D-array and has class "table".
#'
#' @seealso
#'    \code{\link{freq}}
#'    \code{\link{freq_by}}
#'    \code{\link{freqs_by}}
#'    \code{\link{table}}
#'
#' @examples
#' x <- freq_by(mtcars$"gear", grp = mtcars$"vs")
#' str(x)
#' y <- freq_by(mtcars$"am", grp = mtcars$"vs", useNA = "no")
#' str(y)
#' str2str::lv2m(lapply(X = y, FUN = rev), along = 1) # ready to pass to prop.test()
#' @export
freq_by <- function(x, grp, exclude = if (useNA == "no") c(NA, NaN), useNA = "always",
   prop = FALSE, sort = "frequency", decreasing = TRUE, na.last = TRUE) {

   if (!(is.list(grp))) grp <- list(grp) # for aggregate() to work
   grp_len <- lapply(X = grp, FUN = length)
   if (!(all(length(x) == grp_len))) stop("the atomic vectors within `grp` must all have the same length as `x`")
   rtn <- tapply2(.x = x, .grp = grp, .fun = freq,
      exclude = exclude, useNA = useNA, prop = prop, sort = sort,
      decreasing = decreasing, na.last = na.last)
   return(rtn)
}

# freqs #

#' Multiple Univariate Frequency Tables
#'
#' \code{freqs} creates a frequency table for a set of variables in a
#' data.frame. Depending on \code{total}, frequencies for all the variables
#' together can be returned. The function probably makes the most sense for sets
#' of variables with similar unique values (e.g., items from a questionnaire
#' with similar response options).
#'
#' \code{freqs} uses \code{plyr::rbind.fill} to combine the results from
#' \code{table} applied to each variable into a single data.frame. If a variable
#' from \code{data[vrb.nm]} does not have values present in other variables from
#' \code{data[vrb.nm]}, then the frequencies in the return object will be 0.
#'
#' The name for the table element giving the frequency of missing values is
#' "(NA)". This is different from \code{table} where the name is
#' \code{NA_character_}. This change allows for the sorting of tables that
#' include missing values, as subsetting in R is not possible with
#' \code{NA_character_} names. In future versions of the package, this might
#' change as it should be possible to avoid this issue by subetting with a
#' logical vector or integer indices instead of names. However, it is convenient
#' to be able to subset the return object fully by names.
#'
#' @param data data.fame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param prop logical vector of length 1 specifying whether the frequencies
#'   should be counts (FALSE) or proportions (TRUE). Note, whether the
#'   proportions include missing values depends on the \code{useNA} argument.
#'
#' @param useNA character vector of length 1 specifying how missing values
#'   should be handled. The three options are 1) "no" = do not include NA
#'   frequencies in the return object, 2) "ifany" = only NA frequencies if there
#'   are any missing values (in any variable from \code{data[vrb.nm]}), or 3)
#'   "always" = do include NA frequencies regardless of whether there are
#'   missing values or not.
#'
#' @param total character vector of length 1 specifying whether the frequencies
#'   for the set of variables as a whole should be returned. The name "total"
#'   refers to tabulating the frequencies for the variables from
#'   \code{data[vrb.nm]} together as a set. The three options are 1) "no" = do
#'   not include a row for the total frequencies in the return object, 2) "yes"
#'   = do include the total frequencies as the first row in the return object,
#'   or 3) "only" = only include the total frequencies as a single row in the
#'   return object and do not include rows for each of the individual column
#'   frequencies in \code{data[vrb.nm]}.
#'
#' @return data.frame of frequencies for the variables in \code{data[vrb.nm]}.
#'   Depending on \code{prop}, the frequencies are either counts (FALSE) or
#'   proportions (TRUE). Depending on \code{total}, the nrow is either 1)
#'   \code{length(vrb.nm)} (if \code{total} = "no"), 1 + \code{length(vrb.nm)}
#'   (if \code{total} = "yes"), or 3) 1 (if \code{total} = "only"). The rownames
#'   are \code{vrb.nm} for each variable in \code{data[vrb.nm]} and "_total_"
#'   for the total row (if present). The colnames are the unique values present
#'   in \code{data[vrb.nm]}, potentially including "(NA)" depending on
#'   \code{useNA}.
#'
#' @seealso
#'    \code{\link{freq}}
#'    \code{\link{freqs_by}}
#'    \code{\link{freq_by}}
#'    \code{\link{table}}
#'
#' @examples
#' vrb_nm <- str2str::inbtw(names(psych::bfi), "A1","O5")
#' freqs(data = psych::bfi, vrb.nm = vrb_nm) # default
#' freqs(data = psych::bfi, vrb.nm = vrb_nm, prop = TRUE) # proportions by row
#' freqs(data = psych::bfi, vrb.nm = vrb_nm, useNA = "no") # without NA counts
#' freqs(data = psych::bfi, vrb.nm = vrb_nm, total = "yes") # include total counts
#' @export
freqs <- function(data, vrb.nm, prop = FALSE, useNA = "always", total = "no") {

   useNA <- match.arg(arg = useNA, choices = c("always","ifany","no"))
   total <- match.arg(arg = total, choices = c("no","yes","only"))
   if (total == "yes" || total == "only") {
      stacked <- stack(x = data, select = vrb.nm)
      tmp <- table(stacked$"values", useNA = useNA) # remember this table is a one-dimensional array
      if (prop) tmp <- tmp / sum(tmp)
      totaled <- as.data.frame(t(unclass(tmp)))
      names(totaled)[is.na(names(totaled))] <- "(NA)"
      row.names(totaled) <- "_total_"
      if (total == "only") return(totaled)
   }
   tmp <- lapply(X = data[vrb.nm], FUN = function(vec) {
      tab <- table(vec, useNA = useNA)
      if (prop) tab <- tab / sum(tab)
      dfm <- as.data.frame(t(unclass(tab)))
      names(dfm)[is.na(names(dfm))] <- "(NA)"
      return(dfm)
   })
   output <- do.call(what = `plyr`::`rbind.fill`, args = tmp)
   output[is.na(output)] <- 0L # have this be 0 rather than NA
   row.names(output) <- vrb.nm
   if (total == "no") return(output)
   output <- rbind(totaled, output) # rbind.data.frame
   return(output)
}

# freqs_by

#' Multiple Univariate Frequency Tables
#'
#' \code{freqs_by} creates a frequency table for a set of variables in a
#' data.frame by group. Depending on \code{total}, frequencies for all the
#' variables together can be returned by group. The function probably makes the
#' most sense for sets of variables with similar unique values (e.g., items from
#' a questionnaire with similar response options).
#'
#' \code{freqs_by} uses \code{plyr::rbind.fill} to combine the results from
#' \code{table} applied to each variable into a single data.frame for each
#' group. If a variable from \code{data[vrb.nm]} for each group does not have
#' values present in other variables from \code{data[vrb.nm]} for that group,
#' then the frequencies in the return object will be 0.
#'
#' The name for the table element giving the frequency of missing values is
#' "(NA)". This is different from \code{table} where the name is
#' \code{NA_character_}. This change allows for the sorting of tables that
#' include missing values, as subsetting in R is not possible with
#' \code{NA_character_} names. In future versions of the package, this might
#' change as it should be possible to avoid this issue by subetting with a
#' logical vector or integer indices instead of names. However, it is convenient
#' to be able to subset the return object fully by names.
#'
#' @param data data.fame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param prop logical vector of length 1 specifying whether the frequencies
#'   should be counts (FALSE) or proportions (TRUE). Note, whether the
#'   proportions include missing values depends on the \code{useNA} argument.
#'
#' @param useNA character vector of length 1 specifying how missing values
#'   should be handled. The three options are 1) "no" = do not include NA
#'   frequencies in the return object, 2) "ifany" = only NA frequencies if there
#'   are any missing values (in any variable from \code{data[vrb.nm]}), or 3)
#'   "always" = do include NA frequencies regardless of whether there are
#'   missing values or not.
#'
#' @param total character vector of length 1 specifying whether the frequencies
#'   for the set of variables as a whole should be returned. The name "total"
#'   refers to tabulating the frequencies for the variables from
#'   \code{data[vrb.nm]} together as a set. The three options are 1) "no" = do
#'   not include a row for the total frequencies in the return object, 2) "yes"
#'   = do include the total frequencies as the first row in the return object,
#'   or 3) "only" = only include the total frequencies as a single row in the
#'   return object and do not include rows for each of the individual column
#'   frequencies in \code{data[vrb.nm]}.
#'
#' @param sep character vector of length 1 specifying the string to combine the
#'   group values together with. \code{sep} is only used if there are multiple
#'   grouping variables (i.e., \code{length(grp.nm)} > 1).
#'
#' @return list of data.frames containing the frequencies for the variables in
#'   \code{data[vrb.nm]} by group. The number of list elements are the groups
#'   specified by \code{unique(interaction(data[grp.nm], sep = sep))}. Depending
#'   on \code{prop}, the frequencies are either counts (FALSE) or proportions
#'   (TRUE) by group. Depending on \code{total}, the nrow for each data.frame is
#'   either 1) \code{length(vrb.nm)} (if \code{total} = "no"), 1 +
#'   \code{length(vrb.nm)} (if \code{total} = "yes"), or 3) 1 (if \code{total} =
#'   "only"). The rownames are \code{vrb.nm} for each variable in
#'   \code{data[vrb.nm]} and "_total_" for the total row (if present). The
#'   colnames for each data.frame are the unique values present in
#'   \code{data[vrb.nm]}, potentially including "(NA)" depending on
#'   \code{useNA}.
#'
#' @seealso
#'    \code{\link{freqs}}
#'    \code{\link{freq_by}}
#'    \code{\link{freqs_by}}
#'    \code{\link{table}}
#'
#' @examples
#' vrb_nm <- str2str::inbtw(names(psych::bfi), "A1","O5")
#' freqs_by(data = psych::bfi, vrb.nm = vrb_nm, grp.nm = "gender") # default
#' freqs_by(data = psych::bfi, vrb.nm = vrb_nm, grp.nm = "gender",
#'    prop = TRUE) # proportions by row
#' freqs_by(data = psych::bfi, vrb.nm = vrb_nm, grp.nm = "gender",
#'    useNA = "no") # without NA counts
#' freqs_by(data = psych::bfi, vrb.nm = vrb_nm, grp.nm = "gender",
#'    total = "yes") # include total counts
#' freqs_by(data = psych::bfi, vrb.nm = vrb_nm,
#'    grp.nm = c("gender","education")) # multiple grouping variables
#' @export
freqs_by <- function(data, vrb.nm, grp.nm, prop = FALSE, useNA = "always",
   total = "no", sep = ".") {

   by2(.data = data, .vrb.nm = vrb.nm, .grp.nm = grp.nm, .sep = sep, .fun = freqs,
      vrb.nm = vrb.nm, prop = prop, useNA = useNA, total = total)
}

# mode2 #

#' Statistical Mode of a Numeric Vector
#'
#' \code{mode2} calculates the statistical mode - a measure of central tendancy
#' - of a numeric vector. This is in contrast to \code{\link{mode}} in base R,
#' which returns the storage mode of an object. In the case multiple modes
#' exist, the \code{multiple} argument allows the user to specify if they want
#' the multiple modes returned or just one.
#'
#' @param x atomic vector
#'
#' @param na.rm logical vector of length 1 specifying if missing values should
#'   be removed from \code{x} before calculating its frequencies.
#'
#' @param multiple logical vector of length 1 specifying if multiple modes
#'   should be returned in the case they exist. If multiple modes exist and
#'   \code{multiple} = TRUE, the multiple modes will be returned in alphanumeric
#'   order. If multiple modes exist and \code{multiple} = TRUE, the first mode
#'   in alphanumeric order will be returned. Note, NA is always last in the
#'   alphanumeric order. If only one mode exists, then the \code{multiple}
#'   argument is not used.
#'
#' @return atomic vector of the same storage mode as \code{x} providing the
#'   statistical mode(s).
#'
#' @seealso
#'    \code{\link{freq}}
#'    \code{\link{table}}
#'
#' @examples
#'
#' # ONE MODE
#' vec <- c(7,8,9,7,8,9,9)
#' mode2(vec)
#' mode2(vec, multiple = TRUE)
#'
#' # TWO MODES
#' vec <- c(7,8,9,7,8,9,8,9)
#' mode2(vec)
#' mode2(vec, multiple = TRUE)
#'
#' # WITH NA
#' vec <- c(7,8,9,7,8,9,NA,9)
#' mode2(vec)
#' mode2(vec, na.rm = TRUE)
#' vec <- c(7,8,9,7,8,9,NA,9,NA,NA)
#' mode2(vec)
#' mode2(vec, multiple = TRUE)
#' @export
mode2 <- function(x, na.rm = FALSE, multiple = FALSE) {

   if (na.rm) use_na <- "no" else use_na <- "always"
   freq_obj <- freq(x, useNA = use_na, decreasing = FALSE, na.last = TRUE)
   if (!multiple) {
      max_pos <- which.max(freq_obj) # only selects the *first* maximimum
      max_nm <- names(max_pos)
      # max_val <- freq_obj[max_pos]
      # max_nm <- names(max_val)
      # rtn <- `mode<-`(max_nm, value = mode(x))
   }
   if (multiple) {
      max_pos <- which.max(freq_obj) # only selects the *first* maximimum
      max_val <- freq_obj[max_pos]
      max_pos2 <- freq_obj[freq_obj == max_val]
      max_nm <- names(max_pos2)
      # max_val2 <- freq_obj[max_pos2]
      # max_nm2 <- names(max_val2)
      # rtn <- `mode<-`(max_nm2, value = mode(x))
   }
   rtn <- suppressWarnings(`mode<-`(max_nm, value = mode(x))) # to prevent NA conversion warning from printing
   return(rtn)
}

# RECODE ####

# recode2other #

#' Recode Unique Values in a Character Vector to 0ther (or NA)
#'
#' \code{recode2other} recodes multiple unique values in a character vector to
#' the same new value (e.g., "other", NA_character_). It's primary use is to
#' recode based on the minimum frequency of the unique values so that low
#' frequency values can be combined into the same category; however, it also
#' allows for recoding particular unique values given by the user (see details).
#' This function is a wrapper for \code{car::recode}, which can handle general
#' recoding of character vectors.
#'
#' The \code{extra.nm} argument allows for \code{recode2other} to be used as
#' simpler function that just recodes particular unique values to the same new
#' value (although arguably this is easier to do using \code{car::recode}
#' directly). To do so set \code{freq.min = 0} and provide the unique values to
#' \code{extra.nm}. Note, that the current version of this function does not
#' allow for NA_character_ to be included in \code{extra.nm} as it will end up
#' treating it as "NA" (see examples).
#'
#' @param x character vector. If not a character vector, it will be coarced to
#'   one via \code{as.character}.
#'
#' @param freq.min numeric vector of length 1 specifying the minimum frequency
#'   of a unique value to keep it unchanged and consequentially recode any
#'   unique values with frequencues less than (or equal to) it.
#'
#' @param prop logical vector of length 1 specifying if \code{freq.min} provides
#'   the frequency as a count (FALSE) or proportion (TRUE).
#'
#' @param inclusive logical vector of length 1 specifying whether the frequency
#'   of a unique value exactly equal to \code{freq.min} should be kept unchanged
#'   (and not recoded to \code{other.nm}).
#'
#' @param other.nm character vector of length 1 specifying what value the other
#'   unique values should be recoded to. This can be NA_character_ resulting in
#'   recoding to a missing value.
#'
#' @param extra.nm character vector specifying extra unique values that should
#'   be recoded to \code{other.nm} that are not included based on the minimum
#'   frequency from the combination of \code{freq.min}, \code{prop},
#'   \code{inclusive}. The default is NULL, meaning no extra unique values are
#'   recoded.
#'
#' @return character vector of the same length as \code{x} with unique values
#'   with frequency less than \code{freq.nm} recoded to \code{other.nm} as well
#'   as any unique values in \code{extra.nm}. While the current version of the
#'   function allows for recoding *to* NA values via \code{other.nm}, it does
#'   not allow for recoding *from* NA values via \code{extra.nm} (see examples).
#'
#' @seealso
#'    \code{\link[car]{recode}}
#'    \code{\link{ifelse}}
#'
#' @examples
#'
#' # based on minimum frequency unique values
#' state_region <- as.character(state.region)
#' recode2other(state_region, freq.min = 13) # freq.min as a count
#' recode2other(state_region, freq.min = 0.26, prop = TRUE) # freq.min as a proportion
#' recode2other(state_region, freq.min = 13, other.nm = "_blank_")
#' recode2other(state_region, freq.min = 13,
#'    other.nm = NA) # allows for other.nm to be NA
#' recode2other(state_region, freq.min = 13,
#'    extra.nm = "South") # add an extra unique value to recode
#' recode2other(state_region, freq.min = 13,
#'    inclusive = FALSE) # recodes "West" to "other"
#'
#' # based on user given unique values
#' recode2other(state_region, freq.min = 0,
#'    extra.nm = c("South","West")) # recodes manually rather than by freq.min
#' # current version does NOT allow for NA to be a unique value that is converted to other
#' state_region2 <- c(NA, state_region, NA)
#' recode2other(state_region2, freq.min = 13) # NA remains in the character vector
#' recode2other(state_region2, freq.min = 0,
#'    extra.nm = c("South","West",NA)) # NA remains in the character vector
#'
#' @export
recode2other <- function(x, freq.min, prop = FALSE, inclusive = TRUE,
   other.nm = "other", extra.nm = NULL) {

   if(!(is.character(x))) x <- as.character(x)
   count <- c(tapply(X = x, INDEX = x, FUN = length)) # c to get rid of the single dimension
   if (prop) freq_min <- freq.min * length(x)
   if (!prop) freq_min <- freq.min
   if (inclusive) `%fun%` <- `<`
   if (!inclusive) `%fun%` <- `<=`
   other <- names(count)[count %fun% freq_min]
   other_quote <- paste0("'", c(other, extra.nm), "'")
   if (!(is.na(other.nm)))
      other.nm_quote <- paste0("'", other.nm, "'")
   else
      other.nm_quote <- other.nm
   recodes <- paste(other_quote, other.nm_quote, sep = "=", collapse = "; ")
   output <- car::recode(var = x, recodes = recodes)
   return(output)
}

# reorders #

#' Reorder Levels of Factor Data
#'
#' \code{reorders} re-orders the levels of factor data. The factors are columns
#' in a data.frame where the same reordering scheme is desired. This is often
#' useful before using factor data in a statistical analysis (e.g., \code{lm})
#' or a graph (e.g., \code{ggplot}). It is essentially a vectorized version of
#' \code{reorder.default}.
#'
#' @param data data.frame of data.
#'
#' @param fct.nm character vector of colnames in \code{data} that specify the
#'   factor columns. If any of the columns specified by \code{fct.nm} are not
#'   factors, then an error is returned.
#'
#' @param ord.nm character vector of length 1 or \code{NULL}. If a character
#'   vector of length 1, it is a colname in \code{data} specifying the column in
#'   \code{data} that will be used in conjunction with \code{fun} to re-order
#'   the factor columns. If \code{NULL} (default), it is assumed that each
#'   factor column itself will be used in conjunction with \code{fun} to
#'   re-order the factor columns.
#'
#' @param fun function that will be used to re-order the factor columns. The
#'   function is expected to input an atomic vector of length =
#'   \code{nrow(data)} and return an atomic vector of length 1. \code{fun} is
#'   applied to \code{data[[ord.nm]]} if \code{ord.nm} is a character vector of
#'   length 1 or applied to each column in \code{data[fct.nm]} if \code{ord.nm}
#'   = \code{NULL}.
#'
#' @param ... additional named arguments used by \code{fun}. For example, if
#'   \code{fun} is \code{mean}, the user might specify an argument \code{na.rm =
#'   TRUE} to set the \code{na.rm} argument in the \code{mean} function.
#'
#' @param suffix character vector of length 1 specifying the string that will be
#'   appended to the end of the colnames in the return object.
#'
#' @return data.frame of re-ordered factor columns with colnames =
#'   \code{paste0(fct.nm, suffix)}.
#'
#' @seealso
#'    \code{\link[stats]{reorder.default}}
#'
#' @examples
#'
#' # factor vector
#' reorder(x = state.region, X = state.region,
#'    FUN = length) # least frequent to most frequent
#' reorder(x = state.region, X = state.region,
#'    FUN = function(vec) {-1 * length(vec)}) # most frequent to least frequent
#'
#' # data.frame of factors
#' infert_fct <- infert
#' fct_nm <- c("education","parity","induced","case","spontaneous")
#' infert_fct[fct_nm] <- lapply(X = infert[fct_nm], FUN = as.factor)
#' x <- reorders(data = infert_fct, fct.nm = fct_nm,
#'    fun = length) # least frequent to most frequent
#' lapply(X = x, FUN = levels)
#' y <- reorders(data = infert_fct, fct.nm = fct_nm,
#'    fun = function(vec) {-1 * length(vec)}) # most frequent to least frequent
#' lapply(X = y, FUN = levels)
#' # ord.nm specified as a different column in data.frame
#' z <- reorders(data = infert_fct, fct.nm = fct_nm, ord.nm = "pooled.stratum",
#'    fun = mean) # category with highest mean for pooled.stratum to
#'    # category with lowest mean for pooled.stratum
#' lapply(X = z, FUN = levels)
#'
#' @export
reorders <- function(data, fct.nm, ord.nm = NULL, fun, ..., suffix = "_r") {

   test_fct <- lapply(X = data[fct.nm], FUN = is.factor)
   if (!(all(unlist(test_fct)))) stop("At least one column in `data`[`fct.nm`] is not a factor.")
   if (is.null(ord.nm)) {
      tmp_fct <- lapply(X = data[fct.nm], FUN = function(fct)
         reorder(x = fct, X = fct, FUN = fun, ...))
   }
   if (!(is.null(ord.nm))) {
      ord_rep <- replicate(n = length(fct.nm), expr = data[[ord.nm]], simplify = FALSE)
      tmp_fct <- Map(fct = data[fct.nm], ord = ord_rep, f = function(fct, ord)
         reorder(x = fct, X = ord, FUN = fun, ...))
   }
   output <- data.frame(tmp_fct, stringsAsFactors = FALSE)
   names(output) <- paste0(fct.nm, rep.int(x = suffix, times = length(fct.nm)))
   row.names(output) <- row.names(data)
   return(output)
}

# nom2dum #

#' Nominal Variable to Dummy Variables
#'
#' \code{nom2dum} converts a nominal variable into a set of dummy variables.
#' There is one dummy variable for each unique value in the nominal variable.
#' Note, base R does this recoding internally through the
#' \code{model.matrix.default} function, but it is used in the context of
#' regression-like models and it is not clear how to simplify it for general use
#' cases outside that context.
#'
#' Note, that \code{yes} and \code{no} are assumed to be the same typeof. If
#' they are not, then the columns in the return object will be coerced to the
#' most complex typeof (i.e., most to least: character, double, integer,
#' logical).
#'
#' @param nom character vector (or any atomic vector, including factors, which
#'   will be then coerced to a character vector) specifying the nominal
#'   variable.
#'
#' @param yes atomic vector of length 1 specifying what unique value should
#'   represent rows when the nominal category of interest is present. For a
#'   traditional dummy variable this value would be 1.
#'
#' @param no atomic vector of length 1 specifying what unique value should
#'   represent rows when the nominal category of interest is absent. For a
#'   traditional dummy variable this value would be 0.
#'
#' @param prefix character vector of length 1 specifying the string that should
#'   be appended to the beginning of each colname in the return object.
#'
#' @param rtn.fct logical vector of length 1 specifying whether the columns of
#'   the return object should be factors where the first level is \code{no} and
#'   the second level is \code{yes}.
#'
#' @return data.frame of dummy columns with colnames specified by
#'   \code{paste0(prefix, unique(nom))} and rownames specified by
#'   \code{names(nom)} or default \code{data.frame} rownames (i.e.,
#'   c("1","2","3", etc.) if \code{names(nom)} is \code{NULL}.
#'
#' @seealso
#'    \code{\link[stats]{model.matrix.default}}
#'    \code{\link{dum2nom}}
#'
#' @examples
#' nom2dum(infert$"education") # default
#' nom2dum(infert$"education", prefix = "edu_") # use of the `prefix` argument
#' nom2dum(nom = infert$"education", yes = "one", no = "zero",
#'    rtn.fct = TRUE) # returns factor columns
#' @export
nom2dum <- function(nom, yes = 1L, no = 0L, prefix = "", rtn.fct = FALSE) {

   if (!(is.character(nom))) nom <- as.character(nom)
   nom_unique <- na.omit(unique(nom))
   tmp <- lapply(X = nom_unique, FUN = function(vl)
      ifelse(nom == vl, yes = yes, no = no))
   if (rtn.fct) tmp <- lapply(X = tmp, FUN = factor, levels = c(no, yes))
   dum <- data.frame(tmp, stringsAsFactors = FALSE) # stringsAsFactors does not affect vectors that are already factors
   names(dum) <- paste0(prefix, nom_unique)
   if (!(is.null(names(nom)))) row.names(dum) <- names(nom) # if `nom` does not have names, they will be NULL and the call by `row.names<-` is inert but does not through an error or warning
   return(dum)
}

# dum2nom

#' Dummy Variables to a Nominal Variable
#'
#' \code{dum2nom} converts dummy variables to a nominal variable. The
#' information from the dummy columns in a data.frame are combined into a
#' character vector (or factor if \code{rtn.fct} = TRUE) representing a nominal
#' variable. The unique values of the nominal variable will be the dummy
#' colnames (i.e., \code{dum.nm}). Note, *all* the dummy variables associated
#' with a nominal variable are required for this function to work properly. In
#' regression-like models, data analysts will exclude one dummy variable for the
#' category that is the reference group. If d = number of categories in the
#' nominal variable, then that leads to d - 1 dummy variables in the model.
#' \code{dum2nom} requires all d dummy variables.
#'
#' \code{dum2nom} tests to ensure that \code{data[dum.nm]} are indeed a set of
#' dummy columns. First, the dummy columns are expected to have the same mode
#' such that there is one \code{yes} unique value across the dummy columns.
#' Second, each row in \code{data[dum.nm]} is expected to have either 0 or 1
#' instance of \code{yes}. If there is more than one instance of \code{yes} in a
#' row, then an error is returned. If there is 0 instances of \code{yes} in a
#' row (e.g., all missing values), NA is returned for that row. Note, any value
#' other than \code{yes} will be treated as a no.
#'
#' @param data data.frame of data.
#'
#' @param dum.nm character vector of colnames from \code{data} specifying the
#'   dummy variables.
#'
#' @param yes atomic vector of length 1 specifying the unique value of the
#'   category in each dummy column. This must be the same value for all the
#'   dummy variables.
#'
#' @param rtn.fct logical vector of length 1 specifying whether the return
#'   object should be a factor (TRUE) or a character vector (FALSE).
#'
#' @return character vector (or factor if \code{rtn.fct} = TRUE) containing the
#'   unique values of \code{dum.nm} - one for each dummy variable.
#'
#' @seealso
#'    \code{\link{nom2dum}}
#'
#' @examples
#' dum <- data.frame(
#'    "Quebec_nonchilled" = ifelse(CO2$"Type" == "Quebec" & CO2$"Treatment" == "nonchilled",
#'       yes = 1L, no = 0L),
#'    "Quebec_chilled" = ifelse(CO2$"Type" == "Quebec" & CO2$"Treatment" == "chilled",
#'       yes = 1L, no = 0L),
#'    "Mississippi_nonchilled" = ifelse(CO2$"Type" == "Mississippi" & CO2$"Treatment" == "nonchilled",
#'       yes = 1L, no = 0L),
#'    "Mississippi_chilled" = ifelse(CO2$"Type" == "Mississippi" & CO2$"Treatment" == "chilled",
#'       yes = 1L, no = 0L)
#' )
#' dum2nom(data = dum, dum.nm = names(dum)) # default
#' dum2nom(data = dum, dum.nm = names(dum), rtn.fct = TRUE) # return as a factor
#' \dontrun{
#' dum2nom(data = npk, dum.nm = c("N","P","K")) # error due to overlapping dummy columns
#' dum2nom(data = mtcars, dum.nm = c("vs","am"))# error due to overlapping dummy columns
#' }
#' @export
dum2nom <- function(data, dum.nm, yes = 1L, rtn.fct = FALSE) {

   test <- apply(X = as.matrix(data[dum.nm]), MARGIN = 1, FUN = function(vec) # implicitly assumes each column has the same mode when coercing to the same mode via as.matrix()
      ifelse(sum(vec == yes) > 1, yes = FALSE, no = TRUE))
   if (!(all(test))) stop("some rows in `data`[`dum.nm`] have `yes` in multiple columns.")
   nom <- rep.int(x = NA_character_, times = nrow(data))
   for (nm in dum.nm) { # cannot think of a way to use lapply()
      pos <- ifelse(data[[nm]] == yes, yes = TRUE, no = FALSE)
      nom[pos] <- nm
   }

   if (rtn.fct) nom <- as.factor(nom)
   return(nom)
}

# reverse #

#' Reverse Code a Numeric Vector
#'
#' \code{reverse} reverse codes a numeric vector based on minimum and maximum
#' values. For example, say numerical values of response options can range from
#' 1 to 4. The function will change 1 to 4, 2 to 3, 3 to 2, and 4 to 1. If there
#' are an odd number of response options, the middle in the sequence will be
#' unchanged.
#'
#' @param x numeric vector.
#'
#' @param mini numeric vector of length 1 specifying the minimum numeric value.
#'
#' @param maxi numeric vector of length 1 specifying the maximum numeric value.
#'
#' @return numeric vector that correlates exactly -1 with \code{x}.
#'
#' @seealso
#'    \code{\link{reverses}}
#'    \code{\link[psych]{reverse.code}}
#'    \code{\link[car]{recode}}
#'
#' @examples
#' x <- psych::bfi[[1]]
#' head(x, n = 15)
#' y <- reverse(x = psych::bfi[[1]], min = 1, max = 6)
#' head(y, n = 15)
#' cor(x, y, use = "complete.obs")
#' @export
reverse <- function(x, mini, maxi) {
   (mini + maxi) - x
}

# reverses #

#' Reverse Code Numeric Data
#'
#' \code{reverses} reverse codes numeric data based on minimum and maximum
#' values. For example, say numerical values of response options can range from
#' 1 to 4. The function will change 1 to 4, 2 to 3, 3 to 2, and 4 to 1. If there
#' are an odd number of response options, the middle in the sequence will be
#' unchanged.
#'
#' \code{reverses} is simply a vectorized version of \code{reverse} to more
#' easily reverse code multiple columns of a data.frame at the same time.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param mini numeric vector of length 1 specifying the minimum numeric value.
#'
#' @param maxi numeric vector of length 1 specifying the maximum numeric value.
#'
#' @param suffix character vector of length 1 specifying the string to add to
#'   the end of the colnames in the return object.
#'
#' @return data.frame of reverse coded variables with colnames specified by
#'   \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{reverse}}
#'    \code{\link[psych]{reverse.code}}
#'    \code{\link{recodes}}
#'
#' @examples
#' tmp <- !(is.element(el = names(psych::bfi) , set = c("gender","education","age")))
#' vrb_nm <- names(psych::bfi)[tmp]
#' reverses(data = psych::bfi, vrb.nm = vrb_nm, mini = 1, maxi = 6)
#' @export
reverses <- function(data, vrb.nm, mini, maxi, suffix = "_r") {

   tmp_lst <- lapply(X = data[vrb.nm], FUN = reverse, mini = mini, maxi = maxi)
   output <- data.frame(tmp_lst, stringsAsFactors = FALSE)
   names(output) <- paste0(vrb.nm, suffix)
   return(output)
}

# recodes #

#' Recode Data
#'
#' \code{recodes} recodes data based on specified recodes using the
#' \code{car::recode} function. This can be used for numeric or character
#' (including factors) data. See \code{\link[car]{recode}} for details. The
#' \code{levels} argument from \code{car::recode} is excluded because there is
#' no easy way to vectorize it when only a subset of the variables are factors.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param recodes character vector of length 1 specifying the recodes. See
#'   details of \code{\link[car]{recode}} for how to use this argument.
#'
#' @param as.factor logical vector of length 1 specifying if the recoded columns
#'   should be returned as factors. The default depends on the column in
#'   \code{data[vrb.nm]}. If the column is a factor, then \code{as.factor} =
#'   TRUE for that column. If the column is not a factor, then \code{as.factor}
#'   = FALSE for that column. Any non-default, specified value for this argument
#'   will result in \code{as.factor} being universally applied to all columns in
#'   \code{data[vrb.nm]}.
#'
#' @param as.numeric logical vector of length 1 specifying if the recoded
#'   columns should be returned as numeric vectors when possible. This can be
#'   useful when having character vectors converted to numeric, such that
#'   numbers with typeof character (e.g., "1") will be coerced to typeof numeric
#'   (e.g., 1). Note, this argument has no effect on columns in
#'   \code{data[vrb.nm]} which are typeof character and have letters in their
#'   values (e.g., "1a"). Note, this argument is often not needed as you can
#'   directly recode to a numeric by excluding quotes from the number in the
#'   \code{recodes} argument.
#'
#' @param suffix character vector of length 1 specifying the string to add to
#'   the end of the colnames in the return object.
#'
#' @return data.frame of recoded variables with colnames specified by
#'   \code{paste0(vrb.nm, suffix)}. In general, the columns of the data.frame
#'   are the same typeof as those in \code{data} except for instances when
#'   \code{as.factor} and/or \code{as.numeric} change the typeof.
#'
#' @seealso
#'    \code{\link[car]{recode}}
#'    \code{\link{reverses}}
#'
#' @examples
#' recodes(data = psych::bfi, vrb.nm = c("A1","C4","C5","E1","E2","O2","O5"),
#'    recodes = "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
#' re_codes <- "'Quebec' = 'canada'; 'Mississippi' = 'usa'; 'nonchilled' = 'no'; 'chilled' = 'yes'"
#' recodes(data = CO2, vrb.nm = c("Type","Treatment"), recodes = re_codes,
#'    as.factor = FALSE) # convert from factors to characters
#' @export
recodes <- function(data, vrb.nm, recodes, suffix = "_r", as.factor,
                   as.numeric = TRUE) {

   tmp_lst <- lapply(X = data[vrb.nm], FUN = car::recode, recodes = recodes,
                     as.factor = as.factor, as.numeric = as.numeric)
   output <- data.frame(tmp_lst, stringsAsFactors = FALSE)
   for (i in seq_along(tmp_lst)) { # must use for loop (rather than lapply) to convert to factors
      if (is.factor(tmp_lst[[i]])) output[[i]] <- as.factor(output[[i]])
   }
   names(output) <- paste0(vrb.nm, suffix)
   return(output)
}

# pomp #

#' Recode a Numeric Vector to Percentage of Maximum Possible (POMP) Units
#'
#' \code{pomp} recodes a numeric vector to percentage of maximum possible (POMP)
#' units. This can be useful when data is measured with arbitrary units (e.g.,
#' Likert scale).
#'
#' There are two common approaches to POMP scores: 1) absolute POMP units where
#' the minimum and maximum are the smallest/largest values possible from the
#' measurement instrument (e.g., 1 to 7 on a Likert scale) and 2) relative POMP
#' units where the minimum and maximum are the smallest/largest values observed
#' in the data (e.g., 1.3 to 6.8 on a Likert scale). Both will be correlated
#' perfectly with the original units as they are each linear transformations.
#'
#' @param x numeric vector.
#'
#' @param mini numeric vector of length 1 specifying the minimum numeric value
#'   possible.
#'
#' @param maxi numeric vector of length 1 specifying the maximum numeric value
#'   possible.
#'
#' @param relative logical vector of length 1 specifying whether relative POMP
#'   scores (rather than absolute POMP scores) should be created. If TRUE, then
#'   the \code{mini} and \code{maxi} arguments are ignored. See details for the
#'   distinction between absolute and relative POMP scores.
#'
#' @param unit numeric vector of length 1 specifying how many percentage points
#'   is desired for the units. Traditionally, POMP scores use \code{unit} = 1
#'   (default) such that one unit is one percentage point. However, another
#'   option is to use \code{unit} = 100 such that one unit is all 100 percentage
#'   points (i.e., proportion of maximum possible). This argument also gives the
#'   flexibility of specifying units in between 1 and 100 percentage points. For
#'   example, \code{unit} = 50 would mean that one unit represents going from
#'   low (i.e., 25th percentile) to high (i.e., 75th percentile) on the
#'   variable.
#'
#' @return numeric vector from recoding \code{x} to percentage of maximum
#'   possible (pomp) with units specified by \code{unit}.
#'
#' @seealso
#'    \code{\link{pomps}}
#'
#' @examples
#' vec <- psych::bfi[[1]]
#' pomp(x = vec, mini = 1, maxi = 6) # absolute POMP units
#' pomp(x = vec, relative = TRUE) # relative POMP units
#' pomp(x = vec, mini = 1, maxi = 6, unit = 100) # unit = 100
#' pomp(x = vec, mini = 1, maxi = 6, unit = 50) # unit = 50
#' @export
pomp <- function(x, mini, maxi, relative = FALSE, unit = 1) {

   if (relative) {
      mini <- min(x = x, na.rm = TRUE) # R gets confused when trying to call functions which have the same name as arguments (e.g., min, max), even with match.fun()
      maxi <- max(x = x, na.rm = TRUE)
   }
   tmp <- (x - mini) / (maxi - mini)
   output <- tmp * (100 / unit)
   return(output)
}

# pomps #

#' Recode Numeric Data to Percentage of Maximum Possible (POMP) Units
#'
#' \code{pomps} recodes numeric data to percentage of maximum possible (POMP)
#' units. This can be useful when data is measured with arbitrary units (e.g.,
#' Likert scale).
#'
#' There are two common approaches to POMP scores: 1) absolute POMP units where
#' the minimum and maximum are the smallest/largest values possible from the
#' measurement instrument (e.g., 1 to 7 on a Likert scale) and 2) relative POMP
#' units where the minimum and maximum are the smallest/largest values observed
#' in the data (e.g., 1.3 to 6.8 on a Likert scale). Both will be correlated
#' perfectly with the original units as they are each linear transformations.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param mini numeric vector of length 1 specifying the minimum numeric value
#'   possible. Note, this is assumed to be the same for each variable.
#'
#' @param maxi numeric vector of length 1 specifying the maximum numeric value
#'   possible. Note, this is assumed to be the same for each variable.
#'
#' @param relative logical vector of length 1 specifying whether relative POMP
#'   scores (rather than absolute POMP scores) should be created. If TRUE, then
#'   the \code{mini} and \code{maxi} arguments are ignored. See details for the
#'   distinction between absolute and relative POMP scores.
#'
#' @param unit numeric vector of length 1 specifying how many percentage points
#'   is desired for the units. Traditionally, POMP scores use \code{unit} = 1
#'   (default) such that one unit is one percentage point. However, another
#'   option is to use \code{unit} = 100 such that one unit is all 100 percentage
#'   points (i.e., proportion of maximum possible). This argument also gives the
#'   flexibility of specifying units in between 1 and 100 percentage points. For
#'   example, \code{unit} = 50 would mean that one unit represents going from
#'   low (i.e., 25th percentile) to high (i.e., 75th percentile) on the
#'   variable.
#'
#' @param suffix character vector of length 1 specifying the string to add to
#'   the end of the column names in the return object.
#'
#' @return data.frame of variables recoded to percentage of maximum possible
#'   (pomp) with units specified by \code{unit} and names specified by
#'   \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{pomp}}
#'
#' @examples
#' vrb_nm <- names(psych::bfi)[grepl(pattern = "A", x = names(psych::bfi))]
#' pomps(data = psych::bfi, vrb.nm = vrb_nm, min = 1, max = 6) # absolute POMP units
#' pomps(data = psych::bfi, vrb.nm = vrb_nm, relative = TRUE) # relative POMP units
#' pomps(data = psych::bfi, vrb.nm = vrb_nm, min = 1, max = 6, unit = 100) # unit = 100
#' pomps(data = psych::bfi, vrb.nm = vrb_nm, min = 1, max = 6, unit = 50) # unit = 50
#' pomps(data = psych::bfi, vrb.nm = vrb_nm, min = 1, max = 6, suffix = "_pomp")
#' @export
pomps <- function(data, vrb.nm, mini, maxi, relative = FALSE, unit = 1,
                 suffix = paste0("_p", unit)) {

   tmp_pomp <- Map(vec = data[vrb.nm], f = function(vec)
      pomp(x = vec, mini = mini, maxi = maxi, relative = relative, unit = unit))
   output <- data.frame(tmp_pomp, stringsAsFactors = FALSE)
   row.names(output) <- row.names(data)
   names(output) <- paste0(vrb.nm, suffix)
   return(output)
}

# valid_test #

#' Test for Invalid Elements in a Vector
#'
#' \code{valid_test} tests whether a vector has any invalid elements. Valid
#' values are specified by \code{valid}. If the vector \code{x} has any values
#' other than \code{valid}, then FALSE is returned; If the vector \code{x} only
#' has values in \code{valid}, then TRUE is returned. This function can be
#' useful for checking data after manual human entry.
#'
#' @param x atomic vector or list vector.
#'
#' @param valid atomic vector or list vector of valid values.
#'
#' @param na.rm logical vector of length 1 specifying whether NA should be
#'   ignored from the validity test. If TRUE (default), then any NAs are treated
#'   as valid.
#'
#' @return logical vector of length 1 specifying whether all elements in
#'   \code{x} are valid values. If FALSE, then (at least one) invalid values are
#'   present.
#'
#' @seealso
#'    \code{\link{valids_test}}
#'    \code{\link{revalid}}
#'    \code{\link{revalids}}
#'
#' @examples
#' valid_test(x = psych::bfi[[1]], valid = 1:6) # return TRUE
#' valid_test(x = psych::bfi[[1]], valid = 0:5) # 6 is not present in `valid`
#' valid_test(x = psych::bfi[[1]], valid = 1:6,
#'    na.rm = FALSE) # NA is not present in `valid`
#' @export
valid_test <- function(x, valid, na.rm = TRUE) {

   unique_values <- unique(x)
   if (na.rm) valid <- c(valid, NA)
   diff_values <- setdiff(x = unique_values, y = valid)
   if (length(diff_values) > 0)
      output <- FALSE
   else
      output <- TRUE
   return(output)
}

# valids_test #

#' Test for Invalid Elements in Data
#'
#' \code{Valid.test} tests whether data has any invalid elements. Valid values
#' are specified by \code{valid}. Each variable is tested independently. If the
#' variable in \code{data[vrb.nm]} has any values other than \code{valid}, then
#' FALSE is returned for that variable; If the variable in \code{data[vrb.nm]}
#' only has values in \code{valid}, then TRUE is returned for that variable.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables
#'
#' @param valid atomic vector or list vector of valid values.
#'
#' @param na.rm logical vector of length 1 specifying whether NA should be
#'   ignored from the validity test. If TRUE (default), then any NAs are treated
#'   as valid.
#'
#' @return logical vector with length = \code{length(vrb.nm)} and names =
#'   \code{vrb.nm} specifying whether all elements in each variable of
#'   \code{data[vrb.nm]} are valid. If FALSE, then (at least one) invalid values
#'   are present in that variable of \code{data[vrb.nm]}.
#'
#' @seealso
#'    \code{\link{valid_test}}
#'    \code{\link{revalids}}
#'    \code{\link{revalid}}
#'
#' @examples
#' valids_test(data = psych::bfi, vrb.nm = names(psych::bfi)[1:25],
#'    valid = 1:6) # return TRUE
#' valids_test(data = psych::bfi, vrb.nm = names(psych::bfi)[1:25],
#'    valid = 0:5) # 6 is not present in `valid`
#' valids_test(data = psych::bfi, vrb.nm = names(psych::bfi)[1:25],
#'    valid = 1:6, na.rm = FALSE) # NA is not present in `valid`
#' valids_test(data = ToothGrowth, vrb.nm = c("supp","dose"),
#'    valid = list("VC", "OJ", 0.5, 1.0, 2.0)) # list vector as `valid` to allow for
#'    # elements of different typeof
#' @export
valids_test <- function(data, vrb.nm, valid, na.rm = TRUE) {

   tmp_lst <- lapply(X = data[vrb.nm], FUN = valid_test, valid = valid, na.rm = na.rm)
   output <- unlist(tmp_lst) # names are kept because names are present in X of lapply
   return(output)
}

# revalid #

#' Recode Invalid Values from a Vector
#'
#' \code{revalid} recodes invalid data to specified values. For example,
#' sometimes invalid values are present in a vector of data (e.g., age = -1).
#' This function allows you to specify which values are possible and will then
#' recode any impossible values to \code{undefined}. This function is a useful
#' wrapper for the function \code{car::recode}, tailored for the specific use of
#' recoding invalid values.
#'
#' @param x atomic vector.
#'
#' @param valid atomic vector of valid values for \code{x}.
#'
#' @param undefined atomic vector of length 1 specifying what the invalid values
#'   should be recoded to.
#'
#' @return atomic vector with the same typeof as \code{x} where any values not
#'   present in \code{valid} have been recoded to \code{undefined}.
#'
#' @seealso
#'    \code{\link{revalids}}
#'    \code{\link{valid_test}}
#'    \code{\link{valids_test}}
#'
#' @examples
#' revalid(x = attitude[[1]], valid = 25:75, undefined = NA) # numeric vector
#' revalid(x = as.character(ToothGrowth[["supp"]]), valid = c('VC'),
#'    undefined = NA) # character vector
#' revalid(x = ToothGrowth[["supp"]], valid = c('VC'),
#'    undefined = NA) # factor
#' @export
revalid <- function(x, valid, undefined = NA) {

   if (!(is.character(valid)))
      valid_chr <- paste0(valid , " = ", valid, ";", collapse = " ")
   if (is.character(valid))
      valid_chr <- paste0("'", valid , "'", " = ", "'", valid, "'", ";", collapse = " ")
   else_chr <- paste0("else", " = ", undefined)
   recodes_chr <- paste0(valid_chr, else_chr, collapse = " ")
   output <- car::recode(var = x, recodes = recodes_chr)
   return(output)
}

# revalids #

#' Recode Invalid Values from Data
#'
#' \code{revalids} recodes invalid data to specified values. For example,
#' sometimes invalid values are present in a vector of data (e.g., age = -1).
#' This function allows you to specify which values are possible and will then
#' recode any impossible values to \code{undefined}. \code{revalids} is simply a
#' vectorized version of \code{revalid} to more easily revalid multiple columns
#' of a data.frame at the same time.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param valid atomic vector of valid values for the data. Note, the valid
#'   values must be the same for each variable.
#'
#' @param undefined atomic vector of length 1 specifying what the invalid values
#'   should be recoded to.
#'
#' @param suffix character vector of length 1 specifying the string to add to
#'   the end of the colnames in the return object.
#'
#' @return data.frame of recoded variables where any values not present in
#'   \code{valid} have been recoded to \code{undefined} with colnames specified
#'   by \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{revalid}}
#'    \code{\link{valids_test}}
#'    \code{\link{valid_test}}
#'
#' @examples
#' revalids(data = attitude, vrb.nm = names(attitude),
#'    valid = 25:75) # numeric data
#' revalids(data = as.data.frame(CO2), vrb.nm = c("Type","Treatment"),
#'    valid = c('Quebec','nonchilled')) # factors
#' @export
revalids <- function(data, vrb.nm, valid, undefined = NA, suffix = "_v") {

   tmp_lst <- lapply(X = data[vrb.nm], FUN = revalid, valid = valid, undefined = undefined)
   output <- data.frame(tmp_lst, stringsAsFactors = FALSE)
   for (i in seq_along(tmp_lst)) { # must use for loop (rather than lapply) to convert make to factors
      if (is.factor(tmp_lst[[i]])) output[[i]] <- as.factor(output[[i]])
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# STAT_IF ####

# sum_if #

#' Sum Conditional on Minimum Frequency of Observed Values
#'
#' \code{sum_if} calculates the sum of a numeric or logical vector conditional
#' on a specified minimum frequency of observed values. If the amount of
#' observed data is less than (or equal to) \code{ov.min}, then \code{NA} is
#' returned rather than the sum.
#'
#' @param x numeric or logical vector.
#'
#' @param impute logical vector of length 1 specifying if missing values should
#'   be imputed with the mean of observed values of \code{x}. If TRUE (default),
#'   this will make sums over the same vectors with different amounts of missing
#'   data comparable.
#'
#' @param ov.min minimum frequency of observed values required. If \code{prop} =
#'   TRUE, then this is a decimal between 0 and 1. If \code{prop} = FALSE, then
#'   this is a integer between 0 and \code{length(x)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the sum should
#'   be calculated (rather than NA) if the frequency of observed values is
#'   exactly equal to \code{ov.min}.
#'
#' @return numeric vector of length 1 providing the sum of \code{x} or \code{NA}
#'   conditional on if the frequency of observed data is greater than (or equal
#'   to) \code{ov.min}.
#'
#' @seealso
#'    \code{\link{sum}}
#'    \code{\link{mean_if}}
#'    \code{\link{make.fun_if}}
#'
#' @examples
#' sum_if(x = airquality[[1]], ov.min = .75) # proportion of observed values
#' sum_if(x = airquality[[1]], ov.min = 116,
#'    prop = FALSE) # count of observe values
#' sum_if(x = airquality[[1]], ov.min = 116, prop = FALSE,
#'    inclusive = FALSE) # not include ov.min value itself
#' sum_if(x = c(TRUE, NA, FALSE, NA),
#'    ov.min = .50) # works with logical vectors as well as numeric
#' @export
sum_if <- function(x, impute = TRUE, ov.min = 1, prop = TRUE, inclusive = TRUE) {

   ov_count <- vecNA(x = x, prop = FALSE, ov = TRUE)
   if (prop) ov_min <- ov.min * length(x)
   if (!prop) ov_min <- ov.min
   if (inclusive) `%fun%` <- `>=`
   if (!inclusive) `%fun%` <- `>`
   if (ov_count %fun% ov_min) {
      if (impute) x[is.na(x)] <- mean(x = x, na.rm = TRUE) # mean.default
      return(sum(x, na.rm = TRUE))
   } else return(as.numeric(NA))
}

# mean_if #

#' Mean Conditional on Minimum Frequency of Observed Values
#'
#' \code{mean_if} calculates the mean of a numeric or logical vector conditional
#' on a specified minimum frequency of observed values. If the frequency of
#' observed values is less than (or equal to) \code{ov.min}, then \code{NA} is
#' returned rather than the mean.
#'
#' @param x numeric or logical vector.
#'
#' @param trim numeric vector of length 1 specifying the proportion of values
#'   from each end of \code{x} to trim. Trimmed values are recoded to their
#'   endpoint for calculation of the mean. See \code{mean.default}.
#'
#' @param ov.min minimum frequency of observed values required. If \code{prop} =
#'   TRUE, then this is a decimal between 0 and 1. If \code{prop} = FALSE, then
#'   this is a integer between 0 and \code{length(x)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the mean
#'   should be calculated if the frequency of observed values is exactly equal
#'   to \code{ov.min}.
#'
#' @return numeric vector of length 1 providing the mean of \code{x} or
#'   \code{NA} conditional on if the frequency of observed data is greater than
#'   (or equal to) \code{ov.min}.
#'
#' @seealso
#'    \code{\link{mean.default}}
#'    \code{\link{sum_if}}
#'    \code{\link{make.fun_if}}
#'
#' @examples
#' mean_if(x = airquality[[1]], ov.min = .75) # proportion of observed values
#' mean_if(x = airquality[[1]], ov.min = 116,
#'    prop = FALSE) # count of observe values
#' mean_if(x = airquality[[1]], ov.min = 116, prop = FALSE,
#'    inclusive = FALSE) # not include ov.min value itself
#' mean_if(x = c(TRUE, NA, FALSE, NA),
#'    ov.min = .50) # works with logical vectors as well as numeric
#' @export
mean_if <- function(x, trim = 0, ov.min = 1, prop = TRUE, inclusive = TRUE) {

   ov_count <- vecNA(x = x, prop = FALSE, ov = TRUE)
   if (prop) ov_min <- ov.min * length(x)
   if (!prop) ov_min <- ov.min
   if (inclusive) `%fun%` <- `>=`
   if (!inclusive) `%fun%` <- `>`
   if (ov_count %fun% ov_min) {
      return(mean(x, trim = trim, na.rm = TRUE)) # mean.default
   } else return(as.numeric(NA))
}

# make.fun_if #

#' Make a Function Conditional on Frequency of Observed Values
#'
#' \code{make.fun_if} makes a function that evaluates conditional on a specified
#' minimum frequency of observed values. Within the function, if the frequency
#' of observed values is less than (or equal to) \code{ov.min}, then
#' \code{false} is returned rather than the return value.
#'
#' @param fun function that takes an atomic vector as its first argument. The
#'   first argument does not have to be named "x" within \code{fun}, but it will
#'   be named "x" in the returned function.
#'
#' @param ... additional arguments with parameters to \code{fun}. This would be
#'   similar to \code{impute} in \code{sum_if}. However in the current version
#'   of \code{make.fun_if}, the parameters you provide will always be used
#'   within the returned function and cannot be specified by the user of the
#'   returned function. Unfortunately, I cannot figure out how to include
#'   user-specified arguments (with defaults) within the returned function other
#'   than \code{ov.min.default}, \code{prop.default}, and
#'   \code{inclusive.default}.
#'
#' @param ov.min.default numeric vector of length 1 specifying what the default
#'   should be for the argument \code{ov.min} within the returned function,
#'   which specifies the minimum frequency of observed values required. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{length(x)}.
#'
#' @param prop.default logical vector of length 1 specifying what the default
#'   should be for the argument \code{prop} within the returned function, which
#'   specifies whether \code{ov.min} should refer to the proportion of observed
#'   values (TRUE) or the count of observed values (FALSE).
#'
#' @param inclusive.default logical vector of length 1 speicfying what the
#'   default should be for the argument \code{inclusive} within the returned
#'   function, which specifies whether the function should be evaluated if the
#'   frequency of observed values is exactly equal to \code{ov.min}.
#'
#' @param false vector of length 1 specifying what should be returned if the
#'   observed values condition is not met within the returned function. The
#'   default is NA. Whatever the value is, it will be coerced to the same mode
#'   as \code{x} within the returned function.
#'
#' @return function that takes an atomic vector \code{x} as its first argument,
#'   \code{...} as other arguments, ending with \code{ov.min}, \code{prop}, and
#'   \code{inclusive} as final arguments with defaults specified by
#'   \code{ov.min.default}, \code{prop.default}, and \code{inclusive.default},
#'   respectively.
#'
#' @seealso
#'    \code{\link{sum_if}}
#'    \code{\link{mean_if}}
#'
#' @examples
#'
#' # SD
#' sd_if <- make.fun_if(fun = sd, na.rm = TRUE) # always have na.rm = TRUE
#' sd_if(x = airquality[[1]], ov.min = .75) # proportion of observed values
#' sd_if(x = airquality[[1]], ov.min = 116,
#'    prop = FALSE) # count of observed values
#' sd_if(x = airquality[[1]], ov.min = 116, prop = FALSE,
#'    inclusive = FALSE) # not include ov.min values itself
#'
#' # skewness
#' skew_if <- make.fun_if(fun = psych::skew, type = 1) # always have type = 1
#' skew_if(x = airquality[[1]], ov.min = .75) # proportion of observed values
#' skew_if(x = airquality[[1]], ov.min = 116,
#'    prop = FALSE) # count of observed values
#' skew_if(x = airquality[[1]], ov.min = 116, prop = FALSE,
#'    inclusive = FALSE) # not include ov.min values itself
#'
#' # mode
#' popular <- function(x) names(sort(table(x), decreasing = TRUE))[1]
#' popular_if <- make.fun_if(fun = popular) # works with character vectors too
#' popular_if(x = c(unlist(dimnames(HairEyeColor)), rep.int(x = NA, times = 10)),
#'    ov.min = .50)
#' popular_if(x = c(unlist(dimnames(HairEyeColor)), rep.int(x = NA, times = 10)),
#'    ov.min = .60)
#' @export
make.fun_if <- function(fun, ..., ov.min.default = 1, prop.default = TRUE,
                        inclusive.default = TRUE, false = NA) {

   fun <- match.fun(fun)
   fun_if <- function(x, ov.min = ov.min.default, prop = prop.default,
                      inclusive = inclusive.default) {

      ov_count <- vecNA(x = x, prop = FALSE, ov = TRUE)
      if (prop) ov_min <- ov.min * length(x)
      if (!prop) ov_min <- ov.min
      if (inclusive) `%fun%` <- `>=`
      if (!inclusive) `%fun%` <- `>`
      if (ov_count %fun% ov_min) {
         output <- fun(x, ...)
         return(output)
      } else {
         output <- false
         mode(output) <- mode(x)
         return(output)
      }
   }
   return(fun_if)
}

# rowSums_if #

#' Row Sums Conditional on Frequency of Observed Values
#'
#' \code{rowSums_if} calculates the sum of every row in a numeric or logical
#' matrix conditional on the frequency of observed data. If the frequency of
#' observed values in that row is less than (or equal to) that specified by
#' \code{ov.min}, then NA is returned for that row. It also has the option to
#' return a value other than 0 (e.g., NA) when all rows are NA, which differs
#' from \code{rowSums(x, na.rm = TRUE)}.
#'
#' Conceptually this function is doing: \code{apply(X = x, MARGIN = 1, FUN =
#' sum_if, ov.min = ov.min, prop = prop, inclusive = inclusive)}. But for
#' computational efficiency purposes it does not because then the observed
#' values conditioning would not be vectorized. Instead, it uses \code{rowSums}
#' and then inserts NAs for rows that have too few observed values.
#'
#' @param x numeric or logical matrix. If not a matrix, it will be coerced to
#'   one.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{ncol(x)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the sum should
#'   be calculated if the frequency of observed values in a row is exactly equal
#'   to \code{ov.min}.
#'
#' @param impute logical vector of length 1 specifying if missing values should
#'   be imputed with the mean of observed values of \code{x[i, ]}. If TRUE
#'   (default), this will make sums over the same columns with different amounts
#'   of observed data comparable.
#'
#' @param allNA numeric vector of length 1 specifying what value should be
#'   returned for rows that are all NA. This is most applicable when
#'   \code{ov.min = 0} and \code{inclusive = TRUE}. The default is NA, which
#'   differs from \code{rowSums} with \code{na.rm = TRUE} where 0 is returned.
#'   Note, the value is overwritten by NA if the frequency of observed values in
#'   that row is less than (or equal to) that specified by \code{ov.min}.
#'
#' @return numeric vector of length = \code{nrow(x)} with names =
#'   \code{rownames(x)} providing the sum of each row or NA (or \code{allNA})
#'   depending on the frequency of observed values.
#'
#' @seealso
#'    \code{\link{rowMeans_if}}
#'    \code{\link{colSums_if}}
#'    \code{\link{colMeans_if}}
#'    \code{\link{rowSums}}
#'
#' @examples
#' rowSums_if(airquality)
#' rowSums_if(x = airquality, ov.min = 5, prop = FALSE)
#' x <- data.frame("x" = c(1, 1, NA), "y" = c(2, NA, NA), "z" = c(NA, NA, NA))
#' rowSums_if(x)
#' rowSums_if(x, ov.min = 0)
#' rowSums_if(x, ov.min = 0, allNA = 0)
#' identical(x = rowSums(x, na.rm = TRUE),
#'    y = unname(rowSums_if(x, impute = FALSE, ov.min = 0, allNA = 0))) # identical to
#'    # rowSums(x, na.rm = TRUE)
#' @export
rowSums_if <- function(x, ov.min = 1, prop = TRUE, inclusive = TRUE,
   impute = TRUE, allNA = NA_real_) {

   if (!(is.matrix(x))) mat <- as.matrix(x, rownames.force = TRUE) # methods depends on input
   else mat <- x
   ov_count <- rowNA(x = mat, prop = FALSE, ov = TRUE)
   if (prop) ov_min <- ov.min * ncol(mat)
   if (!prop) ov_min <- ov.min
   if (inclusive) `%fun%` <- `<`
   if (!inclusive) `%fun%` <- `<=`
   if (impute) {
      tmp <- apply(X = mat, MARGIN = 1, function(row) {
         row[is.na(row)] <- mean(x = row, na.rm = TRUE)
         return(row)
      })
      mat <- t(tmp)
   }
   output <- rowSums(x = mat, na.rm = TRUE)
   output[ov_count == 0] <- allNA
   output[ov_count %fun% ov_min] <- NA_real_
   return(output)
}

# rowMeans_if #

#' Row Means Conditional on Frequency of Observed Values
#'
#' \code{rowMean_if} calculates the mean of every row in a numeric or logical
#' matrix conditional on the frequency of observed data. If the frequency of
#' observed values in that row is less than (or equal to) that specified by
#' \code{ov.min}, then NA is returned for that row.
#'
#' Conceptually this function does: \code{apply(X = x, MARGIN = 1, FUN =
#' mean_if, ov.min = ov.min, prop = prop, inclusive = inclusive)}. But for
#' computational efficiency purposes it does not because then the observed
#' values conditioning would not be vectorized. Instead, it uses \code{rowMeans}
#' and then inserts NAs for rows that have too few observed values
#'
#' @param x numeric or logical matrix. If not a matrix, it will be coerced to
#'   one.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{ncol(x)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the mean
#'   should be calculated if the frequency of observed values in a row is
#'   exactly equal to \code{ov.min}.
#'
#' @return numeric vector of length = \code{nrow(x)} with names =
#'   \code{rownames(x)} providing the mean of each row or NA depending on the
#'   frequency of observed values.
#'
#' @seealso
#'    \code{\link{rowSums_if}}
#'    \code{\link{colMeans_if}}
#'    \code{\link{colSums_if}}
#'    \code{\link{rowMeans}}
#'
#' @examples
#' rowMeans_if(airquality)
#' rowMeans_if(x = airquality, ov.min = 5, prop = FALSE)
#' @export
rowMeans_if <- function(x, ov.min = 1, prop = TRUE, inclusive = TRUE) {

   if (!(is.matrix(x))) mat <- as.matrix(x, rownames.force = TRUE) # methods depends on input
   else mat <- x
   ov_count <- rowNA(x = mat, prop = FALSE, ov = TRUE)
   if (prop) ov_min <- ov.min * ncol(mat)
   if (!prop) ov_min <- ov.min
   if (inclusive) `%fun%` <- `<`
   if (!inclusive) `%fun%` <- `<=`
   output <- rowMeans(x = mat, na.rm = TRUE)
   output[ov_count %fun% ov_min] <- as.numeric(NA)
   return(output)
}

# colSums_if #

#' Column Sums Conditional on Frequency of Observed Values
#'
#' \code{colSums_if} calculates the sum of every column in a numeric or logical
#' matrix conditional on the frequency of observed data. If the frequency of
#' observed values in that column is less than (or equal to) that specified by
#' \code{ov.min}, then NA is returned for that column. It also has the option to
#' return a value other than 0 (e.g., NA) when all columns are NA, which differs
#' from \code{colSums(x, na.rm = TRUE)}.
#'
#' Conceptually this function does: \code{apply(X = x, MARGIN = 2, FUN = sum_if,
#' ov.min = ov.min, prop = prop, inclusive = inclusive)}. But for computational
#' efficiency purposes it does not because then the observed values conditioning
#' would not be vectorized. Instead, it uses \code{colSums} and then inserts NAs
#' for columns that have too few observed values.
#'
#' @param x numeric or logical matrix. If not a matrix, it will be coerced to
#'   one.
#'
#' @param ov.min minimum frequency of observed values required per column. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{nrow(x)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the sum should
#'   be calculated if the frequency of observed values in a column is exactly
#'   equal to \code{ov.min}.
#'
#' @param impute logical vector of length 1 specifying if missing values should
#'   be imputed with the mean of observed values of \code{x[, i]}. If TRUE
#'   (default), this will make sums over the same rows with different amounts of
#'   observed data comparable.
#'
#' @param allNA numeric vector of length 1 specifying what value should be
#'   returned for columns that are all NA. This is most applicable when
#'   \code{ov.min = 0} and \code{inclusive = TRUE}. The default is NA, which
#'   differs from \code{colSums} with \code{na.rm = TRUE} where 0 is returned.
#'   Note, the value is overwritten by NA if the frequency of observed values in
#'   that column is less than (or equal to) that specified by \code{ov.min}.
#'
#' @return numeric vector of length = \code{ncol(x)} with names =
#'   \code{colnames(x)} providing the sum of each column or NA depending on the
#'   frequency of observed values.
#'
#' @seealso
#'    \code{\link{colMeans_if}}
#'    \code{\link{rowSums_if}}
#'    \code{\link{rowMeans_if}}
#'    \code{\link{colSums}}
#'
#' @examples
#' colSums_if(airquality)
#' colSums_if(x = airquality, ov.min = 150, prop = FALSE)
#' x <- data.frame("x" = c(1, 2, NA), "y" = c(1, NA, NA), "z" = c(NA, NA, NA))
#' colSums_if(x)
#' colSums_if(x, ov.min = 0)
#' colSums_if(x, ov.min = 0, allNA = 0)
#' identical(x = colSums(x, na.rm = TRUE),
#'    y = colSums_if(x, impute = FALSE, ov.min = 0, allNA = 0)) # identical to
#'    # colSums(x, na.rm = TRUE)
#' @export
colSums_if <- function(x, ov.min = 1, prop = TRUE, inclusive = TRUE,
   impute = TRUE, allNA = NA_real_) {

   if (!(is.matrix(x))) mat <- as.matrix(x, rownames.force = TRUE) # methods depends on input
   else mat <- x
   ov_count <- colNA(x = mat, prop = FALSE, ov = TRUE)
   if (prop) ov_min <- ov.min * ncol(mat)
   if (!prop) ov_min <- ov.min
   if (inclusive) `%fun%` <- `<`
   if (!inclusive) `%fun%` <- `<=`
   if (impute) {
      mat <- apply(X = mat, MARGIN = 2, function(col) {
         col[is.na(col)] <- mean(x = col, na.rm = TRUE)
         return(col)
      })
   }
   output <- colSums(x = mat, na.rm = TRUE)
   output[ov_count == 0] <- allNA
   output[ov_count %fun% ov_min] <- NA_real_
   return(output)
}

# colMeans_if #

#' Column Means Conditional on Frequency of Observed Values
#'
#' \code{colMeans_if} calculates the mean of every column in a numeric or
#' logical matrix conditional on the frequency of observed data. If the
#' frequency of observed values in that column is less than (or equal to) that
#' specified by \code{ov.min}, then NA is returned for that row.
#'
#' Conceptually this function does: \code{apply(X = x, MARGIN = 2, FUN =
#' mean_if, ov.min = ov.min, prop = prop, inclusive = inclusive)}. But for
#' computational efficiency purposes it does not because then the missing values
#' conditioning would not be vectorized. Instead, it uses \code{colMeans} and
#' then inserts NAs for columns that have too few observed values.
#'
#' @param x numeric or logical matrix. If not a matrix, it will be coerced to
#'   one.
#'
#' @param ov.min minimum frequency of observed values required per column. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{nrow(x)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the mean
#'   should be calculated if the frequency of observed values in a column is
#'   exactly equal to \code{ov.min}.
#'
#' @return numeric vector of length = \code{ncol(x)} with names =
#'   \code{colnames(x)} providing the mean of each column or NA depending on the
#'   frequency of observed values.
#'
#' @seealso
#'    \code{\link{colSums_if}}
#'    \code{\link{rowMeans_if}}
#'    \code{\link{rowSums_if}}
#'    \code{\link{colMeans}}
#'
#' @examples
#' colMeans_if(airquality)
#' colMeans_if(x = airquality, ov.min = 150, prop = FALSE)
#' @export
colMeans_if <- function(x, ov.min = 1, prop = TRUE, inclusive = TRUE) {

   if (!(is.matrix(x))) mat <- as.matrix(x, rownames.force = TRUE) # methods depends on input
   else mat <- x
   ov_count <- colNA(x = mat, prop = FALSE, ov = TRUE)
   if (prop) ov_min <- ov.min * ncol(mat)
   if (!prop) ov_min <- ov.min
   if (inclusive) `%fun%` <- `<`
   if (!inclusive) `%fun%` <- `<=`
   output <- colMeans(x = mat, na.rm = TRUE)
   output[ov_count %fun% ov_min] <- as.numeric(NA)
   return(output)
}

# SCORE ####

# score #

#' Observed Unweighted Scoring of a Set of Variables/Items
#'
#' \code{score} calculates observed unweighted scores across a set of variables/items.
#' If a row's frequency of observed data is less than (or equal to)
#' \code{ov.min}, then NA is returned for that row. \code{data[vrb.nm]} is
#' coerced to a matrix before scoring. If the coercion leads to a character
#' matrix, an error is returned.
#'
#' @param data data.frame or numeric/logical matrix
#'
#' @param vrb.nm character vector of colnames in \code{data} specifying the set
#'   of variables/items.
#'
#' @param avg logical vector of length 1 specifying whether mean scores (TRUE)
#'   or sum scores (FALSE) should be created.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and \code{length(vrb.nm)}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE).
#'
#' @param inclusive logical vector of length 1 specifying whether the score
#'   should be calculated (rather than NA) if the frequency of observed values
#'   in a row is exactly equal to \code{ov.min}.
#'
#' @param impute logical vector of length 1 specifying if missing values should
#'   be imputed with the mean of observed values from each row of
#'   \code{data[vrb.nm]} (i.e., row mean imputation). If TRUE (default), this
#'   will make sums over the same rows with different frequencies of missing
#'   values comparable. Note, this argument is only used when \code{avg} = FALSE
#'   since when \code{avg} = TRUE row mean imputation is always done implicitly.
#'
#' @param std logical vector of length 1 specifying whether 1)
#'   \code{data[vrb.nm]} should be standardized before scoring and 2) the score
#'   standardized after creation. This argument is for convenience as these two
#'   standardization processes are often used together. However, this argument
#'   will be overwritten by any non-default value for \code{std.data} and
#'   \code{std.score}.
#'
#' @param std.data logical vector of length 1 specifying whether
#'   \code{data[vrb.nm]} should be standardized before scoring.
#'
#' @param std.score logical vector of length 1 specifying whether the score
#'   should be standardized after creation.
#'
#' @return numeric vector of the mean/sum of each row or \code{NA} if the
#'   frequency of observed values is less than (or equal to) \code{ov.min}. The
#'   names are the rownames of \code{data}.
#'
#' @seealso
#'    \code{\link{scores}}
#'    \code{\link{rowMeans_if}}
#'    \code{\link{rowSums_if}}
#'    \code{\link[psych]{scoreItems}}
#'
#' @examples
#' score(data = attitude, vrb.nm = c("complaints","privileges","learning","raises"))
#' score(data = attitude, vrb.nm = c("complaints","privileges","learning","raises"),
#'    std = TRUE) # standardized scoring
#' score(data = airquality, vrb.nm = c("Ozone","Solar.R","Temp"),
#'    ov.min = 0.75) # conditional on observed values
#' @export
score <- function(data, vrb.nm, avg = TRUE, ov.min = 1, prop = TRUE, inclusive = TRUE,
   impute = TRUE, std = FALSE, std.data = std, std.score = std) {

   data_matrix <- as.matrix(x = data[vrb.nm], rownames.force = TRUE) # as.matrix.data.frame
   if (is.character(data_matrix)) stop("`data[vrb.nm]` was coerced to a character matrix; check for factors.")
   if (std.data) data_matrix <- scale(data_matrix)
   if (avg) output <- rowMeans_if(x = data_matrix, ov.min = ov.min, prop = prop, inclusive = inclusive)
   if (!avg) output <- rowSums_if(x = data_matrix, impute = impute, ov.min = ov.min, prop = prop, inclusive = inclusive)
   if (std.score) output <- setNames(object = as.vector(scale(output)), nm = rownames(data_matrix))
   return(output)
}

# scores #

#' Observed Unweighted Scoring of Multiple Sets of Variables/Items
#'
#' \code{scores} calculates observed unweighted scores across multiple sets of
#' variables/items. If a row's frequency of observed data is less than (or equal
#' to) \code{ov.min}, then NA is returned for that row. Each set of
#' variables/items are coerced to a matrix before scoring. If the coercion leads
#' to a character matrix, an error is returned. This can be tested with
#' \code{lapply(X = vrb.nm.list, FUN = function(nm)
#' is.character(as.matrix(data[nm])))}.
#'
#' @param data data.frame or numeric/logical matrix
#'
#' @param vrb.nm.list list where each element is a character vector of colnames
#'   in \code{data} specifying the variables/items for that score. The names  of
#'   \code{vrb.nm.list} will be the names of the scores in the return object.
#'
#' @param avg logical vector of length 1 specifying whether mean scores (TRUE)
#'   or sum scores (FALSE) should be created.
#'
#' @param ov.min minimum frequency of observed values required per row. If
#'   \code{prop} = TRUE, then this is a decimal between 0 and 1. If \code{prop}
#'   = FALSE, then this is a integer between 0 and
#'   \code{length(vrb.nm.list[[i]])}.
#'
#' @param prop logical vector of length 1 specifying whether \code{ov.min}
#'   should refer to the proportion of observed values (TRUE) or the count of
#'   observed values (FALSE). If the multiple sets of variables/items contain
#'   different numbers of variables, it probably makes the most sense to use the
#'   proportion of observed values (TRUE).
#'
#' @param inclusive logical vector of length 1 specifying whether the scores
#'   should be calculated (rather than NA) if the frequency of observed values
#'   in a row is exactly equal to \code{ov.min}.
#'
#' @param impute logical vector of length 1 specifying if missing values should
#'   be imputed with the mean of observed values from each row of
#'   \code{data[vrb.nm.list[[i]] ]} (i.e., row mean imputation). If TRUE
#'   (default), this will make sums over the same rows with different
#'   frequencies of missing values comparable. Note, this argument is only used
#'   when \code{avg} = FALSE since when \code{avg} = TRUE row mean imputation is
#'   always done implicitly.
#'
#' @param std logical vector of length 1 specifying whether 1) the variables
#'   should be standardized before scoring and 2) the score standardized after
#'   creation. This argument is for convenience as these two standardization
#'   processes are often used together. However, this argument will be
#'   overwritten by any non-default value for \code{std.data} and
#'   \code{std.score}.
#'
#' @param std.data logical vector of length 1 specifying whether the
#'   variables/items should be standardized before scoring.
#'
#' @param std.score logical vector of length 1 specifying whether the scores
#'   should be standardized after creation.
#'
#' @return data.frame of mean/sum scores with \code{NA} for any row with the
#'   frequency of observed values less than (or equal to) \code{ov.min}. The
#'   colnames are specified by \code{names(vrb.nm.list)} and rownames by
#'   \code{row.names(data)}.
#'
#' @seealso
#'    \code{\link{score}}
#'    \code{\link{rowMeans_if}}
#'    \code{\link{rowSums_if}}
#'    \code{\link[psych]{scoreItems}}
#'
#' @examples
#' list_colnames <- list("first" = c("rating","complaints","privileges"),
#'     "second" = c("learning","raises","critical"))
#' scores(data = attitude, vrb.nm.list = list_colnames)
#' list_colnames <- list("first" = c("Ozone","Wind"),
#'     "second" = c("Solar.R","Temp"))
#' scores(data = airquality, vrb.nm.list = list_colnames, ov.min = .50,
#'    inclusive = FALSE) # scoring conditional on observed values
#' @export
scores <- function(data, vrb.nm.list, avg = TRUE, ov.min = 1, prop = TRUE, inclusive = TRUE,
   impute = TRUE, std = FALSE, std.data = std, std.score = std) {

   tmp_scores <- lapply(X = vrb.nm.list, FUN = function(nm) {
      score(data = data, vrb.nm = nm, avg = avg, ov.min = ov.min, prop = prop,
         inclusive = inclusive, impute = impute,
         std = std, std.data = std.data, std.score = std.score)
   })
   output <- data.frame(tmp_scores, stringsAsFactors = FALSE)
   return(output)
}

# TRANSFORM ####

# center #

#' Centering and/or Standardizing a Numeric Vector
#'
#' \code{center} centers and/or standardized a numeric vector. It is an
#' alternative to \code{scale.default} that returns a numeric vector rather than
#' a numeric matrix.
#'
#' \code{center} first coerces \code{x} to a matrix in preparation for the call
#' to \code{scale.default}. If the coercion results in a non-numeric matrix
#' (e.g., \code{x} is a character vector or factor), then an error is returned.
#'
#' @param x numeric vector.
#'
#' @param center logical vector with length 1 specifying whether grand-mean
#'   centering should be done.
#'
#' @param scale logical vector with length 1 specifying whether grand-SD scaling
#'   should be done.
#'
#' @return numeric vector of \code{x} centered and/or standardized with the same
#'   names as \code{x}.
#'
#' @seealso
#'    \code{\link{centers}}
#'    \code{\link{center_by}}
#'    \code{\link{centers_by}}
#'    \code{\link{scale.default}}
#'
#' @examples
#' center(x = mtcars$"disp")
#' center(x = mtcars$"disp", scale = TRUE)
#' center(x = mtcars$"disp", center = FALSE, scale = TRUE)
#' center(x = setNames(mtcars$"disp", nm = row.names(mtcars)))
#' @export
center <- function(x, center = TRUE, scale = FALSE) {

   x_mat <- as.matrix(x) # method depends on the input
   if (!(is.numeric(x_mat)))
      stop("`x` was coerced to a non-numeric matrix. Check if `x` is typeof character or a factor.")
   centered <- scale.default(x = x_mat, center = center, scale = scale) # to prevent naming conflict between the function and argument
   output <- as.vector(centered)
   names(output) <- names(x)
   return(output)
}

# centers #

#' Centering and/or Standardizing Numeric Data
#'
#' \code{centers} centers and/or standardized data. It is an alternative to
#' \code{scale.default} that returns a data.frame rather than a numeric matrix.
#'
#' \code{centers} first coerces \code{data[vrb.nm]} to a matrix in preparation
#' for the call to \code{scale.default}. If the coercion results in a
#' non-numeric matrix (e.g., any columns in \code{data[vrb.nm]} are character
#' vectors or factors), then an error is returned.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param center logical vector with length 1 specifying whether grand-mean
#'   centering should be done.
#'
#' @param scale logical vector with length 1 specifying whether grand-SD scaling
#'   should be done.
#'
#' @param suffix character vector with a single element specifying the string to
#'   append to the end of the colnames of the return object. The default depends
#'   on the \code{center} and \code{scale} arguments: 1)if \code{center} = TRUE
#'   and \code{scale} = FALSE, then \code{suffix} = "_c", 2) if \code{center} =
#'   FALSE and \code{scale} = TRUE, then \code{suffix} = "_s", 3) if
#'   \code{center} = TRUE and \code{scale} = TRUE, then \code{suffix} = "_z", 4)
#'   if \code{center} = FALSE and \code{scale} = FALSE, then \code{suffix} = "".
#'
#' @return data.frame of centered and/or standardized variables with colnames
#'   specified by \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{center}}
#'    \code{\link{centers_by}}
#'    \code{\link{center_by}}
#'    \code{\link{scale.default}}
#'
#' @examples
#' centers(data = mtcars, vrb.nm = c("disp","hp","drat","wt","qsec"))
#' centers(data = mtcars, vrb.nm = c("disp","hp","drat","wt","qsec"),
#'    scale = TRUE)
#' centers(data = mtcars, vrb.nm = c("disp","hp","drat","wt","qsec"),
#'    center = FALSE, scale = TRUE)
#' centers(data = mtcars, vrb.nm = c("disp","hp","drat","wt","qsec"),
#'    scale = TRUE, suffix = "_std")
#' @export
centers <- function(data, vrb.nm, center = TRUE, scale = FALSE, suffix) {

   data_matrix <- as.matrix(data[vrb.nm]) # as.matrix.data.frame: no reason to add rownames.force argument because scale.default() calls as.matrix() without rownames.force argument to start the function
   if (!(is.numeric(data_matrix)))
      stop("`data`[`vrb.nm`] was coerced to a non-numeric matrix. Check if some variables are typeof character or factors.")
   Centered <- scale.default(x = data_matrix, center = center, scale = scale) # to prevent naming conflict between the function and argument
   output <- as.data.frame(Centered) # as.data.frame.matrix
   if (missing(suffix)) {
      if (!center & !scale) suffix <- ""
      if (center & !scale) suffix <- "_c"
      if (!center & scale) suffix <- "_s"
      if (center & scale) suffix <- "_z"
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# center_by #

#' Centering and/or Standardizing a Numeric Vector by Group
#'
#' \code{center_by} centers and/or standardized a numeric vector by group. This
#' is sometimes called group-mean centering and/or group-SD standardizing.
#'
#' \code{center_by} first coerces \code{x} to a matrix in preparation for the
#' core of the function, which is essentially: \code{lapply(X = split(x = x, f =
#' grp), FUN = scale.default)}. If the coercion results in a non-numeric matrix
#' (e.g., \code{x} is a character vector or factor), then an error is returned.
#' An error is also returned if \code{x} and the elements of \code{grp} do not
#' have the same length.
#'
#' @param x numeric vector.
#'
#' @param grp list of atomic vector(s) and/or factor(s) (e.g., data.frame)
#'   containing the groups. They should each have same length as \code{x}. It
#'   can also be an atomic vector or factor, which will then be made the first
#'   element of a list internally.
#'
#' @param center logical vector with length 1 specifying whether group-mean
#'   centering should be done.
#'
#' @param scale logical vector with length 1 specifying whether group-SD scaling
#'   should be done.
#'
#' @return numeric vector of \code{x} centered and/or standardized by group with
#'   the same names as \code{x}.
#'
#' @seealso
#'    \code{\link{centers_by}}
#'    \code{\link{center}}
#'    \code{\link{centers}}
#'    \code{\link{scale.default}}
#'
#' @examples
#' chick_data <- as.data.frame(ChickWeight) # because the "groupedData" class calls
#'    # `[.groupedData`, which is different than `[.data.frame`
#' center_by(x = ChickWeight[["weight"]], grp = ChickWeight[["Chick"]])
#' center_by(x = setNames(obj = ChickWeight[["weight"]], nm = row.names(ChickWeight)),
#'    grp = ChickWeight[["Chick"]]) # with names
#' tmp_nm <- c("Type","Treatment") # b/c Roxygen2 doesn't like a c() within a []
#' center_by(x = as.data.frame(CO2)[["uptake"]], grp = as.data.frame(CO2)[tmp_nm],
#'    scale = TRUE) # multiple grouping vectors
#' @export
center_by <- function(x, grp, center = TRUE, scale = FALSE) {

   x_mat <- as.matrix(x) # method depends on input (I don't actually need this for the function; it is just for error checking)
   if (!(is.numeric(x_mat)))
      stop("`x` was coerced to a non-numeric matrix. Check if `x` is typeof character or a factor.")
   if (!(is.list(grp))) grp <- list(grp)
   grp_len <- lapply(X = grp, FUN = length)
   if (!(all(length(x_mat) == unlist(grp_len))))
      stop("`x` and each element of `grp` must be the same length")
   x_by <- split(x = x_mat, f = grp) # split.default: no reason to make the factor myself, because if it is a list, unsplit() will automatrically remake the factor
   tmp_by <- lapply(X = x_by, FUN = scale.default, center = center, scale = scale)
   centered_by <- lapply(X = tmp_by, FUN = as.vector)
   output <- unsplit(value = centered_by, f = grp)
   names(output) <- names(x)
   return(output)
}

# centers_by #

#' Centering and/or Standardizing Numeric Data by Group
#'
#' \code{centers_by} centers and/or standardized data by group. This is sometimes
#' called group-mean centering and/or group-SD standardizing. The groups can be
#' specified by multiple columns in \code{data} (e.g., \code{grp.nm} with length
#' > 1), and \code{interaction} will be implicitly called to create the groups.
#'
#' \code{centers_by} first coerces \code{data[vrb.nm]} to a matrix in preparation
#' for the core of the function, which is essentially \code{lapply(X = split(x =
#' data[vrb.nm], f = data[grp.nm]), FUN = scale.default)} If the coercion
#' results in a non-numeric matrix (e.g., any columns in \code{data[vrb.nm]} are
#' character vectors or factors), then an error is returned.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param center logical vector with length 1 specifying whether group-mean
#'   centering should be done.
#'
#' @param scale logical vector with length 1 specifying whether group-SD scaling
#'   should be done.
#'
#' @param suffix character vector with a single element specifying the string to
#'   append to the end of the colnames of the return object. The default depends
#'   on the \code{center} and \code{scale} arguments: 1)if \code{center} = TRUE
#'   and \code{scale} = FALSE, then \code{suffix} = "_cw", 2) if \code{center} =
#'   FALSE and \code{scale} = TRUE, then \code{suffix} = "_sw", 3) if
#'   \code{center} = TRUE and \code{scale} = TRUE, then \code{suffix} = "_zw",
#'   4) if \code{center} = FALSE and \code{scale} = FALSE, then \code{suffix} =
#'   "".
#'
#' @return data.frame of centered and/or standardized variables by group with
#'   colnames specified by \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{center_by}}
#'    \code{\link{centers}}
#'    \code{\link{center}}
#'    \code{\link{scale.default}}
#'
#' @examples
#' ChickWeight2 <- as.data.frame(ChickWeight) # because the "groupedData" class calls
#'    # `[.groupedData`, which is different than `[.data.frame`
#' row.names(ChickWeight2) <- as.numeric(row.names(ChickWeight)) / 1000
#' centers_by(data = ChickWeight2, vrb.nm = c("weight","Time"), grp.nm = "Chick")
#' centers_by(data = ChickWeight2, vrb.nm = c("weight","Time"), grp.nm = "Chick",
#'    scale = TRUE, suffix = "_within")
#' centers_by(data = as.data.frame(CO2), vrb.nm = c("conc","uptake"),
#'    grp.nm = c("Type","Treatment"), scale = TRUE) # multiple grouping columns
#' @export
centers_by <- function(data, vrb.nm, grp.nm, center = TRUE, scale = FALSE, suffix) {

   data_matrix <- as.matrix(data[vrb.nm]) # as.matrix.data.frame: no reason to add rownames.force argument because scale.default() calls as.matrix() without rownames.force argument to start the function
   if (!(is.numeric(data_matrix)))
      stop("`data`[`vrb.nm`] was coerced to a non-numeric matrix. Check if some variables are typeof character or factors.")
   grp <- data[grp.nm]
   data_by <- split(x = data[vrb.nm], f = grp) # split.data.frame
   tmp_by <- lapply(X = data_by, FUN = scale.default, center = center, scale = scale)
   Centered_by <- lapply(X = tmp_by, FUN = as.data.frame)
   output <- unsplit(value = Centered_by, f = grp) # unsplit() returns the original data.frame order, while rbind.data.frame() does not
   if (missing(suffix)) {
      if (!center & !scale) suffix <- ""
      if (center & !scale) suffix <- "_cw"
      if (!center & scale) suffix <- "_sw"
      if (center & scale) suffix <- "_zw"
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)  # split() and unsplit() appear to retain rownames so I might not need to specify them, but I have been having problems with rownames so I am keeping it for now
   return(output)
}

# agg #

#' Aggregate an Atomic Vector by Group
#'
#' \code{agg} evaluates a function separately for each group and combines the
#' results back together into an atomic vector of data.frame that is returned.
#' Depending on the argument \code{rep}, the results of \code{fun} are repeated
#' for each element of \code{x} in the group (TRUE) or only once for each group
#' (FALSE). Depending on the argument \code{rtn.grp}, the return object is a
#' data.frame and the groups within \code{grp} are included in the data.frame as
#' columns (TRUE) or the return object is an atomic vector and the groups are
#' the names (FALSE).
#'
#' If \code{rep} = TRUE, then \code{agg} calls \code{ave}; if \code{rep} =
#' FALSE, then \code{agg} calls \code{aggregate}.
#'
#' @param x atomic vector.
#'
#' @param grp atomic vector or list of atomic vectors (e.g., data.frame)
#'   specifying the groups. The atomic vector(s) must be the length of \code{x}
#'   or else an error is returned.
#'
#' @param rep logical vector of length 1 specifying whether the result of
#'   \code{fun} should be repeated for every instance of the group in \code{x}
#'   (TRUE) or only once for each group (FALSE).
#'
#' @param rtn.grp logical vector of length 1 specifying whether the groups
#'   (i.e., \code{grp}) should be included in the return object as columns. The
#'   default is the opposite of \code{rep} as traditionally it is most important
#'   to return the group columns when \code{rep} = FALSE.
#'
#' @param sep character vector of length 1 specifying what string should
#'   separate different group values when naming the return object. This
#'   argument is only used if \code{grp} is a list of atomic vectors (e.g.,
#'   data.frame) AND \code{rep} = FALSE AND \code{rtn.grp} = FALSE.
#'
#' @param fun function to use for aggregation. This function is expected to
#'   return an atomic vector of length 1.
#'
#' @param ... additional named arguments to \code{fun}.
#'
#' @return result of \code{fun} applied to \code{x} for each group
#'   within \code{grp}. The structure of the return object depends on the
#'   arguments \code{rep} and \code{rtn.grp}:
#'
#' \describe{
#'   \item{If rep = TRUE and rtn.grp = TRUE:}{then the return
#'   object is a data.frame with nrow = \code{nrow(data)} where the first
#'   columns are \code{grp} and the last column is the result of \code{fun}. If
#'   \code{grp} is not a list with names, then its colnames will be "Group.1",
#'   "Group.2", "Group.3" etc. similar to \code{aggregate}'s return object. The
#'   colname for the result of \code{fun} will be "x".}
#'
#'   \item{If rep = TRUE and rtn.grp = FALSE:}{then the return
#'   object is an atomic vector with length = \code{length(x)} where the values
#'   are the result of \code{fun} and the names = \code{names(x)}.}
#'
#'   \item{If rep = FALSE and rtn.grp = TRUE:}{then the return
#'   object is a data.frame with nrow =
#'   \code{length(levels(interaction(grp)))}
#'   where the first columns are the unique group combinations in \code{grp} and
#'   the last column is the result of \code{fun}. If \code{grp} is not a list
#'   with names, then its colnames will be "Group.1", "Group.2", "Group.3" etc.
#'   similar to \code{aggregate}'s return object. The colname for the result of
#'   \code{fun} will be "x".}
#'
#'   \item{If rep = FALSE and rtn.grp = FALSE:}{then the return
#'   object is an atomic vector with length
#'   \code{length(levels(interaction(grp)))} where the values are the result of
#'   \code{fun} and the names are each group value pasted together by \code{sep}
#'   if there are multiple grouping variables within \code{grp} (i.e.,
#'   \code{is.list(grp) && length(grp) > 2}).}
#' }
#'
#' @seealso
#'    \code{\link{aggs}},
#'    \code{\link{agg_dfm}},
#'    \code{\link[stats]{ave}},
#'    \code{\link[stats]{aggregate}},
#'
#' @examples
#'
#' # one grouping variable
#' agg(x = airquality$"Solar.R", grp = airquality$"Month", fun = mean)
#' agg(x = airquality$"Solar.R", grp = airquality$"Month", fun = mean,
#'    na.rm = TRUE) # ignoring missing values
#' agg(x = setNames(airquality$"Solar.R", nm = row.names(airquality)), grp = airquality$"Month",
#'    fun = mean, na.rm = TRUE) # keeps the names in the return object
#' agg(x = airquality$"Solar.R", grp = airquality$"Month", rep = FALSE,
#'    fun = mean, na.rm = TRUE) # do NOT repeat aggregated values
#' agg(x = airquality$"Solar.R", grp = airquality$"Month", rep = FALSE, rtn.grp = FALSE,
#'    fun = mean, na.rm = TRUE) # groups are the names of the returned atomic vector
#'
#' # two grouping variables
#' tmp_nm <- c("vs","am") # Roxygen2 doesn't like a c() within a []
#' agg(x = mtcars$"mpg", grp = mtcars[tmp_nm], rep = TRUE, fun = sd)
#' agg(x = mtcars$"mpg", grp = mtcars[tmp_nm], rep = FALSE,
#'    fun = sd) # do NOT repeat aggregated values
#' agg(x = mtcars$"mpg", grp = mtcars[tmp_nm], rep = FALSE, rtn.grp = FALSE,
#'    fun = sd) # groups are the names of the returned atomic vector
#' agg(x = mtcars$"mpg", grp = mtcars[tmp_nm], rep = FALSE, rtn.grp = FALSE,
#'    sep = ".", fun = sd) # change the separater for naming
#'
#' # error messages
#' \dontrun{
#'    agg(x = airquality$"Solar.R", grp = mtcars[tmp_nm]) # error returned
#'    # b/c  atomic vectors within \code{grp} not having the same length as \code{x}
#' }
#'
#' @export
agg <- function(x, grp, rep = TRUE, rtn.grp = !rep, sep = "_", fun, ...) {

   if (!(is.list(grp))) grp <- list(grp) # for aggregate() to work
   grp_len <- lapply(X = grp, FUN = length)
   if (!(all(length(x) == grp_len))) stop("the atomic vectors within `grp` must all have the same length as `x`")
   fun <- match.fun(fun)
   if (rep) {
      output <- ave(x = x, grp, FUN = function(vec) fun(vec, ...))
      if (!rtn.grp) attributes(output) <- attributes(x)
      if (rtn.grp) {
         if (is.null(names(grp)))
            nm <- c(paste0("Group.", seq_along(grp)), "x")
         else
            nm <- c(names(grp), "x")
         output <- setNames(data.frame(list2DF(grp), output), nm = nm)
      }
   }
   if (!rep) {
      dfm <- aggregate(x = x, by = grp, simplify = TRUE, drop = FALSE,
         FUN = function(vec) fun(vec, ...)) # aggregate.default (which just calls aggregate.data.frame())
      if (rtn.grp) output <- dfm
      if (!rtn.grp) {
         output <- dfm[[ncol(dfm)]]
         args <- as.list(dfm[-ncol(dfm)]) # as.list.data.frame
         args[["sep"]] <- sep
         names(output) <- do.call(what = `paste`, args = args)
      }
   }
   return(output)
}

# aggs #

#' Aggregate Data by Group
#'
#' \code{aggs} evaluates a function separately for each group and combines the
#' results back together into a data.frame that is returned. Depending on
#' \code{rep}, the results of \code{fun} are repeated for each element of
#' \code{data[vrb.nm]} in the group (TRUE) or only once for each group (FALSE).
#' Note, \code{aggs} evaluates \code{fun} separately for each variable
#' \code{vrb.nm} within \code{data}. If instead, you want to evaluate \code{fun}
#' for variables as a set \code{data[vrb.nm]}, then use \code{agg_dfm}.
#'
#' If \code{rep} = TRUE, then \code{agg} calls \code{ave}; if \code{rep} =
#' FALSE, then \code{agg} calls \code{aggregate}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param rep logical vector of length 1 specifying whether the result of
#'   \code{fun} should be repeated for every instance of the group in
#'   \code{data[vrb.nm]} (TRUE) or only once for each group (FALSE).
#'
#' @param rtn.grp logical vector of length 1 specifying whether the group
#'   columns (i.e., \code{data[grp.nm]}) should be included in the return object
#'   as columns. The default is the opposite of \code{rep} as traditionally it
#'   is most important to return the group columns when \code{rep} = FALSE.
#'
#' @param sep character vector of length 1 specifying what string should
#'   separate different group values when naming the return object. This
#'   argument is only used if \code{grp.nm} has length > 1 AND \code{rep} =
#'   FALSE AND \code{rtn.grp} = FALSE.
#'
#' @param suffix character vector of length 1 specifying the string to append to
#'   the end of the colnames in the return object.
#'
#' @param fun function to use for aggregation. This function is expected to
#'   return an atomic vector of length 1.
#'
#' @param ... additional named arguments to \code{fun}.
#'
#' @return data.frame of aggregated values. If \code{rep} is TRUE, then nrow =
#'   \code{nrow(data)}. If \code{rep} = FALSE, then nrow =
#'   \code{length(levels(interaction(data[grp.nm])))}. The names are specified
#'   by \code{paste0(vrb.nm, suffix)}. If \code{rtn.grp} = TRUE, then the group
#'   columns are appended to the begining of the data.frame:
#'
#' \describe{
#'   \item{If rep = TRUE and rtn.grp = TRUE:}{then the return
#'   object is a data.frame with nrow = \code{nrow(data)} where the first
#'   columns are \code{grp} and the last column is the result of \code{fun}. If
#'   \code{grp} is not a list with names, then its colnames will be "Group.1",
#'   "Group.2", "Group.3" etc. similar to \code{aggregate}'s return object. The
#'   colname for the result of \code{fun} will be "x".}
#'
#'   \item{If rep = TRUE and rtn.grp = FALSE:}{then the return
#'   object is an atomic vector with length = \code{length(x)} where the values
#'   are the result of \code{fun} and the names = \code{names(x)}.}
#'
#'   \item{If rep = FALSE and rtn.grp = TRUE:}{then the return
#'   object is a data.frame with nrow =
#'   \code{length(levels(interaction(grp)))}
#'   where the first columns are the unique group combinations in \code{grp} and
#'   the last column is the result of \code{fun}. If \code{grp} is not a list
#'   with names, then its colnames will be "Group.1", "Group.2", "Group.3" etc.
#'   similar to \code{aggregate}'s return object. The colname for the result of
#'   \code{fun} will be "x".}
#'
#'   \item{If rep = FALSE and rtn.grp = FALSE:}{then the return
#'   object is an atomic vector with length
#'   \code{length(levels(interaction(grp)))} where the values are the result of
#'   \code{fun} and the names are each group value pasted together by \code{sep}
#'   if there are multiple grouping variables within \code{grp} (i.e.,
#'   \code{is.list(grp) && length(grp) > 2}).}
#' }
#'
#' @seealso
#'    \code{\link{agg}},
#'    \code{\link{agg_dfm}},
#'    \code{\link[stats]{ave}},
#'    \code{\link[stats]{aggregate}},
#'
#' @examples
#' aggs(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    fun = mean, na.rm = TRUE)
#' aggs(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    rtn.grp = TRUE, fun = mean, na.rm = TRUE) # include the group columns
#' aggs(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    rep = FALSE, fun = mean, na.rm = TRUE) # do NOT repeat aggregated values
#' aggs(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = FALSE, fun = mean, na.rm = TRUE) # with multiple group columns
#' aggs(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = FALSE, rtn.grp = FALSE, fun = mean, na.rm = TRUE) # without returning groups
#' @export
aggs <- function(data, vrb.nm, grp.nm, rep = TRUE, rtn.grp = !rep, sep = "_", suffix = "_a",
   fun, ...) {

   grp <- data[grp.nm]
   fun <- match.fun(fun)
   if (rep) {
      tmp_ave <- lapply(X = data[vrb.nm], FUN = function(vec) {
         ave(x = vec, grp, FUN = function(vec_by) fun(vec_by, ...))
      })
      output <- data.frame(tmp_ave, stringsAsFactors = FALSE)
      if (rtn.grp) output <- cbind(grp, output) # cbind.data.frame
   }
   if (!rep) {
      output <- aggregate(x = data[vrb.nm], by = grp, simplify = TRUE, drop = FALSE,
         FUN = function(vec) fun(vec, ...)) # aggregate.data.frame
      if (!rtn.grp) {
         grp_val <- output[, seq.int(from = 1L, to = length(grp.nm)), drop = FALSE]
         output <- output[, -(seq.int(from = 1L, to = length(grp.nm))), drop = FALSE]
         paste_args <- c(grp_val, "sep" = sep)
         row.names(output) <- do.call(what = `paste`, args = paste_args)
      }
   }
   names(output)[!(is.element(el = names(output), set = grp.nm))] <- paste0(vrb.nm, suffix)
   return(output)
}

# ave_dfm #

#' Repeated Group Statistics for a Data-Frame
#'
#' \code{ave_dfm} evaluates a function on a set of variables \code{vrb.nm}
#' separately for each group within \code{grp.nm}. The results are combined back
#' together in line with the rows of \code{data} similar to \code{\link{ave}}.
#' \code{ave_dfm} is different than \code{ave} or \code{agg} because it operates
#' on a data.frame, not an atomic vector.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames in \code{data} specifying the
#'   variables to use for the aggregation function \code{fun}.
#'
#' @param grp.nm character vector of colnames in \code{data} specifying the
#'   grouping variables.
#'
#' @param fun function that returns an atomic vector of length 1. Probably makes
#'   sense to ensure the function always returns the same typeof as well.
#'
#' @param ... additional named arguments to \code{fun}.
#'
#' @return atomic vector of length = \code{nrow(data)} providing the result of
#'   the function \code{fun} for the subset of data with that group value (i.e.,
#'   \code{data[levels(interaction(data[grp.nm]))[i], vrb.nm]}) for that row.
#'
#' @seealso
#'    \code{\link[stats]{ave}} for the same functionality with atomic vector inputs
#'    \code{\link{agg_dfm}} for similar functionality with data.frames, but can return
#'       the result for each group once rather than repeating the result for each group
#'       value in the data.frame
#'
#' @examples
#'
#' # one grouping variables
#' ave_dfm(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    fun = function(dat) cor(dat, use = "complete")[1,2])
#'
#' # two grouping variables
#' ave_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    fun = nrow) # with multiple group columns
#'
#' @export
ave_dfm <- function(data, vrb.nm, grp.nm, fun, ...) {

   # export this function for ease of use, but don't emphasize it
   dat <- data[vrb.nm]
   grp <- data[grp.nm]
   result_by <- lapply(X = split(x = dat, f = grp), # split.data.frame
      FUN = fun, ...)
   rtn <- setNames(unsplit(value = result_by, f = grp), nm = row.names(data))
   return(rtn)
}

# agg_dfm #

#' Data Information by Group
#'
#' \code{agg_dfm} evaluates a function on a set of variables in a data.frame
#' separately for each group and combines the results back together. The
#' \code{rep} and \code{rtn.grp} arguments determine exactly how the results are
#' combined together. If \code{rep} = TRUE, then the result of \code{fun} is
#' repeated for every row of the group in \code{data[grp.nm]}; If \code{rep} =
#' FALSE, then the result of \code{fun} for each unique combination of
#' \code{data[grp.nm]} is returned once. If \code{rtn.grp} = TRUE, then the
#' results are returned in a data.frame where the first columns are the groups
#' from \code{data[grp.nm]}; If \code{rtn.grp} = FALSE, then the results are
#' returned in an atomic vector. Note, \code{agg_dfm} evaluates \code{fun} on
#' all the variables in \code{data[vrb.nm]} as a whole, If instead, you want to
#' evaluate \code{fun} separately for variable \code{vrb.nm} in \code{data},
#' then use \code{Agg}.
#'
#' If \code{rep} = TRUE, then \code{agg_dfm} calls \code{ave_dfm}; if \code{rep}
#' = FALSE, then \code{agg_dfm} calls \code{by}. When \code{rep} = FALSE and
#' \code{rtn.grp} = TRUE, \code{agg_dfm} is very similar to \code{plyr::ddply};
#' when \code{rep} = FALSE and \code{rtn.grp} = FALSE, then \code{agg_dfm} is
#' very similar to \code{plyr::daply}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   set of variables to evaluate \code{fun} on.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param rep logical vector of length 1 specifying whether the result of
#'   \code{fun} should be repeated for every instance of the group in
#'   \code{data[vrb.nm]} (TRUE) or only once for each group (FALSE).
#'
#' @param rtn.grp logical vector of length 1 specifying whether the group
#'   columns (i.e., \code{data[grp.nm]}) should be included in the return object
#'   as columns. The default is the opposite of \code{rep} as traditionally it
#'   is most important to return the group columns when \code{rep} = FALSE.
#'
#' @param sep character vector of length 1 specifying the string to paste the
#'   group values together with when there are multiple grouping variables
#'   (i.e., \code{length(grp.nm) > 1}). Only used if \code{rep} = FALSE and
#'   \code{rtn.grp} = FALSE.
#'
#' @param rtn.result.nm character vector of length 1 specifying the name for the
#'   column of results in the return object. Only used if \code{rtn.grp} = TRUE.
#'
#' @param fun function to evaluate each grouping of \code{data[vrb.nm]} by. This
#'   function must return an atomic vector of length 1. If not, then consider
#'   using \code{by2} or \code{plyr::dlply}.
#'
#' @param ... additional named arguments to \code{fun}.
#'
#' @return result of \code{fun} applied to each grouping of
#'   \code{data[vrb.nm]}. The structure of the return object depends on the
#'   arguments \code{rep} and \code{rtn.grp}.
#'
#' \describe{
#'   \item{If rep = TRUE and rtn.grp = TRUE:}{then the return
#'   object is a data.frame with nrow = \code{nrow(data)} where the first
#'   columns are \code{data[grp.nm]} and the last column is the result of
#'   \code{fun} with colname = \code{rtn.result.nm}.}
#'
#'   \item{If rep = TRUE and rtn.grp = FALSE:}{then the return
#'   object is an atomic vector with length = \code{nrow(data)} where the values
#'   are the result of \code{fun} and the names = \code{row.names(data)}.}
#'
#'   \item{If rep = FALSE and rtn.grp = TRUE:}{then the return
#'   object is a data.frame with nrow =
#'   \code{length(levels(interaction(data[grp.nm])))} where the first columns
#'   are the unique group combinations in \code{data[grp.nm]} and the last
#'   column is the result of \code{fun} with colname = \code{rtn.result.nm}.}
#'
#'   \item{If rep = FALSE and rtn.grp = FALSE:}{then the return
#'   object is an atomic vector with length
#'   \code{length(levels(interaction(data[grp.nm])))} where the values are the
#'   result of \code{fun} and the names are each group value pasted together by
#'   \code{sep} if there are multiple grouping variables (i.e.,
#'   \code{length(grp.nm)} > 2).}
#'  }
#'
#' @seealso
#'    \code{\link{agg}}
#'    \code{\link{aggs}}
#'    \code{\link{by2}}
#'    \code{\link[plyr]{ddply}}
#'    \code{\link[plyr]{daply}}
#'
#' @examples
#'
#' ### one grouping variable
#'
#' ## by in base R
#' by(data = airquality[c("Ozone","Solar.R")], INDICES = airquality["Month"],
#'    simplify = FALSE, FUN = function(dat) cor(dat, use = "complete")[1,2])
#'
#' ## rep = TRUE
#'
#' # rtn.group = TRUE
#' agg_dfm(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    rep = TRUE, rtn.grp = TRUE, fun = function(dat) cor(dat, use = "complete")[1,2])
#'
#' # rtn.group = FALSE
#' agg_dfm(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    rep = TRUE, rtn.grp = FALSE, fun = function(dat) cor(dat, use = "complete")[1,2])
#'
#' ## rep = FALSE
#'
#' # rtn.group = TRUE
#' agg_dfm(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    rep = FALSE, rtn.grp = TRUE, fun = function(dat) cor(dat, use = "complete")[1,2])
#' suppressWarnings(plyr::ddply(.data = airquality[c("Ozone","Solar.R","Month")],
#'    .variables = "Month", .fun = function(dat) cor(dat, use = "complete")[1,2]))
#'
#' # rtn.group = FALSE
#' agg_dfm(data = airquality, vrb.nm = c("Ozone","Solar.R"), grp.nm = "Month",
#'    rep = FALSE, rtn.grp = FALSE, fun = function(dat) cor(dat, use = "complete")[1,2])
#' suppressWarnings(plyr::daply(.data = airquality[c("Ozone","Solar.R","Month")],
#'    .variables = "Month", .fun = function(dat) cor(dat, use = "complete")[1,2]))
#'
#' ### two grouping variables
#'
#' ## by in base R
#' by(data = mtcars[c("mpg","cyl","disp")], INDICES = mtcars[c("vs","am")],
#'    FUN = nrow, simplify = FALSE) # with multiple group columns
#'
#' ## rep = TRUE
#'
#' # rtn.grp = TRUE
#' agg_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = TRUE, rtn.grp = TRUE, fun = nrow)
#'
#' # rtn.grp = FALSE
#' agg_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = TRUE, rtn.grp = FALSE, fun = nrow)
#'
#' ## rep = FALSE
#'
#' # rtn.grp = TRUE
#' agg_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = FALSE, rtn.grp = TRUE, fun = nrow)
#' agg_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = FALSE, rtn.grp = TRUE, rtn.result.nm = "value", fun = nrow)
#'
#' # rtn.grp = FALSE
#' agg_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = FALSE, rtn.grp = FALSE, fun = nrow)
#' agg_dfm(data = mtcars, vrb.nm = c("mpg","cyl","disp"), grp.nm = c("vs","am"),
#'    rep = FALSE, rtn.grp = FALSE, sep = "_", fun = nrow)
#'
#' @export
agg_dfm <- function(data, vrb.nm, grp.nm, rep = FALSE, rtn.grp = !rep,
   sep = ".", rtn.result.nm = "result", fun, ...) {

   # sep only used if rep = TRUE and rtn.grp = FALSE
   # rtn.result.nm is only used if rtn.grp = TRUE
   grp <- data[grp.nm]
   fun <- match.fun(fun)
   if (rep) {
      output <- ave_dfm(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm,
         fun = fun, ...)
      if (!rtn.grp) {
         names(output) <- row.names(data)
      }
      if (rtn.grp) {
         tmp <- setNames(data.frame(output), nm = rtn.result.nm)
         output <- cbind(grp, tmp) # cbind.data.frame
         row.names(output) <- row.names(data)
      }
   }
   if (!rep) {
      by_list <- by(data[vrb.nm], INDICES = grp, simplify = FALSE,
         FUN = fun, ...)
      by_vec <- unlist(str2str::undim(by_list))
      if (!rtn.grp) {
         grp_nm <- levels(do.call(what = `interaction`, # for some reason, need to add levels()
            args = c(dimnames(by_list), list("sep" = sep))))
         output <- setNames(by_vec, nm = grp_nm)
      }
      if (rtn.grp) {
         grp_nm <- expand.grid(dimnames(by_list))
         by_dfm <- setNames(data.frame(by_vec), nm = rtn.result.nm)
         output <- cbind(grp_nm, by_dfm) # cbind.data.frame
         row.names(output) <- seq.int(from = 1L, to = nrow(output), by = 1L)
      }
   }
   return(output)
}

# shift #

#' Shift a Vector (i.e., lag/lead)
#'
#' \code{shift} shifts elements of a vector right (\code{n} < 0) for lags or
#' left (\code{n} > 0) for leads replacing the undefined data with a
#' user-defined value (e.g., NA). The number of elements shifted is equal to
#' \code{abs(n)}. It is assumed that \code{x} is already sorted by time such
#' that the first element is earliest in time and the last element is the latest
#' in time.
#'
#' If \code{n} is negative, then \code{shift} inserts \code{undefined} into the
#' first \code{abs(n)} elements of \code{x}, shifting all other values of
#' \code{x} to the right \code{abs(n)} positions, and then dropping the last
#' \code{abs(n)} elements of \code{x} to preserve the original length of
#' \code{x}. If \code{n} is positive, then \code{shift} drops the first
#' \code{abs(n)} elements of \code{x}, shifting all other values of \code{x}
#' left \code{abs(n)} positions, and then inserts \code{undefined} into the last
#' \code{abs(n)} elements of \code{x} to preserve the original length of
#' \code{x}. If \code{n} is zero, then \code{shift} simply returns \code{x}.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shift} tries to circumvent this
#' issue by a call to \code{round} within \code{shift} if \code{n} is not an
#' integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shift} truncates rather than rounds.
#'
#' @param x atomic vector or list vector.
#'
#' @param n integer vector with length 1. Specifies the direction and magnitude
#'   of the shift. See details.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See details.
#'
#' @return an atomic vector of the same length as \code{x} that is shifted. If
#'   \code{x} and \code{undefined} are different typeofs, then the return will
#'   be coerced to the more complex typeof (i.e., complex to simple: character,
#'   double, integer, logical).
#'
#' @seealso
#'    \code{\link{shifts}}
#'    \code{\link{shift_by}}
#'    \code{\link{shifts_by}}
#'
#' @examples
#' shift(x = attitude[[1]], n = -1L) # use L to prevent problems with floating point numbers
#' shift(x = attitude[[1]], n = -2L) # can specify any integer up to the length of `x`
#' shift(x = attitude[[1]], n = +1L) # can specify negative or positive integers
#' shift(x = attitude[[1]], n = +2L, undefined = -999) # user-specified indefined value
#' shift(x = setNames(object = letters, nm = LETTERS), n = 3L) # names are kept
#' @export
shift <- function(x, n, undefined = NA){

   if (!is.integer(n)) n <- round(n)
   if (abs(n) > length(x)) stop("abs(`n`) is greater than length(`x`)")
   # TODO: consider if you want to allow for a warning vs. error vs. NA if abs(n) > length(x): could have an argument that changes this
   if (n < 0L) { # lag
      i <- seq.int(from = length(x) - abs(n) + 1, to = length(x))
      output <- c(rep.int(x = undefined, times = abs(n)), x[-1 * i])
   }
   if (n > 0L)  { # lead
      i <- seq.int(from = 1, to = abs(n))
      output <- c(x[-1 * i], rep.int(x = undefined, times = abs(n)))
   }
   if (n == 0L) output <- x
   names(output) <- names(x)
   return(output)
}

# shifts #

#' Shift Data (i.e., lag/lead)
#'
#' \code{shifts} shifts rows of data down (\code{n} < 0) for lags or up (\code{n}
#' > 0) for leads replacing the undefined data with a user-defined value (e.g.,
#' NA). The number of rows shifted is equal to \code{abs(n)}. It is assumed that
#' \code{data[vrb.nm]} is already sorted by time such that the first row is
#' earliest in time and the last row is the latest in time.
#'
#' If \code{n} is negative, then \code{shifts} inserts \code{undefined} into the
#' first \code{abs(n)} rows of \code{data[vrb.nm]}, shifting all other rows of
#' \code{x} down \code{abs(n)} positions, and then dropping the last
#' \code{abs(n)} row of \code{data[vrb.nm]} to preserve the original nrow of
#' \code{data}. If \code{n} is positive, then \code{shifts} drops the first
#' \code{abs(n)} rows of \code{x}, shifting all other rows of
#' \code{data[vrb.nm]} up \code{abs(n)} positions, and then inserts
#' \code{undefined} into the last \code{abs(n)} rows of \code{x} to preserve the
#' original length of \code{data}. If \code{n} is zero, then \code{shifts} simply
#' returns \code{data[vrb.nm]}.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shifts} tries to circumvent this
#' issue by a call to \code{round} within \code{shifts} if \code{n} is not an
#' integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shifts} truncates rather than rounds.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param n integer vector of length 1. Specifies the direction and magnitude of
#'   the shift. See details.
#'
#' @param suffix character vector of length 1 specifying the string to append to
#'   the end of the colnames of the return object. The default depends on the
#'   \code{n} argument: 1) if \code{n} < 0, then \code{suffix} =
#'   \code{paste0("_g", -n)}, 2) if \code{n} > 0, then \code{suffix} =
#'   \code{paste0("_d", +n)}, 3) if \code{n} = 0, then \code{suffix} = "".
#'
#' @param undefined atomic vector of length 1 (probably makes sense to be the
#'   same typeof as the vectors in \code{data[vrb.nm]}). Specifies what to
#'   insert for undefined values after the shifting takes place. See details.
#'
#' @return data.frame of shifted data with colnames specified by \code{suffix}.
#'
#' @seealso
#'    \code{\link{shift}}
#'    \code{\link{shifts_by}}
#'    \code{\link{shift_by}}
#'
#' @examples
#' shifts(data = attitude, vrb.nm = colnames(attitude), n = -1L)
#' shifts(data = mtcars, vrb.nm = colnames(mtcars), n = 2L)
#' @export
shifts <- function(data, vrb.nm, n, undefined = NA, suffix) {

   shifted <- lapply(X = data[vrb.nm], FUN = shift, n = n, undefined = undefined)
   output <- data.frame(shifted, stringsAsFactors = FALSE)
   if (missing(suffix)) {
      if (n < 0L) suffix <- paste0("_g", -n)
      if (n > 0L) suffix <- paste0("_d", +n)
      if (n == 0L) suffix <- ""
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# shift_by #

#' Shift a Vector (i.e., lag/lead) by Group
#'
#' \code{shift_by} shifts elements of a vector right (\code{n} < 0) for lags or
#' left (\code{n} > 0) for leads by group, replacing the undefined data with a
#' user-defined value (e.g., NA). The number of elements shifted is equal to
#' \code{abs(n)}. It is assumed that \code{x} is already sorted within each
#' group by time such that the first element for that group is earliest in time
#' and the last element for that group is the latest in time.
#'
#' If \code{n} is negative, then \code{shift_by} inserts \code{undefined} into the
#' first \code{abs(n)} elements of \code{x} for each group, shifting all other
#' values of \code{x} to the right \code{abs(n)} positions, and then dropping
#' the last \code{abs(n)} elements of \code{x} to preserve the original length
#' of each group. If \code{n} is positive, then \code{shift_by} drops the first
#' \code{abs(n)} elements of \code{x} for each group, shifting all other values
#' of \code{x} left \code{abs(n)} positions, and then inserts \code{undefined}
#' into the last \code{abs(n)} elements of \code{x} to preserve the original
#' length of each group. If \code{n} is zero, then \code{shift_by} simply returns
#' \code{x}.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shift_by} tries to circumvent this
#' issue by a call to \code{round} within \code{shift_by} if \code{n} is not an
#' integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shift_by} truncates rather than rounds.
#'
#' @param x atomic vector or list vector.
#'
#' @param grp list of atomic vector(s) and/or factor(s) (e.g., data.frame),
#'   which each have same length as \code{x}. It can also be an atomic vector or
#'   factor, which will then be made the first element of a list internally.
#'
#' @param n integer vector with length 1. Specifies the direction and magnitude
#'   of the shift. See details.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See details.
#'
#' @return an atomic vector of the same length as \code{x} that is shifted by
#'   group. If \code{x} and \code{undefined} are different typeofs, then the
#'   return will be coerced to the most complex typeof (i.e., complex to simple:
#'   character, double, integer, logical).
#'
#' @seealso
#'    \code{\link{shifts_by}}
#'    \code{\link{shift}}
#'    \code{\link{shifts}}
#'
#' @examples
#' shift_by(x = ChickWeight[["Time"]], grp = ChickWeight[["Chick"]], n = -1L)
#' tmp_nm <- c("vs","am") # b/c Roxygen2 doesn't like c() in a []
#' shift_by(x = mtcars[["disp"]], grp = mtcars[tmp_nm], n = 1L)
#' tmp_nm <- c("Type","Treatment") # b/c Roxygen2 doesn't like c() in a []
#' shift_by(x = as.data.frame(CO2)[["uptake"]], grp = as.data.frame(CO2)[tmp_nm],
#'    n = 2L) # multiple grouping vectors
#' @export
shift_by <- function(x, grp, n, undefined = NA) {

   if (!(is.list(grp))) grp <- list(grp)
   grp_len <- lapply(X = grp, FUN = length)
   if (!(all(length(x) == unlist(grp_len))))
      stop("`x` and each element of `grp` must be the same length")
   x_by <- split(x = x, f = grp) # split.default: no reason to make the factor `f` myself, because unsplit() will remake the vector in the original order either way
   shifted_by <- lapply(X = x_by, FUN = shift, n = n, undefined = undefined)
   output <- unsplit(value = shifted_by, f = grp)
   names(output) <- names(x)
   return(output)
}

# shifts_by #

#' Shift Data (i.e., lag/lead) by Group
#'
#' \code{shifts_by} shifts rows of data down (\code{n} < 0) for lags or up (\code{n}
#' > 0) for leads replacing the undefined data with a user-defined value (e.g.,
#' NA). The number of rows shifted is equal to \code{abs(n)}. It is assumed that
#' \code{data[vrb.nm]} is already sorted within each group by time such that the
#' first row for that group is earliest in time and the last row for that group
#' is the latest in time. The groups can be specified by multiple columns in
#' \code{data} (e.g., \code{grp.nm} with length > 1), and \code{interaction}
#' will be implicitly called to create the groups.
#'
#' If \code{n} is negative, then \code{shifts_by} inserts \code{undefined} into
#' the first \code{abs(n)} rows of \code{data[vrb.nm]} for each group, shifting
#' all other rows of \code{x} down \code{abs(n)} positions, and then dropping
#' the last \code{abs(n)} row of \code{data[vrb.nm]} to preserve the original
#' nrow of each group. If \code{n} is positive, then \code{shifts_by} drops the
#' first \code{abs(n)} rows of \code{x} for each group, shifting all other rows
#' of \code{data[vrb.nm]} up \code{abs(n)} positions, and then inserts
#' \code{undefined} into the last \code{abs(n)} rows of \code{x} to preserve the
#' original length of each group. If \code{n} is zero, then \code{shifts_by}
#' simply returns \code{data[vrb.nm]}.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shifts_by} tries to circumvent
#' this issue by a call to \code{round} within \code{shifts_by} if \code{n} is
#' not an integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shifts_by} truncates rather than
#' rounds.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param n integer vector of length 1. Specifies the direction and magnitude of
#'   the shift. See details.
#'
#' @param suffix character vector of length 1 specifying the string to append to
#'   the end of the colnames of the return object. The default depends on the
#'   \code{n} argument: 1) if \code{n} < 0, then \code{suffix} =
#'   \code{paste0("_gw", -n)}, 2) if \code{n} > 0, then \code{suffix} =
#'   \code{paste0("_dw", +n)}, 3) if \code{n} = 0, then \code{suffix} = "".
#'
#' @param undefined atomic vector of length 1 (probably makes sense to be the
#'   same typeof as the vectors in \code{data[vrb.nm]}). Specifies what to
#'   insert for undefined values after the shifting takes place. See details.
#'
#' @return data.frame of shifted data by group with colnames specified by
#'   \code{suffix}.
#'
#' @seealso
#'    \code{\link{shift_by}}
#'    \code{\link{shifts}}
#'    \code{\link{shift}}
#'
#' @examples
#' shifts_by(data = ChickWeight, vrb.nm = c("weight","Time"), grp.nm = "Chick", n = -1L)
#' shifts_by(data = mtcars, vrb.nm = c("disp","mpg"), grp.nm = c("vs","am"), n = 1L)
#' shifts_by(data = as.data.frame(CO2), vrb.nm = c("conc","uptake"),
#'    grp.nm = c("Type","Treatment"), n = 2L) # multiple grouping columns
#' @export
shifts_by <- function(data, vrb.nm, grp.nm, n, undefined = NA, suffix) {

   grp <- data[grp.nm]
   data_by <- split(x = data[vrb.nm], f = grp) # split.data.frame
   Shifted_by <- lapply(X = data_by, FUN = shifts, vrb.nm = vrb.nm, n = n,
                        undefined = undefined, suffix = "") # because don't need to rename with suffix for every group
   output <- unsplit(value = Shifted_by, f = grp) # unsplit() returns the original data.frame order, while rbind.data.frame() does not
   if (missing(suffix)) {
      if (n < 0L) suffix <- paste0("_gw", -n)
      if (n > 0L) suffix <- paste0("_dw", +n)
      if (n == 0L) suffix <- ""
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# decompose #

#' Decompose a Numeric Vector by Group
#'
#' \code{decompose} decomposes a numeric vector into within-group and
#' between-group components via within-group centering and group-mean
#' aggregation. There is an option to create a grand-mean centered version of
#' the between-person component as well as lead/lag versions of the original
#' vector and the within-group component.
#'
#' @param x numeric vector.
#'
#' @param grp list of atomic vector(s) and/or factor(s) (e.g., data.frame),
#'   which each have same length as \code{x}. It can also be an atomic vector or
#'   factor, which will then be made the first element of a list internally.
#'
#' @param grand logical vector of length 1 specifying whether a grand-mean
#'   centered version of the the between-group component should be computed.
#'
#' @param n.shift integer vector specifying the direction and magnitude of the
#'   shifts. For example a one-lead is +1 and a two-lag is -2. See \code{shift}
#'   details.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See \code{shift} details.
#'
#' @return data.frame with nrow = \code{length(x)} and \code{row.names =
#'   names(x)}. The first two columns correspond to the within-group component
#'   (i.e., "wth") and the between-group component (i.e., "btw"). If grand =
#'   TRUE, then the third column corresponds to the grand-mean centered
#'   between-group component (i.e., "btw_c"). If shift != NULL, then the last
#'   columns are the shifts indicated by n.shift, where the shifts of \code{x}
#'   are first (i.e., "tot") and then the shifts of the within-group component
#'   are second (i.e., "wth"). The naming of the shifted columns is based on the
#'   default behavior of \code{Shift_by}. See the details of \code{Shift_by}. If
#'   you don't like the default naming, then call \code{Decompose} instead and
#'   use the different suffix arguments.
#'
#' @seealso
#'   \code{\link{decomposes}}
#'   \code{\link{center_by}}
#'   \code{\link{agg}}
#'   \code{\link{shift_by}}
#'
#' @examples
#'
#' # single grouping variable
#' chick_data <- as.data.frame(ChickWeight) # because the "groupedData" class
#'    # calls `[.groupedData`, which is different than `[.data.frame`
#' decompose(x = ChickWeight[["weight"]], grp = ChickWeight[["Chick"]])
#' decompose(x = ChickWeight[["weight"]], grp = ChickWeight[["Chick"]],
#'    grand = FALSE) # no grand-mean centering
#' decompose(x = setNames(obj = ChickWeight[["weight"]],
#'    nm = paste0(row.names(ChickWeight),"_row")), grp = ChickWeight[["Chick"]]) # with names
#'
#' # multiple grouping variables
#' tmp_nm <- c("Type","Treatment") # b/c Roxygen2 doesn't like c() in a []
#' decompose(x = as.data.frame(CO2)[["uptake"]], grp = as.data.frame(CO2)[tmp_nm])
#' decompose(x = as.data.frame(CO2)[["uptake"]], grp = as.data.frame(CO2)[tmp_nm],
#'    n.shift = 1)
#' decompose(x = as.data.frame(CO2)[["uptake"]], grp = as.data.frame(CO2)[tmp_nm],
#'    n.shift = c(+2, +1, -1, -2))
#' @export
decompose <- function(x, grp, grand = TRUE, n.shift = NULL, undefined = NA) {

   wth <- center_by(x = x, grp = grp, center = TRUE, scale = FALSE)
   btw <- agg(x = x, grp = grp, rep = TRUE, fun = mean, na.rm = TRUE)
   all <- list("wth" = wth, "btw" = btw)
   if (grand) {
      btw_c <- center(x = btw, center = TRUE, scale = FALSE)
      all <- c(all, list("btw_c" = btw_c))
   }
   if (!(is.null(n.shift))) {
      shift_tot <- lapply(X = n.shift, FUN = function(n)
         shift_by(x = x, grp = grp, n = n, undefined = undefined))
      names(shift_tot) <- paste0("tot_", ifelse(n.shift > 0L, yes = "dw", no = "gw"), abs(n.shift))
      shift_wth <- lapply(X = n.shift, FUN = function(n)
         shift_by(x = wth, grp = grp, n = n, undefined = undefined))
      names(shift_wth) <- paste0("wth_", ifelse(n.shift > 0L, yes = "dw", no = "gw"), abs(n.shift))
      all <- c(all, shift_tot, shift_wth)
   }
   output <- as.data.frame(all) # as.data.frame.list
   if (!(is.null(names(x)))) row.names(output) <- names(x)
   return(output)
}

# decomposes #

#' Decompose Numeric Data by Group
#'
#' \code{decomposes} decomposes numeric data by group into within-group and
#' between- group components via within-group centering and group-mean
#' aggregation. There is an option to create a grand-mean centered version of
#' the between-group components.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param grand logical vector of length 1 specifying whether grand-mean
#'   centered versions of the the between-group components should be computed.
#'
#' @param n.shift integer vector specifying the direction and magnitude of the
#'   shifts. For example a one-lead is +1 and a two-lag is -2. See
#'   \code{Shift_by} details.
#'
#' @param undefined atomic vector of length 1 (probably makes sense to be the
#'   same typeof as the vectors in \code{data[vrb.nm]}). Specifies what to
#'   insert for undefined values after the shifting takes place. See details of
#'   \code{Shift_by}.
#'
#' @param suffix.wth character vector with a single element specifying the
#'   string to append to the end of the within-group component colnames of the
#'   return object.
#'
#' @param suffix.btw character vector with a single element specifying the
#'   string to append to the end of the between-group component colnames of the
#'   return object.
#'
#' @param suffix.grand character vector with a single element specifying the
#'   string to append to the end of the grand-mean centered version of the
#'   between-group component colnames of the return object. Note, this is a
#'   string that is appended after \code{suffix.btw} has already been appended.
#'
#' @param suffix.lead character vector with a single element specifying the
#'   string to append to the end of the positive shift colnames of the return
#'   object. Note, \code{decomposes} will add \code{abs(n.shift)} to the end of
#'   \code{suffix.lead}.
#'
#' @param suffix.lag character vector with a single element specifying the
#'   string to append to the end of the negative shift colnames of the return
#'   object. Note, \code{decomposes} will add \code{abs(n.shift)} to the end of
#'   \code{suffix.lag}.
#'
#' @return data.frame with nrow = \code{nrow(data} and rownames =
#'   \code{row.names(data)}. The first set of columns correspond to the
#'   within-group components, followed by the between-group components. If grand
#'   = TRUE, then the next set of columns correspond to the grand-mean centered
#'   between-group components. If shift != NULL, then the last columns are the
#'   shifts by group indicated by n.shift, where the shifts of
#'   \code{data[vrb.nm]} are first and then the shifts of the within-group
#'   components are second.
#'
#' @seealso
#'    \code{\link{decompose}}
#'    \code{\link{centers_by}}
#'    \code{\link{aggs}}
#'    \code{\link{shifts_by}}
#'
#' @examples
#' ChickWeight2 <- as.data.frame(ChickWeight)
#' row.names(ChickWeight2) <- as.numeric(row.names(ChickWeight)) / 1000
#' decomposes(data = ChickWeight2, vrb.nm = c("weight","Time"), grp.nm = "Chick")
#' decomposes(data = ChickWeight2, vrb.nm = c("weight","Time"), grp.nm = "Chick",
#'    suffix.wth = ".wth", suffix.btw = ".btw", suffix.grand = ".grand")
#' decomposes(data = as.data.frame(CO2), vrb.nm = c("conc","uptake"),
#'    grp.nm = c("Type","Treatment")) # multiple grouping columns
#' decomposes(data = as.data.frame(CO2), vrb.nm = c("conc","uptake"),
#'    grp.nm = c("Type","Treatment"), n.shift = 1) # with lead
#' decomposes(data = as.data.frame(CO2), vrb.nm = c("conc","uptake"), grp.nm = c("Type","Treatment"),
#'    n.shift = c(+2, +1, -1, -2)) # with multiple lead/lags
#' @export
decomposes <- function(data, vrb.nm, grp.nm, grand = TRUE, n.shift = NULL, undefined = NA,
   suffix.wth = "_w", suffix.btw = "_b", suffix.grand = "c",
   suffix.lead = "_dw", suffix.lag = "_gw") {

   wth <- centers_by(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm,
      center = TRUE, scale = FALSE, suffix = suffix.wth)
   btw <- aggs(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm, rep = TRUE,
      suffix = suffix.btw, fun = mean, na.rm = TRUE)
   all <- c(wth, btw)
   if (grand) {
      btw_c <- centers(data = btw, vrb.nm = names(btw),
         center = TRUE, scale = FALSE, suffix = suffix.grand)
      all <- c(all, btw_c)
   }
   if (!(is.null(n.shift))) {
      shift_tot <- lapply(X = n.shift, FUN = function(n) {
         shifts_by(data = data, vrb.nm = vrb.nm, grp.nm = grp.nm, n = n, undefined = undefined,
            suffix = paste0(ifelse(n > 0L, yes = suffix.lead, no = suffix.lag), abs(n)))
      })
      data_wth <- as.data.frame(c(wth, data[grp.nm])) # as.data.frame.list
      vrb_wth <- str2str::pick(x = names(data_wth), val = grp.nm, not = TRUE)
      shift_wth <- lapply(X = n.shift, FUN = function(n) {
         shifts_by(data = data_wth, vrb.nm = vrb_wth, grp.nm = grp.nm, n = n, undefined = undefined,
            suffix = paste0(ifelse(n > 0L, yes = suffix.lead, no = suffix.lag), abs(n)))
      })
      all <- c(all, shift_tot, shift_wth)
   }
   output <- as.data.frame(all) # as.data.frame.list
   row.names(output) <- row.names(data)
   return(output)
}

# change #

#' Change Score from a Numeric Vector
#'
#' \code{change} creates a change score (aka difference score) from a numeric
#' vector. It is assumed that the vector is already sorted by time such that the
#' first element is earliest in time and the last element is the latest in time.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shift} tries to circumvent this
#' issue by a call to \code{round} within \code{shift} if \code{n} is not an
#' integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shift} truncates rather than rounds.
#' See details of \code{\link{shift}}.
#'
#' @param x numeric vector.
#'
#' @param n integer vector with length 1. Specifies how the change score is
#'   calculated. If \code{n} is positive, then the change score is calculated
#'   from lead - original; if \code{n} is negative, then the change score is
#'   calculated from original - lag. The magnitude of \code{n} determines how
#'   many elements are shifted for the lead/lag within the calculation. If
#'   \code{n} is zero, then \code{change} simply returns a vector or zeros. See
#'   details of \code{\link{shift}}.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See details of \code{\link{shift}}.
#'
#' @return an atomic vector of the same length as \code{x} that is the change
#'   score. If \code{x} and \code{undefined} are different typeofs, then the
#'   return will be coerced to the most complex typeof (i.e., complex to simple:
#'   character, double, integer, logical).
#'
#' @seealso
#'    \code{\link{changes}}
#'    \code{\link{change_by}}
#'    \code{\link{changes_by}}
#'    \code{\link{shift}}
#'
#' @examples
#' change(x = attitude[[1]], n = -1L) # use L to prevent problems with floating point numbers
#' change(x = attitude[[1]], n = -2L) # can specify any integer up to the length of `x`
#' change(x = attitude[[1]], n = +1L) # can specify negative or positive integers
#' change(x = attitude[[1]], n = +2L, undefined = -999) # user-specified indefined value
#' change(x = attitude[[1]], n = -2L, undefined = -999) # user-specified indefined value
#' change(x = attitude[[1]], n = 0L) # returns a vector of zeros
#' \dontrun{
#' change(x = setNames(object = letters, nm = LETTERS), n = 3L) # character vector returns an error
#' }
#' @export
change <- function(x, n, undefined = NA) {

   x_shift <- shift(x = x, n = n, undefined = NA)
   n_sign <- sign(n)
   if (n_sign == 0) {
      rtn <- x - x
   }
   if (n_sign == +1) {
      rtn <- x_shift - x
      len_rtn <- length(rtn)
      rtn[(len_rtn - abs(n) + 1):len_rtn] <- undefined
   }
   if (n_sign == -1) {
      rtn <- x - x_shift
      len_rtn <- length(rtn)
      rtn[1:(abs(n))] <- undefined
   }
   return(rtn)
}

# changes #

#' Change Scores from Numeric Data
#'
#' \code{changes} creates change scores (aka difference scores) from numeric
#' data. It is assumed that the data is already sorted by time such that the
#' first row is earliest in time and the last row is the latest in time.
#' \code{changes} is a multivariate version of \code{\link{change}} that operates
#' on multiple variabes rather than just one.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shifts} tries to circumvent this
#' issue by a call to \code{round} within \code{shifts} if \code{n} is not an
#' integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shifts} truncates rather than rounds.
#' See details of \code{\link{shifts}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param n integer vector with length 1. Specifies how the change score is
#'   calculated. If \code{n} is positive, then the change score is calculated
#'   from lead - original; if \code{n} is negative, then the change score is
#'   calculated from original - lag. The magnitude of \code{n} determines how
#'   many rows are shifted for the lead/lag within the calculation. See details
#'   of \code{\link{shifts}}.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See details of \code{\link{shifts}}.
#'
#' @param suffix character vector of length 1 specifying the string to append to
#'   the end of the colnames of the return object. The default depends on the
#'   \code{n} argument: 1) if \code{n} < 0, then \code{suffix} =
#'   \code{paste0("_hg", -n)}, 2) if \code{n} > 0, then \code{suffix} =
#'   \code{paste0("_hd", +n)}, 3) if \code{n} = 0, then \code{suffix} = "".
#'
#' @return data.frame of change scores with colnames specified by
#'   \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{change}}
#'    \code{\link{changes_by}}
#'    \code{\link{change_by}}
#'    \code{\link{shifts}}
#'
#' @examples
#' changes(attitude, vrb.nm = names(attitude),
#'    n = -1L) # use L to prevent problems with floating point numbers
#' changes(attitude, vrb.nm = names(attitude),
#'    n = -2L) # can specify any integer up to the length of `x`
#' changes(attitude, vrb.nm = names(attitude),
#'    n = +1L) # can specify negative or positive integers
#' changes(attitude, vrb.nm = names(attitude),
#'    n = +2L, undefined = -999) # user-specified indefined value
#' changes(attitude, vrb.nm = names(attitude),
#'    n = -2L, undefined = -999) # user-specified indefined value
#' \dontrun{
#' changes(str2str::d2d(InsectSprays), names(InsectSprays),
#'   n = 3L) # character vector returns an error
#' }
#' @export
changes <- function(data, vrb.nm, n, undefined = NA, suffix) {

   changed <- lapply(X = data[vrb.nm], FUN = change, n = n, undefined = undefined)
   output <- data.frame(changed, stringsAsFactors = FALSE)
   if (missing(suffix)) {
      if (n < 0L) suffix <- paste0("_hg", -n)
      if (n > 0L) suffix <- paste0("_hd", +n)
      if (n == 0L) suffix <- ""
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# change_by #

#' Change Scores from a Numeric Vector by Group
#'
#' \code{change_by} creates a change score (aka difference score) from a numeric
#' vector separately for each group. It is assumed that the vector is already
#' sorted within each group by time such that the first element for that group
#' is earliest in time and the last element for that group is the latest in
#' time.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shift_by} tries to circumvent
#' this issue by a call to \code{round} within \code{shift_by} if \code{n} is
#' not an integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shift_by} truncates rather than
#' rounds. See details of \code{\link{shift_by}}.
#'
#' @param x numeric vector.
#'
#' @param grp list of atomic vector(s) and/or factor(s) (e.g., data.frame),
#'   which each have same length as \code{x}. It can also be an atomic vector or
#'   factor, which will then be made the first element of a list internally.
#'
#' @param n integer vector with length 1. Specifies how the change score is
#'   calculated. If \code{n} is positive, then the change score is calculated
#'   from lead - original; if \code{n} is negative, then the change score is
#'   calculated from original - lag. The magnitude of \code{n} determines how
#'   many rows are shifted for the lead/lag within the calculation. See details
#'   of \code{\link{shift_by}}.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See details of \code{\link{shift_by}}.
#'
#' @return an atomic vector of the same length as \code{x} that is the change
#'   score by group. If \code{x} and \code{undefined} are different typeofs,
#'   then the return will be coerced to the more complex typoof (i.e., complex
#'   to simple: character, double, integer, logical).
#'
#' @seealso
#'    \code{\link{changes_by}}
#'    \code{\link{change}}
#'    \code{\link{changes}}
#'    \code{\link{shift_by}}
#'
#' @examples
#' change_by(x = ChickWeight[["Time"]], grp = ChickWeight[["Chick"]], n = -1L)
#' tmp_nm <- c("vs","am") # multiple grouping vectors
#' change_by(x = mtcars[["disp"]], grp = mtcars[tmp_nm], n = +1L)
#' tmp_nm <- c("Type","Treatment") # multiple grouping vectors
#' change_by(x = as.data.frame(CO2)[["uptake"]], grp = as.data.frame(CO2)[tmp_nm], n = 2L)
#' @export
change_by <- function(x, grp, n, undefined = NA) {

   if (!(is.list(grp))) grp <- list(grp)
   grp_len <- lapply(X = grp, FUN = length)
   if (!(all(length(x) == unlist(grp_len))))
      stop("`x` and each element of `grp` must be the same length")
   x_by <- split(x = x, f = grp) # split.default: no reason to make the factor `f` myself, because unsplit() will remake the vector in the original order either way
   changed_by <- lapply(X = x_by, FUN = change, n = n, undefined = undefined)
   output <- unsplit(value = changed_by, f = grp)
   names(output) <- names(x)
   return(output)
}

# changes_by #

#' Change Scores from Numeric Data by Group
#'
#' \code{changes_by} creates change scores (aka difference scores) from numeric
#' data separately for each group. It is assumed that the data is already sorted
#' within each group by time such that the first row for that group is earliest
#' in time and the last row for that group is the latest in time.
#'
#' It is recommended to use \code{L} when specifying \code{n} to prevent
#' problems with floating point numbers. \code{shifts_by} tries to circumvent
#' this issue by a call to \code{round} within \code{shifts_by} if \code{n} is
#' not an integer; however that is not a complete fail safe. The problem is that
#' \code{as.integer(n)} implicit in \code{shifts_by} truncates rather than
#' rounds. See details of \code{\link{shifts_by}}.
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the
#'   variables.
#'
#' @param grp.nm character vector of colnames from \code{data} specifying the
#'   groups.
#'
#' @param n integer vector with length 1. Specifies how the change score is
#'   calculated. If \code{n} is positive, then the change score is calculated
#'   from lead - original; if \code{n} is negative, then the change score is
#'   calculated from original - lag. The magnitude of \code{n} determines how
#'   many rows are shifted for the lead/lag within the calculation. See details
#'   of \code{\link{shifts_by}}.
#'
#' @param undefined atomic vector with length 1 (probably makes sense to be the
#'   same typeof as \code{x}). Specifies what to insert for undefined values
#'   after the shifting takes place. See details of \code{\link{shifts_by}}.
#'
#' @param suffix character vector of length 1 specifying the string to append to
#'   the end of the colnames of the return object. The default depends on the
#'   \code{n} argument: 1) if \code{n} < 0, then \code{suffix} =
#'   \code{paste0("_hgw", -n)}, 2) if \code{n} > 0, then \code{suffix} =
#'   \code{paste0("_hdw", +n)}, 3) if \code{n} = 0, then \code{suffix} = "".
#'
#' @return data.frame of change scores by group with colnames specified by
#'   \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{change_by}}
#'    \code{\link{changes}}
#'    \code{\link{change}}
#'    \code{\link{shifts_by}}
#'
#' @examples
#' changes_by(data = ChickWeight, vrb.nm = c("weight","Time"), grp.nm = "Chick", n = -1L)
#' changes_by(data = mtcars, vrb.nm = c("disp","mpg"), grp.nm = c("vs","am"), n = 1L)
#' changes_by(data = as.data.frame(CO2), vrb.nm = c("conc","uptake"),
#'    grp.nm = c("Type","Treatment"), n = 2L) # multiple grouping columns
#' @export
changes_by <- function(data, vrb.nm, grp.nm, n, undefined = NA, suffix) {

   grp <- data[grp.nm]
   data_by <- split(x = data[vrb.nm], f = grp) # split.data.frame
   Changed_by <- lapply(X = data_by, FUN = changes, vrb.nm = vrb.nm, n = n,
      undefined = undefined, suffix = "") # because don't need to rename with suffix for every group
   output <- unsplit(value = Changed_by, f = grp) # unsplit() returns the original data.frame order, while rbind.data.frame() does not
   if (missing(suffix)) {
      if (n < 0L) suffix <- paste0("_hgw", -n)
      if (n > 0L) suffix <- paste0("_hdw", +n)
      if (n == 0L) suffix <- ""
   }
   names(output) <- paste0(vrb.nm, suffix)
   row.names(output) <- row.names(data)
   return(output)
}

# winsor #

#' Winsorize a Numeric Vector
#'
#' \code{winsor} winsorizes a numeric vector by recoding extreme values as a user-identified boundary value, which is defined by z-score units. The \code{to.na}
#' argument provides the option of recoding the extreme values as missing.
#'
#' Note, the psych package also has a function called \code{winsor}, which
#' offers the option to winsorize a numeric vector by quantiles rather than
#' z-scores. If you have both the quest package and the psych package attached
#' in your current R session (e.g., using \code{library}), depending on which
#' package you attached first, R might default to using the \code{winsor}
#' function in either the quest package or the psych package. One way to deal
#' with this issue is to explicitly call which package you want to use the
#' \code{winsor} package from. You can do this using the \code{::} function in
#' base R where the package name comes before the \code{::} and the function
#' names comes after it (e.g., \code{quest::winsor}).
#'
#' @param x numeric vector
#'
#' @param z.min numeric vector of length 1 specifying the lower boundary value
#' in z-score units.
#'
#' @param z.max numeric vector of length 1 specifying the upper boundary value
#' in z-score units.
#'
#' @param rtn.int logical vector of length 1 specifying whether the recoded values
#' should be rounded to the nearest integer. This can be useful when working with
#' count data and decimal values are impossible.
#'
#' @param to.na logical vector of length 1 specifying whether the extreme values
#' should be recoded to NA rather than winsorized to the boundary values.
#'
#' @return numeric vector of the same length as \code{x} with extreme values
#' recoded as either the boundary values or NA.
#'
#' @seealso
#'    \code{\link{winsors}}
#'    \code{\link[psych]{winsor}} # psych package
#'
#' @examples
#'
#' # winsorize
#' table(quakes$"stations")
#' new <- winsor(quakes$"stations")
#' table(new)
#'
#' # recode as NA
#' vecNA(quakes$"stations")
#' new <- winsor(quakes$"stations", to.na = TRUE)
#' vecNA(new)
#'
#' # rtn.int = TRUE
#' winsor(x = cars[[1]], z.min = -2, z.max = 2, rtn.int = FALSE)
#' winsor(x = cars[[1]], z.min = -2, z.max = 2, rtn.int = TRUE)
#' @export
winsor <- function(x, z.min = -3, z.max = 3, rtn.int = FALSE, to.na = FALSE) {

   z <- center(x = x, center = TRUE, scale = TRUE)
   z_lo <- z < z.min
   z_hi <- z > z.max
   if (to.na) {
      rtn <- x
      rtn[z_lo | z_hi] <- NA
      return(rtn)
   }
   x.min <- mean(x, na.rm = TRUE) + (sd(x, na.rm = TRUE) * z.min)
   x.max <- mean(x, na.rm = TRUE) + (sd(x, na.rm = TRUE) * z.max)
   if (rtn.int) {
      x.min <- round(x.min)
      x.max <- round(x.max)
   }
   rtn <- x
   rtn[z_lo] <- x.min
   rtn[z_hi] <- x.max
   return(rtn)
}

# winsors #

#' Winsorize Numeric Data
#'
#' \code{winsors} winsorizes numeric data by recoding extreme values as a user
#' identified boundary value, which is defined by z-score units. The \code{to.na}
#' argument provides the option of recoding the extreme values as missing.
#'
#' Note, the psych package also has a function called \code{winsor}, which
#' offers the option to winsorize a numeric vector by quantiles rather than
#' z-scores. If you have both the quest package and the psych package attached
#' in your current R session (e.g., using \code{library}), depending on which
#' package you attached first, R might default to using the \code{winsor}
#' function in either the quest package or the psych package. One way to deal
#' with this issue is to explicitly call which package you want to use the
#' \code{winsor} package from. You can do this using the \code{::} function in
#' base R where the package name comes before the \code{::} and the function
#' names comes after it (e.g., \code{quest::winsor}).
#'
#' @param data data.frame of data.
#'
#' @param vrb.nm character vector of colnames from \code{data} specifying the variables.
#'
#' @param z.min numeric vector of length 1 specifying the lower boundary value
#' in z-score units.
#'
#' @param z.max numeric vector of length 1 specifying the upper boundary value
#' in z-score units.
#'
#' @param rtn.int logical vector of length 1 specifying whether the recoded values
#' should be rounded to the nearest integer. This can be useful when working with
#' count data and decimal values are impossible.
#'
#' @param to.na logical vector of length 1 specifying whether the extreme values
#' should be recoded to NA rather than winsorized to the boundary values.
#'
#' @param suffix character vector of length 1 specifying the string to append
#' to the end of the colnames in the return object.
#'
#' @return data.frame of winsorized data with extreme values recoded as either
#' the boundary values or NA and colnames = \code{paste0(vrb.nm, suffix)}.
#'
#' @seealso
#'    \code{\link{winsor}}
#'    \code{\link[psych]{winsor}} # psych package
#'
#' @examples
#'
#' # winsorize
#' lapply(X = quakes[c("mag","stations")], FUN = table)
#' new <- winsors(quakes, vrb.nm = names(quakes))
#' lapply(X = new, FUN = table)
#'
#' # recode as NA
#' vecNA(quakes)
#' new <- winsors(quakes, vrb.nm = names(quakes), to.na = TRUE)
#' vecNA(new)
#'
#' # rtn.int = TRUE
#' winsors(data = cars, vrb.nm = names(cars), z.min = -2, z.max = 2, rtn.int = FALSE)
#' winsors(data = cars, vrb.nm = names(cars), z.min = -2, z.max = 2, rtn.int = TRUE)
#' @export
winsors <- function(data, vrb.nm, z.min = -3, z.max = 3, rtn.int = FALSE,
   to.na = FALSE, suffix = "_win") {

   tmp <- lapply(X = data[vrb.nm], FUN = winsor,
      z.min = z.min, z.max = z.max, rtn.int = rtn.int, to.na = to.na)
   rtn <- data.frame(tmp, stringsAsFactors = FALSE)
   row.names(rtn) <- row.names(data)
   names(rtn) <- paste0(names(data), suffix)
   return(rtn)
}
