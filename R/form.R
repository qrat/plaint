#' @export
#' @title Form Dataset
#' @description Forms a dataset by applying flexible rules for numeric
#'              formatting, accentuation and filtering.
#'
#' @param data          A dataset with numeric values and other typed data.
#'                      Every column will be coerced to \code{character}.
#' @param format        Default C-formatting (see \code{\link{sprintf}}) of all
#'                      numerical values.
#' @param formatcolumns A list with column-specific C-formatting of numerical
#'                      values. The format is identical to \code{format}. Regular
#'                      expressions (see \code{\link{regex}}) are used to select the
#'                      columns by column names. Example for two formattings:
#'                      \code{list("\%2.3f" = c("mean", "std"), "\%1.4f" =
#'                      "pvalue.*"))}
#' @param marker        A list with marking rules which are going to be applied
#'                      to user-specified columns of \code{data}. Syntax:
#'                      \code{<rule> = <vektor of regular expressions>}. Possible rules include
#'                      column-wise comparing rules \code{min}, \code{max}; and arbitrary
#'                      element-wise comparing rules "\code{...v...}" where \code{v}
#'                      is a placeholder for the numeric value of a cell in a
#'                      logical expression. See Examples.
#' @param symbols       A list with symbols to visually highlight all cells where the
#'                      marking rules of \code{marker} applied. Syntax:
#'                      \code{<symbol> = <vector of regular expressions for marking rules>}.
#'                      Arbitrary symbols are allowed.
#'                      Examples are "\code{***}", "\code{+}" or "\code{[v]}" and
#'                      "\code{\\mathbf{v}}" where \code{v} is a placeholder for the
#'                      (formatted) numeric value of a cell. The marker expressions that can
#'                      be approached by the regular expressions are \code{<rule>.<group>}
#'                      where \code{<group>} is one name specified in \code{groups}.
#'                      \code{<rule>.all} refers to the marks from \code{<rule>} applied
#'                      to the whole column.
#' @param groups        A list with vectors of regular expressions that are going to be
#'                      applied to the row names of \code{data}. The specfied
#'                      column-wise \code{marker} are applied for each group. Syntax:
#'                      \code{<groupname> = <vector of regular expressions>}.
#'                      Do not name groups with "." (dot) or "all"!
#' @param groups_only   A logical value indicating whether to apply the marker
#'                      rules only groupwise.
#' @param filterrows    A vector of regular expressions which are applied to
#'                      the row names of \code{data} for selecting and sorting
#'                      the rows of the formatted table. Default: No filtering,
#'                      all rows are included as they appear.
#' @param filtercolumns A vector of regular expressions which are applied to
#'                      the column names of \code{data} for selecting and
#'                      sorting the columns of the formatted table. Default:
#'                      No filtering, all rows are included as they appear.
#'
#' @details The function allows the user to format all kinds of numeric or
#'          character data on the fly, e.g. resulting output from computations.
#'          It can be employed greatly to automatically prepare data and results
#'          to review it directly in the R session or to export it to a
#'          dynamically designed LaTeX table code via \code{\link{latex}}.
#'
#'          The package and this function will be improved by missing
#'          functionality on a regular basis. The author is eager to receive
#'          your suggestions in order to improve the dynamics and power of
#'          PlainT and \code{form}. Please write to Fabian Raters
#'          (mail@@qrat.de).
#'
#' @return The function returns a formatted character dataset of class
#'         \code{form} which inherits of \code{\link{data.frame}}.
#'
#' @seealso For exporting the formed frame, see \code{\link{latex}}.
#'
#' @examples
#' ## forming mtcars
#' mtcarsf <- form(data = mtcars,
#'                 format = "%i",
#'                 formatcolumns = list("%5.1f" = "disp",
#'                                      "%3i" = "hp",
#'                                      "%4.1f" = "qsec",
#'                                      "%4.2f" = c("drat", "wt")),
#'                 marker = list(min = c("mpg", "cyl", "disp", "hp", "drat",
#'                                       "wt", "qsec"),
#'                               max = c("mpg", "cyl", "disp", "hp", "drat",
#'                                       "wt", "qsec"),
#'                               "v==1" = "am"),
#'                 symbols = list("**v" = "max.all",
#'                                "..v" = "min.all",
#'                                "[v]" = "v==1"))
form <- function(data, ...) {
  UseMethod("form")
}


#' @export
#' @rdname form
#' @method form data.frame
form.data.frame <- function(data, format = "%.3f", formatcolumns = list(),
                            marker = list(), symbols = list(),
                            groups = list(), groups_only = FALSE,
                            filterrows = character(), filtercolumns = character()) {

  # check for empty data.frame
  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    stop("Data frame is empty.")
  }

  # Formatting --------------------------------------------------------------
  # ensure non-empty row and column names
  if (is.null(rownames(data))) {
    rownames(data) <- as.character(1:nrow(data))
  }
  if (is.null(colnames(data))) {
    colnames(data) <- as.character(1:ncol(data))
  }

  # format dataset into character frame
  table <- formatFrame(data, format = format, formatcolumns = formatcolumns)
  n <- nrow(data)
  m <- ncol(data)

  # Accentuation ------------------------------------------------------------
  # optionally mark elements visually
  if (length(marker) > 0) {
    # create array with marks
    mark_array <- markFrame(data, marker, groups, groups_only)

    # create element position matrices [row, col] per marker
    mark_list <- list()
    for (i in 1:n) {
      for (j in 1:m) {
        if (!is.null(mark_array[[i, j]])) {
          for (mark in mark_array[[i, j]]) {
            if (mark %in% names(mark_list)) {
              mark_list[[mark]] <- rbind(mark_list[[mark]], cbind(i, j))
            } else {
              mark_list[[mark]] <- cbind(i, j)
            }
          }
        }
      }
    }

    # prepare symbols for formated printing
    symbols_n <- names(symbols)
    for (i in seq_along(symbols)) {
      if (grepl("v", symbols_n[i], fixed = TRUE)) {
        symbols_n[i] <- gsub("v", "%s", symbols_n[i], fixed = TRUE)
      } else {
        symbols_n[i] <- paste0("%s", symbols_n[i])
      }
    }

    # print symbol to marked elements
    mark_names <- names(mark_list)
    for (i in seq_along(mark_list)) {
      for (j in seq_along(symbols)) {
        if (any(vgrepl(symbols[[j]], mark_names[i]))) {
          table[mark_list[[i]]] <- sprintf(symbols_n[j], table[mark_list[[i]]])
        }
      }
    }
  }

  # Filtering ---------------------------------------------------------------
  # optionally reduce table to specfied rows
  if (length(filterrows) > 0) {
    table <- table[vgrepu(filterrows, rownames(data)), ]
  }

  # optionally reduce table to specfied columns
  if (length(filtercolumns) > 0) {
    table <- table[, vgrepu(filtercolumns, colnames(data))]
  }

  # set class and return form
  class(table) <- c("form", "data.frame")
  return(table)
}


#' @export
#' @rdname form
#' @method form matrix
form.matrix <- function(data, ...) {
  # check for empty matrix
  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    stop("Matrix is empty.")
  }

  # ensure non-empty row and column names
  if (is.null(rownames(data))) {
    rownames(data) <- as.character(1:nrow(data))
  }
  if (is.null(colnames(data))) {
    colnames(data) <- as.character(1:ncol(data))
  }

  # convert to data.frame and forward to method
  return(form(data.frame(data, stringsAsFactors = FALSE), ...))
}


#' @export
#' @rdname form
#' @method form table
form.table <- function(data, format = "%i", ...) {
  if (is.character(data[1, 1])) {
    stop("Character tables are not yet supported.")
  }
  return(form(as.data.frame.matrix(data), format = format, ...))
}


# formats any data.frame into a character data.frame
formatFrame <- function(data, format, formatcolumns) {
  # get column formats
  formatcolumns_n <- names(formatcolumns)
  col_names <- colnames(data)

  # format numeric columns
  table <- data
  for (i in seq_along(data)) {
    num_vec <- data[[i]]
    if (is.numeric(num_vec)) {
      # check for individual column format
      col_str <- NULL
      for (j in seq_along(formatcolumns)) {
        if (any(vgrepl(formatcolumns[[j]], col_names[i]))) {
          col_str <- sprintf(formatcolumns_n[j], num_vec)
          break
        }
      }
      # use global format otherwise
      if (is.null(col_str)) {
        col_str <- sprintf(format, num_vec)
      }
      table[[i]] <- col_str
    } else {
      # transform factor vector, etc. in character vector
      table[[i]] <- as.character(num_vec)
    }
  }
  rownames(table) <- rownames(data)
  colnames(table) <- col_names
  return(table)
}


# creates an 2D-array of lists containing the results from
# marking dataset elements based on marking rules
markFrame <- function(data, marker, groups, groups_only) {
  if (groups_only && length(groups) == 0) {
    stop(paste0("If you want to use the groups_only-Option,",
                " please specify the groups."))
  }

  # set up groups of rows
  if (length(groups) > 0) {
    # check for "." in groups names. not allowed
    if (any(grepl("\\.", names(groups)))) {
      stop("Group names are not allowed to contain '.' (dot).")
    }
    groups_rows <- getRows(data, groups)
  }

  # define closure for marking elements by a specified method
  setMarks <- function(mark_array, method, fun, col_inds, elementwise) {
    if (elementwise) {
      for (i in col_inds) {
        # mark values in column absolutely
        ind <- fun(data[, i])
        mark_array[ind, i] <- lapply(mark_array[ind, i], c, list(method))
      }
    } else {
      for (i in col_inds) {
        # mark values in column relative to others
        if (!groups_only) {
          ind <- fun(data[, i])
          mark_array[ind, i] <- lapply(mark_array[ind, i], c,
                                       list(paste(method, "all", sep = ".")))
        }
      }
      # optionally apply to groups
      if (length(groups) > 0) {
        groups_n <- names(groups)
        for (i in col_inds) {
          for (j in seq_along(groups_rows)) {
            if (length(groups_rows[[j]]) > 0) {
              ind <- fun(data[groups_rows[[j]], i])
              mark_array[groups_rows[[j]][ind], i] <-
                lapply(mark_array[groups_rows[[j]][ind], i], c,
                       list(paste(method, groups_n[j], sep = ".")))
            }
          }
        }
      }
    }
    return(mark_array)
  }

  # create 2D array of empty lists for marks
  mark_array <- array(list(NULL), c(nrow(data), ncol(data)))

  # apply marking methods to specified columns by creating a function per marker
  col_names <- colnames(data)
  mark_meth <- names(marker)
  mark_fun <- function(v) {}
  for (i in seq_along(marker)) {
    if (mark_meth[i] %in% c("min", "max")) {
      body(mark_fun) <- parse(text = paste0("{return(which(v == ",
                                            mark_meth[i], "(v)))}"))[[1]]
      elementwise <- FALSE
    } else {
      body(mark_fun) <- parse(text = paste0("{return(which(", mark_meth[i],
                                            "))}"))[[1]]
      elementwise <- TRUE
    }
    mark_array <- setMarks(mark_array, mark_meth[i], mark_fun,
                           vgrep(marker[[i]], col_names), elementwise)
  }

  return(mark_array)
}


# return the row numbers per group pattern.
getRows <- function(data, groups) {
  groups_rows <- vector(mode = "list", length = length(groups))
  names(groups_rows) <- names(groups)
  row_names <- rownames(data)
  for (i in seq_along(groups)) {
    groups_rows[[i]] <- vgrep(groups[[i]], row_names)
  }
  return(groups_rows)
}


# return logical vector of same length as 'x' where TRUE indicates if
# an element matches at least one pattern element of 'p'.
vgrepl <- function(p, x, ...) {
  v <- logical(length = length(x))
  for (pattern in p) {
    v[!v] <- grepl(pattern, x[!v], ...)
    if (all(v)) {
      break
    }
  }
  return(v)
}


# return vector of indices of 'x' with at least one match from
# one pattern element of 'p'.
vgrep <- function(p, x, ...) {
  return(which(vgrepl(p, x, ...)))
}


# return vector of indices of 'x' with at least one match from
# one pattern element of 'p'. indices are ordered by pattern matching order.
vgrepu <- function(p, x, ...) {
  y <- numeric()
  for (pattern in p) {
    y <- union(y, grep(pattern, x, ...))
  }
  return(y)
}


# try to convert a a value into a numberic.
asNumber <- function(s) {
  return(suppressWarnings(as.numeric(s)))
}


# check the transformation of a value into a numeric.
isNumber <- function(s) {
  return(!is.na(asNumber(s)))
}
