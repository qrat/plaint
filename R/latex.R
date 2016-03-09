#' @export
#' @title Print Dataset to LaTeX Table
#' @description Prints a dataset into a user-designed LaTeX table based on a
#'              simple markup language for tables: PlainT.
#'
#' @param data     The object to be printed. Will coerce \code{data} via
#'                 \code{\link{form}} into a "formed" frame. Supports arbitrary
#'                 formats such as data.frame, matrix or table.
#' @param file     The character file path to print the latex table to.
#'                 Default: table.tex.
#' @param design   The path to the PlainT table design. By default, the name is
#'                 set to 'design.txt'. If the design file does not exist, a
#'                 basic default design for \code{data} will be created
#'                 automatically and saved to \code{design}. The design rules
#'                 as outlined in the Details section apply. See the vignette
#'                 for examples of the intuitive WYSIWYM PlainT markup language
#'                 in practice. Caution: If you use tabs for formatting, they
#'                 will be replaced by three spaces. Better use spaces directly.
#' @param options  A list with options <name> = <value> may be specified here.
#'                 These options will overwrite all options which might be
#'                 written in the head of the \code{design} file (see Details).
#'                 By default, a wrapper document \code{document.tex} is
#'                 created. It embeds \code{file} within a \code{table}
#'                 environment with \strong{all necessary packages}. This is
#'                 considered very useful for an easy migration of the table
#'                 into other documents as well as for quick testing purposes.
#'                 See Details for a list with all options and their default
#'                 values.
#'
#' @param ...      All additional arguments are passed to \code{\link{form}}.
#'                 Notice that \code{form} can only be applied to unformed
#'                 datasets.
#'
#' @details The function creates a state of the art user-designed LaTeX table
#'          from \code{data} based on the \emph{What You See Is What You Mean
#'          (WYSIWYM)} \strong{Plain Table Markup Language (PlainT)} which is
#'          highly flexible and accessible. PlainT was suggested by this
#'          package's author to allow for easy creation, manipulation and
#'          re-use of table designs for dynamically generated contents. Review
#'          the vignettesa and the example section for feature rich example
#'          designs.
#'
#'          \strong{PlainT syntax and usage:}
#'
#'          A \strong{PlainT design file} is a plain text file allowing you to
#'          visually design your table with characters by means of a preferred
#'          simple text editor of your choice.
#'
#'          The table design is divided into the \strong{head} consisting of
#'          column "titles" and the \strong{body} with column "elements" which
#'          are placeholders for the dynamically inserted column contents from
#'          an arbitrary dataset. The column design is defined by the relative
#'          positions of the head titles which can be written in multiple rows.
#'          Head and body may contain an arbitrary number of horizontal and
#'          vertical spaces and ruler as outlined below.
#'
#'          \strong{Spaces} are set to separate columns. Use three spaces to
#'          separate two columns by an empty column. You can create groups and
#'          subgroups by an arbitrary depth. Multiple empty columns are also
#'          supported. Tabs are converted to three spaces but better use spaces
#'          only in order to still see an unique PlainT design in different
#'          text editor. Empty lines or space lines are directly adopted.
#'
#'          \strong{Ruler} just containing "-" (thin line) or "=" (thick line)
#'          are regarded as horizontal separators. A standard table contains
#'          three of them: top (thick, above the head), mid (thin, betweem head
#'          and body) and bottom (thick, below the body). Vertical lines are
#'          created by means of the character "|". Horizontal and vertical lines
#'          can be connected by "+".
#'
#'          All \strong{titles} and \strong{elements} as mentioned above can be
#'          written and \strong{aligned} as outlined:
#'          \enumerate{
#'            \item "\code{:<text>:}" aligns the title or column to center,
#'                  "\code{:<text>}" aligns to the left and "\code{<text>:}"
#'                  aligns to the right.
#'            \item "\code{:.<text>.:}" is only available for body elements and
#'                  aligns each column value at the decimal sign (a dot).
#'            \item Above, \code{<text>} is an arbtrary LaTeX body text. Use
#'                  \code{$} to even print math formulas in your titles.
#'                  Caution: If \code{<text>} contains spaces, you must
#'                  surround it by \strong{quotes}, i. e. \code{"<text>"}.
#'          }
#'
#'          Currently, the \strong{body} must contain one row with "\code{@@}"
#'          signs \strong{referring data columns} from the dataset to be
#'          inserted. A data column will be inserted in the column of the next
#'          title above "\code{@@}" or a preceding "\code{:}" that covers its
#'          position. If you write "\code{@@<nr>}" and "\code{<nr>}" refers to
#'          a valid column index in \code{data} then that column is inserted at
#'          this position. Also, if you write "\code{@@<name>}" and
#'          "\code{<name>}" refers to a valid column name in \code{data} then
#'          that column taken. If no index or name reference is provided, the
#'          columns will be filled in as they appear in \code{data} without
#'          the subset of those which are referenced directly. Additionally,
#'          you can
#'          \enumerate{
#'            \item \strong{combine} multiple data columns in one table column,
#'            \item the mentioned \strong{alignment} rules apply, and
#'            \item you can \strong{surround each value} per column by static
#'                  text, for instance: \code{:.@@mean(@@sd).:}.
#'          }
#'
#'          \strong{Subsets} are arbitrary text lines beginning or ending with
#'          "::". They are employed to write subtitles and divide the body in
#'          several parts.
#'
#'          \strong{Options} can be written directly in the design file. They
#'          are set line by line on top of \code{design} by means of the
#'          syntax "#+<name> = <value>". All available options with their
#'          default values:
#'          \itemize{
#'            \item centering = TRUE
#'            \item document = document.tex
#'            \item landscape = FALSE
#'            \item rownames = TRUE
#'            \item size = normalsize
#'            \item tabularx = FALSE
#'          }
#'          Caution: The function argument \code{options} overwrites the design
#'          file specified options.
#'
#'          \strong{Description} is to be coming soon.
#'
#'          The package and this function will be improved by missing
#'          functionality on a regular basis. The author is eager to receive
#'          your suggestions in order to improve the dynamics and power of
#'          PlainT and \code{latex}. Please write to Fabian Raters
#'          (mail@@qrat.de).
#'
#' @return The function returns \code{TRUE} if no error occurred.
#'
#' @seealso For forming a dataset, see \code{\link{form}}.
#'
#' @examples
#' ## basic example
#' latex(mtcars)
#'
#' ## table example
#' latex(table(state.division, state.region), design = "table.txt")
#'
#' ## in combination with form
#' mtcarsf <- form(data = mtcars,
#'                 format = "%i",
#'                 formatcolumns = list("%5.1f" = "disp",
#'                                      "%3i" = "hp",
#'                                      "%4.1f" = "qsec",
#'                                      "%4.2f" = c("drat", "wt")),
#'                 marker = list(min = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec"),
#'                               max = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec"),
#'                               "v==1" = "am",
#'                               "sqrt(v)==2" = c("cyl", "gear", "carb")),
#'                 symbols = list("** v" = "max.all",
#'                                ".. v" = "min.all",
#'                                "* v" = "max.german",
#'                                ". v" = "min.german",
#'                                "\\textbf{v}" = "=="),
#'                 groups = list(german = c("Merc", "Porsche")))
#'
#' latex(mtcarsf, design = "mtcars.txt")
#'
#' # use the generated template "mtcars.txt" to design the LaTeX table and
#' # re-run the last command
#' latex(mtcarsf, design = "mtcars.txt")
latex <- function(data, file = "table.tex", design = "design.txt", options = NULL, ...) {
  UseMethod("latex")
}


#' @export
#' @rdname latex
#' @method latex form
latex.form <- function(data, file = "table.tex", design = "design.txt", options = NULL) {
  # check parameters
  if (!is.character(file)) {
    stop("Invalid path to table output file.")
  }
  if (!is.character(design)) {
    stop("Invalid path to table design file.")
  }

  # prepare design file if not existing
  if (!file.exists(design)) {
    # combine function options and get settings
    settings <- getSettings(NULL, options)
    data <- preProcess(data, settings)
    message("Create new table design file: ", design, "\n")
    s <- defaultDesign(file = design, data)
  } else {
    # read design file line by line into character vector
    s <- readLines(design)

    # replace tabs by three spaces
    ind <- which(grepl("\t", s))
    if (length(ind) > 0) {
      warning("At least one tabulator was replaced by three spaces.")
      for (i in ind) {
        s[i] <- gsub("\t", "   ", s[i])
      }
    }

    # parse the specified options and get settings
    o <- parseOptions(s)
    s <- o$s
    settings <- getSettings(o$options, options)
    data <- preProcess(data, settings)
  }

  # parse design structure
  D <- parseDesign(s)

  # create latex table
  table <- createTable(D, data, settings)

  # write table to file
  write(x = table$s, file = file, append = FALSE)

  # write wrapper document
  writeDocument(file = file, settings = settings,
                packages = table$packages, definitions = table$definitions)

  # exit with success
  return(TRUE)
}


#' @export
#' @rdname latex
#' @method latex data.frame
latex.data.frame <- function(data, file = "table.tex", design = "design.txt", options = NULL, ...) {
  return(latex(form(data = data, ...), file = file, design = design, options = options))
}


#' @export
#' @rdname latex
#' @method latex matrix
latex.matrix <- function(data, file = "table.tex", design = "design.txt", options = NULL, ...) {
  return(latex(form(data = data, ...), file = file, design = design, options = options))
}


#' @export
#' @rdname latex
#' @method latex table
latex.table <- function(data, file = "table.tex", design = "design.txt", options = NULL, ...) {
  return(latex(form(data = data, ...), file = file, design = design, options = options))
}


# create very simple standard design
defaultDesign <- function(file, data) {
  v <- nchar(colnames(data))
  pos <- cumsum(v + c(0, rep(1, length(v) - 1)))

  s <- paste(colnames(data), collapse = " ")

  # elements
  b <- rep(" ", nchar(s))
  b[pos] <- "@"

  s <- c(paste(rep("=", nchar(s)), collapse = ""),
         s,
         paste(rep("-", nchar(s)), collapse = ""),
         paste(b, collapse = ""),
         paste(rep("=", nchar(s)), collapse = ""))

  # write table design to file
  write(x = paste(s, sep = "\n"), file = file, append = FALSE)

  # return file name
  return(s)
}


# parse Desk to abstract design
parseDesign <- function(s) {
  # parse the vertical lines
  v <- parseVLines(s)
  s <- v$s
  v <- v$v

  # read the design structure
  e <- getElements(s)

  # parse title defined structure
  h <- parseHead(s, e, v)
  v <- h$v
  h <- h$h

  # add parsed body to structure
  b <- parseBody(s[e$body], h)

  # add parsed subs to structure
  u <- parseSubs(s[e$sub], h)

  # add parsed ruler to structure
  r <- parseRuler(s[e$ruler], h, e)

  # return structure
  return(c(e, n = length(s), list(h = h, b = b, u = u, r = r, v = v)))
}


# separate option lines
parseOptions <- function(s) {
  oind <- which(isOptionLine(s))
  if (length(oind) == 0) {
    return(list(options = NULL, s = s))
  }
  ind <- 1:oind[length(oind)]
  rind <- setdiff(ind, oind)
  if (length(rind) > 0 && !all(isSpaceLine(s[rind]))) {
    stop("Invalid syntax: The options block needs to be defined on top.")
  }
  return(list(options = parseList(s[oind]), s = s[-ind]))
}


# parse the vertical lines
parseVLines <- function(s) {
  n <- length(s)
  m <- max(c(sapply(s, nchar)))
  s <- makeField(s, m)

  # scan columnwise
  v <- data.frame()
  for (j in 1:m) {
    i <- 1
    while (i <= n) {
      if (s[i, j] == "|") {
        l <- heightVLine(i, s[, j])
        v <- rbind(v, data.frame(type = "lightrule", start = j, end = j,
                                      top = i, bottom = i + l - 1,
                                      stringsAsFactors = FALSE))
        # replace "|"
        s[i:(i + l - 1), j] <- rep(" ", l)
        i <- i + l
      } else {
        i <- i + 1
      }
    }
  }

  return(list(v = v, s = apply(s, 1, paste, collapse = "")))
}



# set and combine options
getSettings <- function(opts, options) {
  # define all possible options
  settings <- list(centering = TRUE,
                   document = "document.tex",
                   landscape = FALSE,
                   rownames = TRUE,
                   size = "normalsize",
                   tabularx = FALSE)

  # assign design options
  optsn <- tolower(names(opts))
  for (i in seq_along(opts)) {
    s <- settings[[optsn[i]]]
    if (is.null(s)) {
      stop("Unknown option in design file: ", optsn[i], " = " ,
           as.character(opts[[i]]))
    }
    if (is.logical(s)) {
      o <- tolower(opts[[i]])
      if (o %in% c("true", "1")) {
        opts[[i]] <- TRUE
      } else  if (o %in% c("false", "0")) {
        opts[[i]] <- FALSE
      } else {
        stop("Logical value expected but not found: ", optsn[i], " = " ,
             as.character(opts[[i]]))
      }
    } else if (is.numeric(s)) {
      if (isNumber(opts[[i]])) {
        opts[[i]] <- as.numeric(opts[[i]])
      } else {
        stop("Numeric value expected but not found: ", optsn[i], " = " ,
             as.character(opts[[i]]))
      }
    } else {
      o <- tolower(opts[[i]])
      if (o %in% c("true", "1")) {
        opts[[i]] <- TRUE
      } else  if (o %in% c("false", "0")) {
        opts[[i]] <- FALSE
      }
    }
    settings[[optsn[i]]] <- opts[[i]]
  }

  # replace options from design file
  optionsn <- tolower(names(options))
  for (i in seq_along(options)) {
    s <- settings[[optionsn[i]]]
    if (is.null(s)) {
      stop("Unknown option in call: ", optionsn[i], " = " , as.character(options[[i]]))
    }
    if (is.logical(s) && !is.logical(options[[i]])) {
      stop("Logical value expected but not found: ", optionsn[i], " = " ,
           as.character(options[[i]]))
    } else if (is.numeric(s) && !is.numeric(options[[i]])) {
      stop("Numeric value expected but not found: ", optionsn[i], " = " ,
           as.character(options[[i]]))
    }
    settings[[optionsn[i]]] <- options[[i]]
  }

  # return settings
  return(settings)
}


preProcess <- function(data, settings) {
  # check whether to include the rownames as first column
  if (settings$rownames) {
    data <- cbind(data.frame(name = row.names(data)), data)
  }
  return(data)
}


getElements <- function(s) {
  # classify each string line
  ind <- seq_along(s)
  sind <- ind[isSpaceLine(s)]
  ind <- setdiff(ind, sind)
  hind <- ind[isHLine(s[ind])]
  ind <- setdiff(ind, hind)
  bind <- ind[isBodyLine(s[ind])]
  ind <- setdiff(ind, bind)
  uind <- ind[isSubLine(s[ind])]
  ind <- setdiff(ind, uind)
  tind <- ind[isTitleLine(s[ind])]
  ind <- setdiff(ind, tind)

  # syntax checks
  if (length(ind) > 0) {
    stop("Invalid line syntax in line ", ind[1], ": ", strtrim(s[ind[1]], 20))
  }
  if (length(tind) == 0 && length(bind) == 0) {
    stop("Invalid syntax: Design defines neither head nor body.")
  }

  # return characteristics
  return(list(head = tind, sub = uind, body = bind,
              ruler = hind, space = sind,
              width = max(c(sapply(s, nchar)))))
}


parseHead <- function(s, e, v) {
  # get design by head
  if (length(e$head) > 0) {
    h <- parseBlock(makeField(s[e$head], e$width), f = TRUE)
  } else {
    # h <- parseBodyOnly(s[e$body])
  }

  # correct for vertical lines and determine their columns
  column <- numeric(nrow(v))
  starts <- v$start
  ends <- v$end
  for (i in seq_along(starts)) {
    j <- max(which(apply(starts[i] >= h$starts
                         & ends[i] <= h$ends, 2, any)))

    # correct columns in structure for vertical lines
    if (!((j - 1) %in% column)) {
      if (all(as.character(h$texts[, j]) %in% c("", "NULL"))) {
        h$texts <- h$texts[, -j]
        h$ends <- h$ends[, -j]
        if (j == ncol(h$starts)) {
          h$starts <- h$starts[, -j]
        } else {
          h$starts <- h$starts[, -(j + 1)]
        }
      }
    }

    column[i] <- j - 1
  }

  return(list(h = h, v = cbind(v, data.frame(column = column))))
}


# parse body elements
parseBody <- function(s, h) {
  # extract head titles end positions
  starts <- h$starts
  n <- nrow(starts)
  m <- ncol(starts)
  st <- numeric(m)
  for (j in 1:m) {
    for (i in n:1) {
      if (starts[i, j] != 0) {
        st[j] <- starts[i, j]
        break
      }
    }
  }

  # add body rows to structure
  return(getParts(":?[^ ]*@[[:alnum:]_]*[^ ]*:?", s, matrix(st, nrow = 1)))
}


getParts <- function(pattern, s, starts, ends = NULL) {
  n <- nrow(starts)
  m <- ncol(starts)
  nm <- n * (m - 1)
  column <- col(starts)

  # pre-define body structure
  n1 <- length(s)
  y <- list(texts = matrix(list(""), nrow = n1, ncol = m),
            starts = matrix(Inf, nrow = n1, ncol = m),
            ends = matrix(Inf, nrow = n1, ncol = m))

  # find pattern in string lines
  r <- gregexpr(pattern, s)
  matches <- regmatches(s, r)
  for (i in seq_along(s)) {
    tt <- as.list(matches[[i]])
    ts <- as.numeric(r[[i]])
    te <- ts + attr(r[[i]], "match.length") - 1
    cs <- numeric(length(tt))
    if (is.null(ends)) {
      for (j in seq_along(tt)) {
        # get start column number
        tm <- ts[j] - starts
        tm[tm < 0] <- Inf
        ind <- which.min(tm)
        cs[j] <- column[ind]
      }
    } else {
      for (j in seq_along(tt)) {
        # get start column number
        ind <- which.min(abs(ts[j] - starts))
        cs[j] <- column[ind]

        # get end column number
        ind <- which.min(abs(te[j] - ends))
        # loop NULL entrys
        while (ind < nm && ends[ind + n] == 0) {
          ind <- ind + n
        }
        ce <- column[ind]
        # expand with NULL
        if (ce > cs[j]) {
          y$texts[i, (cs[j] + 1):ce] <- matrix(list(NULL), nrow = 1, ncol = ce - cs[j])
        }
      }
    }
    # copy value with positions
    y$texts[i, cs] <- tt
    y$starts[i, cs] <- ts
    y$ends[i, cs] <- te
  }

  return(y)
}


# parse all kinds of ruler / lines
parseRuler <- function(s, h, e) {
  # extract head titles end positions
  if (length(s) == 0) {
    return(NULL)
  }

  # parse rulers to structure
  texts <- getParts("[\\+=-]+", s, h$starts, h$ends)$texts

  # pre-definitions
  hs <- min(e$head)
  be <- max(e$body)
  f_toprule <- FALSE

  # validity check and string reduction
  n <- nrow(texts)
  m <- ncol(texts)
  r <- vector(mode = "list", n)
  for (i in 1:n) {
    r[[i]] <- list()
    j <- 1
    while (j <= m) {
      tt <- texts[[i, j]]
      if (!is.null(tt) && tt != "") {
        # determine characterizing details
        if (grepl("^=+$", tt)) {
          linetype <- "heavyrule"
        } else if (grepl("^-+$", tt)) {
          linetype <- "midrule"
        } else if (grepl("^[-\\+]+$", tt)) {
          linetype <- "lightrule"
        } else {
          stop("Invalid ruler: ", tt)
        }
        l <- widthColumns(j, texts[i, ])

        # classify type
        if (l == m) {
          if (linetype == "heavyrule") {
            eri <- e$ruler[i]
            if (!f_toprule && eri < hs) {
              r[[i]] <- list(list(type = "toprule"))
              f_toprule <- TRUE
            } else if (eri > be && i == n) {
              r[[i]] <- list(list(type = "bottomrule"))
            } else {
              r[[i]] <- list(list(type = "heavyrule"))
            }
          } else {
            r[[i]] <- list(list(type = linetype))
          }
        } else {
          r[[i]] <- c(r[[i]], list(list(type = linetype,
                                        start = j, end = j + l - 1)))
        }

        j <- j + l
      } else {
        j <- j + 1
      }
    }
  }

  return(r)
}


createRows <- function(x, h) {
  # add space rows to structure
  n <- length(x)
  m <- ncol(h$text)
  texts <- matrix(list(NULL), nrow = n, ncol = m)
  texts[, 1] <- matrix(x, nrow = n, ncol = 1)
  starts <- matrix(0, nrow = n, ncol = m)
  ends <- matrix(0, nrow = n, ncol = m)
  return(list(texts = texts, starts = starts, ends = ends))
}


parseSubs <- function(s, h) {
  # add sub rows to structure
  if (length(s) > 0) {
    s <- trimws(s)
    return(createRows(as.list(s), h))
  } else {
    return(NULL)
  }
}


# transform vector of strings into field of characters
makeField <- function(s, m) {
  # split string for parsing
  s <- strsplit(s, "")

  # fill lines with spaces
  for (i in seq_along(s)) {
    l <- length(s[[i]])
    if (l < m) {
      s[[i]] <- c(s[[i]], rep(" ", m - l))
    }
  }
  # simplify to 2D character array
  return(t(simplify2array(s)))
}


# checks for correct naming of body elements
isBodyLine <- function(s) {
  return(grepl("^( *:?[^ ]*@[[:alnum:]_]*[^ ]*:?)+ *$", s))
}


# checks for a horizontal line
isHLine <- function(s) {
  return(grepl("^( *[\\+=-])+ *$", s))
}


# checks for an option line
isOptionLine <- function(s) {
  return(grepl("^ *#\\+.+$", s))
}


# checks for a line with spaces only or an empty line
isSpaceLine <- function(s) {
  return(grepl("^ *$", s))
}


# checks for a subtext line
isSubLine <- function(s) {
  return(grepl("^ *(::.*|.*::) *$", s))
}


# checks for correct naming of title elements
isTitleLine <- function(s) {
  return(grepl("^( *[^ ])+ *$", s))
}


parseBlock <- function(h, f) {
  if (f && !all(h == " ")) {
    # determine space columns
    pos <- getColumnPos(h, z = " ", len = 3)

    # divide and conquer
    if (length(pos) > 0) {
      p <- c(-3, pos - 1, ncol(h))
      n <- nrow(h)
      texts <- matrix(list(), nrow = n)
      starts <- matrix(numeric(), nrow = n)
      ends <- matrix(numeric(), nrow = n)
      for (i in 2:length(p)) {
        k <- p[i - 1] + 4

        # avoid NULL column blocks
        if (p[i] > k) {
          # skip to spaces horizontally
          while (all(h[, k] == " ")) {
            k <- k + 1
          }
          # parse group and correct starts and end postions
          y <- parseBlock(h[, k:p[i], drop = FALSE], f = TRUE)
          ind <- y$starts != 0
          y$starts[ind] <- y$starts[ind] + k - 1
          y$ends[ind] <- y$ends[ind] + k - 1
        } else {
          y <- list()
        }

        # stack blocks together
        texts <- cbind(texts, matrix(list(""), nrow = n), y$texts)
        starts <- cbind(starts, matrix(p[i - 1] + 1, nrow = n), y$starts)
        ends <- cbind(ends, matrix(k - 1, nrow = n), y$ends)
      }
      # delete first empty columns
      texts <- texts[, -1, drop = FALSE]
      starts <- starts[, -1, drop = FALSE]
      ends <- ends[, -1, drop = FALSE]

      return(list(texts = texts, starts = starts, ends = ends))
    }
  }

  # extract all column titles and their positions
  line <- paste(h[1, , drop = FALSE], collapse = "")
  r <- gregexpr(':?\\.?("[^"]+"|[^ ]+)\\.?:?', line)
  if (length(r[[1]]) == 1 && r[[1]] == -1) {
    x <- list(texts = matrix(list(NULL), nrow = 1),
              starts = matrix(0, nrow = 1),
              ends = matrix(0, nrow = 1))
  } else {
    texts <- matrix(regmatches(line, r)[[1]], nrow = 1)
    starts <- matrix(as.numeric(r[[1]]), nrow = 1)
    x <- list(texts = gsub('"', "", texts),
              starts = starts,
              ends = matrix(starts + attr(r[[1]], "match.length") - 1,
                            nrow = 1))
  }

  # return at last non-empty level
  if (nrow(h) == 1) {
    return(x)
  }

  # parse block below
  m = ncol(x$texts)
  y <- parseBlock(h[-1, , drop = FALSE], f = m == 1)

  # postioning columns child columns
  y <- positionColumns(x, y)

  # avoid positioning of a single parent column
  if (m > 1) {
    x <- positionColumns(y, x)
    m <- ncol(x$texts)
  }

  # fill up needed columns
  d <- ncol(y$texts) - m
  if (d > 0) {
    x$texts <- cbind(x$texts, matrix(list(NULL), ncol = d))
    x$starts <- cbind(x$starts, matrix(0, ncol = d))
    x$ends <- cbind(x$ends, matrix(0, ncol = d))
  } else if (d < 0) {
    n <- nrow(y$texts)
    y$texts <- cbind(y$texts,
                     matrix(list(NULL), nrow = n,  ncol = -d))
    y$starts <- cbind(y$starts,
                      matrix(0, nrow = n, ncol = -d))
    y$ends <- cbind(y$ends,
                    matrix(0, nrow = n, ncol = -d))
  }

  return(list(texts = rbind(x$texts, y$texts),
              starts = rbind(x$starts, y$starts),
              ends = rbind(x$ends, y$ends)))
}


getColumnPos <- function(s, z = " ", len = 1) {
  # block cannot be splitted
  if (ncol(s) < len) {
    return(numeric())
  }

  v <- numeric()
  count <- 0
  m <- logical(nrow(s))
  for (j in 1:ncol(s)) {
    # do not ingnore spaced strings
    ind <- s[, j] == '"'
    if (any(ind)) {
      m[ind] <- !m[ind]
    }
    if (any(m)) {
      next
    }

    if (all(s[, j] == z)) {
      count <- count + 1
    } else {
      count <- 0
    }
    if (count == len) {
      v <- c(v, j - len + 1)
      count <- 0
    }
  }

  # check for closed quotes
  if (any(m)) {
    stop('At least one quote sign (") is missing.')
  }

  return(v)
}


positionColumns <- function(x, y) {
  n <- nrow(y$texts)
  m <- ncol(y$texts)
  m1 <- ncol(x$texts)

  texts = matrix(list(), nrow = n)
  starts = matrix(numeric(), nrow = n)
  ends = matrix(numeric(), nrow = n)

  j <- 1
  for (i in 1:m) {
    while (j <= m1 && y$starts[1, i] != 0 && x$ends[1, j] != 0
           && y$starts[1, i] > x$ends[1, j]) {
      texts <- cbind(texts,
                     matrix(list(NULL), nrow = n))
      starts <- cbind(starts,
                      matrix(0, nrow = n))
      ends <- cbind(ends,
                    matrix(0, nrow = n))
      j <- j + 1
    }
    texts <- cbind(texts, y$texts[, i])
    starts <- cbind(starts, y$starts[, i])
    ends <- cbind(ends, y$ends[, i])
    j <- j + 1
  }

  return(list(texts = texts, starts = starts, ends = ends))
}


# create latex table
createTable <- function(D, data, settings) {
  # check format of elements
  layout <- createLayout(D, settings)
  s_begin <- layout$s_begin
  s_end <- layout$s_end

  # vector for string parts
  s <- character(D$n)

  # parse title defined structure
  head <- createHead(D)
  s[D$head] <- head$s

  # add parsed body to structure
  s[D$body] <- createBody(D$b$texts, data)

  # add parsed subs to structure
  subs <- createSubs(D)
  s[D$sub] <- subs$s

  # add parsed ruler to structure
  ruler <- createRuler(D$r)
  s[D$ruler] <- ruler$s

  # add parsed spaces to structure
  space <- createSpaces(D)
  s[D$space] <- space$s

  return(list(s = paste(s_begin, "", paste0(s, collapse = "\n"), "", s_end, sep = "\n"),
              packages = c(layout$packages, head$packages, subs$packages, ruler$packages,
                           space$packages),
              definitions = c(head$definitions, subs$definitions, ruler$definitions,
                              space$definitions)))
}


createLayout <- function(D, settings) {
  v <- D$b$texts[1, ]
  m <- length(v)
  s <- character(m)
  for (j in 1:m) {
    s[j] <- getAlignment(v[[j]], mark = ":", element = TRUE)
  }

  # collect needed packages
  packages <- NULL

  # create standard tabular definition
  s_begin <- "\\begin{tabular}{"
  s_end <- "\\end{tabular}"

  # check for tabularx option
  if (settings$tabularx) {
    # get expand column
    s[getXColumn(D$h)] <- "X"

    # create tabularx definition
    s_begin <- "\\begin{tabularx}{\\textwidth}{"
    s_end <- "\\end{tabularx}"
    packages <- c(packages, "\\usepackage{tabularx}")
  }

  # check need for decimal point alignment
  if (any(s == "S")) {
    packages <- c(packages, "\\usepackage{siunitx}")
  }

  # add vertical lines
  for (j in 1:m) {
    if (hasRightVLine(1, j, D$body, D$v)) {
      s[j] <- paste0(s[j], "|")
    }
  }

  return(list(s_begin = paste0(s_begin, paste(s, collapse = ""), "}"),
              s_end = s_end, packages = packages))
}


# check whether an element of x has a vertical line on its right
hasRightVLine <- function(i, j, xind, v) {
  if (nrow(v) == 0) {
    return(FALSE)
  }
  vv <- v[v$column == j, ]
  if (nrow(vv) == 0) {
    return(FALSE)
  }
  return(any(vv$top <= xind[i] & vv$bottom >= xind[i]))
}


# returns the expand column for xtabular
# find widest space column
getXColumn <- function(h) {
  ind <- which(h$texts[1, ] == "")
  if (length(ind) == 0) {
    stop("Cannot determine X-Column for xtabular: No empty columns.")
  }
  if (length(ind) == 1) {
    return(ind)
  }
  # extend virtually to the end
  starts <- c(h$starts[1, ], h$starts[1, ncol(h$starts)])
  p <- which.max(starts[ind + 1] - starts[ind])
  return(ind[p])
}


getAlignment <- function(text, mark, element) {
  # decimal point alignment
  if (element && grepl(":\\..+\\.:", text)) {
    return("S")
  }

  r <- gregexpr(mark, text)[[1]]
  n <- length(r)
  if (n == 1) {
    if (r == 1) {
      return("l")
    } else if (r == -1) {
      return("c")
    } else {
      return("r")
    }
  } else if (n == 2) {
    return("c")
  } else {
    stop("Wrong alignment format: ", text)
  }
}


# determine column width
widthColumns <- function(j, trow) {
  m <- length(trow)
  k <- j + 1
  while (k <= m && is.null(trow[[k]])) {
    k <- k + 1
  }
  return(k - j)
}


# determine column width
heightVLine <- function(i, tcol) {
  m <- length(tcol)
  k <- i + 1
  while (k <= m && tcol[[k]] == "|") {
    k <- k + 1
  }
  return(k - i)
}


# create head
createHead <- function(D) {
  h <- D$h$texts
  n <- nrow(h)
  m <- ncol(h)
  z <- character()
  for (i in 1:n) {
    j <- 1
    l <- 0
    s <- character()
    while (j <= m) {
      if (is.null(h[[i, j]]) || h[[i, j]] == "") {
        l <- l + 1
        if (hasRightVLine(i, j, D$head, D$v)) {
          s <- c(s, sprintf("\\multicolumn{%s}{%s}{%s}", l, "c|", ""))
          l <- 0
        }
        j <- j + 1
      } else {
        if (l > 0) {
          s <- c(s, sprintf("\\multicolumn{%s}{%s}{%s}", l, "c", ""))
        }
        l <- widthColumns(j, h[i, ])
        if (hasRightVLine(i, j + l - 1, D$head, D$v)) {
          s <- c(s, sprintf("\\multicolumn{%s}{%s}{%s}", l,
                            paste0(getAlignment(h[[i, j]], mark = ":", element = FALSE), "|"),
                            strTitle(h[[i, j]])))
        } else {
          s <- c(s, sprintf("\\multicolumn{%s}{%s}{%s}", l,
                            getAlignment(h[[i, j]], mark = ":", element = FALSE),
                            strTitle(h[[i, j]])))
        }
        j <- j + l
        l <- 0
      }
    }
    if (l > 0) {
      s <- c(s, sprintf("\\multicolumn{%s}{%s}{%s}", l, "c", ""))
    }
    z <- c(z, paste(s, collapse = "&"))
  }

  # return used packages and definitions
  packages <- "\\usepackage{multicol}"
  definitions <- NULL

  return(list(s = paste0(z, "\\\\"), packages = packages,
              definitions = definitions))
}


# create body
createBody <- function(b, data) {
  m <- ncol(b)
  s <- ""

  # vector with available indices
  k <- 1:ncol(data)
  for (j in 1:m) {
    if (b[[1, j]] == "") {
      z <- ""
    } else {
      elem <- extractElement(b[[1, j]])
      tmp <- strColumn(data, elem, k)
      z <- tmp$s
      k <- tmp$k
    }
    if (j > 1) {
      s <- paste(s, " & ", z)
    } else {
      s <- paste(z)
    }
  }
  return(paste0(paste(s, collapse = "\\\\\n"), "\\\\"))
}


# create subs
createSubs <- function(D) {
  sub <- D$sub
  if (length(sub) == 0) {
    return(list(s = character(), packages = NULL, definitions = NULL))
  }
  u <- D$u$texts
  m <- ncol(u)
  v <- D$v
  nv <- nrow(v)

  s <- character(length(sub))
  for (i in seq_along(sub)) {
    if (nv > 0) {
      column <- unique(v[v$top <= sub[i] & sub[i] <= v$bottom, "column"])
      n <- length(column)
      if (n > 0) {
        x <- sprintf("\\multicolumn{%s}{%s}{%s}", column[1],
                     paste0(getAlignment(u[[i, 1]], mark = "::", element = FALSE), "|"),
                     strTitle(u[[i, 1]]))
        a <- column[1]
        if (n > 1) {
          for (j in 2:n) {
            l <- column[j] - a
            x <- c(x, sprintf("\\multicolumn{%s}{%s}{%s}", l, "c|", ""))
            a <- a + l
          }
        }
        if (a < m) {
          x <- c(x, sprintf("\\multicolumn{%s}{%s}{%s}", m - a, "c", ""))
        }
        s[i] <- paste0(paste(x, collapse = "&"), "\\\\")
        next
      }
    }
    s[i] <- sprintf("\\multicolumn{%s}{%s}{%s}\\\\", m,
                    getAlignment(u[[i, 1]], mark = "::", element = FALSE),
                    strTitle(u[[i, 1]]))
  }

  # return used packages and definitions
  packages <- "\\usepackage{multicol}"
  definitions <- NULL

  return(list(s = s, packages = packages, definitions = definitions))
}


# create ruler
createRuler <- function(r) {
  f_booktabs <- FALSE
  f_hlineb <- FALSE
  f_cmidruleb <- FALSE

  n <- length(r)
  if (n == 0) {
    return(list(s = character(), packages = NULL, definitions = NULL))
  }
  s <- character(n)
  for (i in 1:n) {
    f_cmidrule_line <- FALSE
    for (x in r[[i]]) {
      if (x$type == "toprule") {
        f_booktabs <- TRUE
        s[i] <- "\\toprule"
      } else if (x$type == "bottomrule") {
        f_booktabs <- TRUE
        s[i] <- "\\bottomrule"
      } else if (x$type == "midrule") {
        f_booktabs <- TRUE
        if (is.null(x$start)) {
          s[i] <- "\\midrule"
        } else {
          s[i] <- paste0(s[i], sprintf("\\cmidrule{%i-%i}", x$start, x$end))
          f_cmidrule_line <- TRUE
        }
      } else if (x$type == "lightrule") {
        if (is.null(x$start)) {
          s[i] <- "\\hline"
        } else {
          s[i] <- paste0(s[i], sprintf("\\cline{%i-%i}", x$start, x$end))
        }
      } else if (x$type == "heavyrule") {
        f_booktabs <- TRUE
        if (is.null(x$start)) {
          s[i] <- "\\midruleb"
          f_hlineb <- TRUE
        } else {
          # correct for line vspace of the follow up cmidrules
          if (f_cmidrule_line) {
            s[i] <- paste0(s[i], "\\corcmidrule")
          } else {
            f_cmidrule_line <- TRUE
          }
          s[i] <- paste0(s[i], sprintf("\\cmidruleb{%i-%i}", x$start, x$end))
          f_cmidruleb <- TRUE
        }
      }
    }
  }

  # add preamble
  packages <- NULL
  definitions <- NULL
  if (f_booktabs) {
    packages <- "\\usepackage{booktabs}"
    if (f_hlineb) {
      definitions <- c(definitions, "\\newcommand{\\midruleb}{\\midrule[\\heavyrulewidth]}")
    }
    if (f_cmidruleb) {
      definitions <- c(definitions,
                    paste0("\\newcommand{\\corcmidrule}[1][\\heavyrulewidth]{\n",
                           "\\\\[\\dimexpr-\\normalbaselineskip-\\belowrulesep-\\aboverulesep",
                           "-#1\\relax]}\n",
                           "\\newcommand{\\cmidruleb}[1]{\\cmidrule[",
                           "\\heavyrulewidth]{#1}}"))
    }
  }

  return(list(s = s, packages = packages, definitions = definitions))
}


# create space lines
createSpaces <- function(D) {
  space <- D$space
  v <- D$v
  if (nrow(v) == 0) {
    return(list(s = rep("\\\\", length(space)),
                packages = NULL, definitions = NULL))
  }

  m <- ncol(D$h$texts)
  s <- character(length(space))
  for (i in seq_along(space)) {
    column <- unique(v[v$top <= space[i] & space[i] <= v$bottom, "column"])
    a <- 0
    x <- character()
    for (j in seq_along(column)) {
      l <- column[j] - a
      x <- c(x, sprintf("\\multicolumn{%s}{%s}{%s}", l, "c|", ""))
      a <- a + l
    }
    if (a < m) {
      x <- c(x, sprintf("\\multicolumn{%s}{%s}{%s}", m - a, "c", ""))
    }
    s[i] <- paste0(paste(x, collapse = "&"), "\\\\")
  }

  # return used packages and definitions
  packages <- "\\usepackage{multicol}"
  definitions <- NULL

  return(list(s = s, packages = packages, definitions = definitions))
}


# extract title text
strTitle <- function(text) {
  text <- gsub("\\.?:\\.?", "", text)
  text <- gsub("(\\$.+)_(.+\\$)", "\\1§§§\\2", text)
  text <- gsub("_", "\\\\_", text)
  text <- gsub("§§§", "_", text)
  return(text)
}


# extract element text
extractElement <- function(text) {
  # grab element parts
  text <- gsub("\\.?:\\.?", "", text)
  r <- gregexpr("@([[:alnum:]_]*)", text)
  r[[1]] <- r[[1]] + 1
  attr(r[[1]], "match.length") <- attr(r[[1]], "match.length") - 1
  var <- regmatches(text, r)[[1]]
  sep <- strsplit(text, "@[[:alnum:]_]*")[[1]]
  return(list(var = var, sep = sep))
}


# returns string column vectors
strColumn <- function(data, elem, k) {
  # convert possible reference to column indices selection
  data_n <- colnames(data)
  m <- 1:ncol(data)
  var <- asNumber(elem$var)
  s <- character()
  for (i in seq_along(var)) {
    if (is.na(var[i])) {
      if (elem$var[i] == "") {
        # get next element from unused columns
        v <- data[[k[1]]]
        k <- k[-1]
      } else {
        j <- which(elem$var[i] == data_n)
        if (length(j) != 1) {
          stop("Can not find column name in dataset: ", elem$var[i])
        }
        v <- data[[j]]
        k <- k[k != j]
      }
    } else {
      if (!(i %in% m)) {
        stop("Element refers to invalid column nr: ", var[i])
      }
      v <- data[[i]]
      k <- k[k != i]
    }
    s <- paste0(s, elem$sep[i], v)
  }

  # in case of closing expression
  if (length(elem$sep) > 1) {
    s <- paste0(s, elem$sep[i + 1])
  }
  return(list(s = s, k = k))
}


# write wrapper document
writeDocument <- function(file, settings, packages, definitions) {
  # defaults
  s_table_begin <- "\\begin{table}[t]"
  s_table_end <- "\\end{table}"
  s_size <- NULL
  s_caption <- NULL
  s_tabletitle <- NULL
  tabletitle <- TRUE

  # document
  if (is.logical(settings$document) && !settings$document) {
    return()
  }

  # centering
  if (settings$centering) {
    s_centering <- "\\centering"
  } else {
    s_centering <- NULL
  }

  # landscape
  if (settings$landscape) {
    packages <- c(packages, "\\usepackage{lscape}")
    s_table_begin <- "\\begin{landscape}"
    s_table_end <- "\\end{landscape}"
    tabletitle <- FALSE
  }

  # size
  if (settings$size != "normalsize") {
    s_size <- paste0("\\", settings$size)
  }

  # tabletitle
  if (tabletitle) {
    s_caption <- c("",
                   "% Package used for environment title",
                   "\\usepackage[font=small,labelfont=bf]{caption}")
    s_tabletitle <- c("",
                      "\\protect\\caption{Generated Table}")
  }

  # set up content
  s <- c("\\documentclass{article}",
         s_caption)

  # packages
  if (length(packages) > 0) {
    s <- c(s, "",
           "% Needed packages",
           sort(unique(packages)))
  }

  # definitions
  if (length(definitions) > 0) {
    s <- c(s, "",
           "% Needed definitions",
           sort(unique(definitions)))
  }

  # body
  s <- c(s,  "",
         "% Example environment",
         "\\begin{document}",
         "",
         s_table_begin,
         s_centering,
         s_size,
         s_tabletitle,
         "",
         sprintf("\\input{%s}", file),
         "",
         s_table_end,
         "",
         "\\end{document}")

  # write document to file
  write(x = paste(s, collapse = "\n"), file = settings$document, append = FALSE)
}


# parse options pairs to list: name = value.
parseList <- function(pair_vec) {
  pair_mat <- simplify2array(
    regmatches(pair_vec,
               regexec("^ *(#\\+)? *([[:graph:]]+) *= *([[:graph:]]+)$",
                       pair_vec)))
  if (!is.matrix(pair_mat)) {
    stop("Unknown list format of options.")
  }
  pair_list <- as.list(pair_mat[4, ])
  names(pair_list) <- pair_mat[3, ]
  return(pair_list)
}


# transforms option pairs to character vectors: 'name = value'.
vecList <- function(pair_list, sep = " = ") {
  return(paste0("#+ ", paste(names(pair_list), pair_list, sep = sep)))
}
