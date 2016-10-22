## plaint: Plain Table Markup Language
[![Package-License](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT)

### Introduction
`plaint` (pronounced "plenty") allows you to ease your life with forming data
arrays, such as data frames, matrices or tables, and their export to
user-designed LaTeX tables. It comes with full S3 method support for further
R objects. Get back to the important parts of your work.

This packages provides two S3-type functions: `form` formats, accentuates and 
filters data arrays and `latex` exports 'formed' data frames to LaTeX code
resulting in state of the art tables. Both functions allow the user to easily
transform arbitrary dataset based on a user-specified set of rules and table
markup. Read this documentation, the vignettes and study the examples in order
to see how you can employ these functions with joy.

### Installation
Up to now, `plaint` is not available on CRAN because it needs some more tests
for errors to provide a high level of stability. However, the development
version is almost stable.

* Install the most recent version:

    ```r
    devtools::install_github("ratmaster/plaint")
    ```
    
* When the installation is finished, you can employ the package:

    ```r
    library(plaint)
    ```
    
* If you find any bugs or if you have some ideas of new features, please 
  consider creating an Github issue or contacting me directly by mail.

### Features
* Dynamic, rules-based data array forming.
* Export of R data arrays to user-designed LaTeX tables.
* Simple and intuitive markup language for the table design.
* Clean S3 interface which allows R developers to implement a LaTeX table
  export by providing S3 methods that transform the R object into a data frame.
* Well documented functions.
* (to be continued)

### Documentation
I will publish a vignette soon. At the moment, 
[this poster](http://ratmaster.de/plaint/poster.pdf) might be a good source to get
an overview of the project and the basics of `plaint`.

Please see the function documentations of `form` and `latex`:

```r
?form
?latex
```

### Examples
A convenient way to run all examples in a sequence is:

```r
example(plaint)
```

This will run the following lines of code:

```r
## basic example
latex(mtcars)


## table example
latex(table(state.division, state.region), design = "table.txt")


## advanced example in combination with form
mtcarsf <- form(data = mtcars,
                format = "%i",
                formatcolumns = list("%5.1f" = "disp",
                                     "%3i" = "hp",
                                     "%4.1f" = "qsec",
                                     "%4.2f" = c("drat", "wt")),
                marker = list(min = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec"),
                              max = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec"),
                              "v==1" = "am",
                              "sqrt(v)==2" = c("cyl", "gear", "carb")),
                symbols = list("** v" = "max.all",
                               ".. v" = "min.all",
                               "* v" = "max.german",
                               ". v" = "min.german",
                               "\\textbf{v}" = "=="),
                groups = list(german = c("Merc", "Porsche")))

# translate the formed frame into a LaTeX table
latex(mtcarsf, design = "mtcars.txt")

# use the generated template "mtcars.txt" to design the LaTeX table and
# re-run the last command
latex(mtcarsf, design = "mtcars.txt")

# Three different example plaint designs are already included in this package
# for learning purposes. You are free to view, copy or edit them with your
# prefered text editor. You find their paths within this package by means
# of system.file().

# design 1
latex(mtcarsf,
      file = "mt_table1.tex",
      design = system.file("extdata", "mt_design1.txt", package = "plaint"),
      options = list(document = "document1.tex"))

# design 2
latex(mtcarsf,
      file = "mt_table2.tex",
      design = system.file("extdata", "mt_design2.txt", package = "plaint"))

# design 3
latex(mtcarsf,
      file = "mt_table3.tex",
      design = system.file("extdata", "mt_design3.txt", package = "plaint"))
```

### Author

Fabian H.C. Raters.


### License

MIT, 2015-2016.
