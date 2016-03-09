## plaint: Plain Table Markup Language
[![Package-License](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT)

### Introduction
`plaint` (pronounced "plenty") allows you to ease your life with forming data
arrays, such as data frames, matrices or tables, and their export to a
user-designed LaTeX table. It comes with full S3 method support for further R
objects. Get back to the important parts of your work.

This packages provides two S3-type functions: `form` formats, accentuates and 
filters data arrays and `latex` exports 'formed' data frames to LaTeX code 
resulting in a beautiful table. Both functions allow the user to easily 
transform arbitrary dataset based user-specified set of rules and table markup. 
Read this documentation, the vignettes and study the examples to see how you can
use these functions with joy.

### Installation
Up to now, `plaint` is not available on CRAN because it needs some more tests
for errors to provide a high level of stability. However, the development
version is almost stable.

* Install the most recent version:

    ```r
    devtools::install_github("qrat/plaint")
    ```
    
* If you find any bugs or if you have some ideas of new features, please 
  consider creating an Github issue or contacting me directly by mail.

### Features
* Dynamic, rules-based data array forming.
* Export of R data arrays to user-designed LaTeX tables.
* Simple and intuitive markup language for the table design.
* Clean S3 interface which allows R developers to implement a LaTeX table export
  by providing S3 methods that transform the R object into a data frame.
* Well documented functions.
* (to be continued)

### Examples
```r
## basic example
latex(mtcars)

## table example
latex(table(state.division, state.region), design = "table.txt")

## in combination with form
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

latex(mtcarsf, design = "mtcars.txt")

# use the generated template "mtcars.txt" to design the LaTeX table and
# re-run the last command
latex(mtcarsf, design = "mtcars.txt")
```

### Author

Fabian H.C. Raters


### License

MIT, 2015-2016
