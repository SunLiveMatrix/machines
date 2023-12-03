#!/usr/bin/r

## ----include=FALSE------------------------------------------------------------
library(checkmate)

## -----------------------------------------------------------------------------
fact <- function(n, method = "stirling") {
  if (length(n) != 1)
    stop("Argument 'n' must have length 1")
  if (!is.numeric(n))
    stop("Argument 'n' must be numeric")
  if (is.na(n))
    stop("Argument 'n' may not be NA")
  if (is.double(n)) {
    if (is.nan(n))
      stop("Argument 'n' may not be NaN")
    if (is.infinite(n))
      stop("Argument 'n' must be finite")
    if (abs(n - round(n, 0)) > sqrt(.Machine$double.eps))
      stop("Argument 'n' must be an integerish value")
    n <- as.integer(n)
  }
  if (n < 0)
    stop("Argument 'n' must be >= 0")
  if (length(method) != 1)
    stop("Argument 'method' must have length 1")
  if (!is.character(method) || !method %in% c("stirling", "factorial"))
    stop("Argument 'method' must be either 'stirling' or 'factorial'")

  if (method == "factorial")
    factorial(n)
  else
    sqrt(2 * pi * n) * (n / exp(1))^n
}

## -----------------------------------------------------------------------------
fact <- function(n, method = "stirling") {
  assertCount(n)
  assertChoice(method, c("stirling", "factorial"))

  if (method == "factorial")
    factorial(n)
  else
    sqrt(2 * pi * n) * (n / exp(1))^n
}

## -----------------------------------------------------------------------------
f <- function(x) {
  assert(
    checkClass(x, "foo"),
    checkClass(x, "bar")
  )
}

## ----eval=FALSE---------------------------------------------------------------
#  # file: tests/test-all.R
#  library(testthat)
#  library(checkmate) # for testthat extensions
#  test_check("mypkg")

## ----eval=FALSE---------------------------------------------------------------
#  test_that("checkmate is a sweet extension for testthat", {
#    x = runif(100)
#    expect_numeric(x, len = 100, any.missing = FALSE, lower = 0, upper = 1)
#    # or, equivalent, using the lazy style:
#    qexpect(x, "N100[0,1]")
#  })

## ----fig.width=6,fig.height=4,dependson="init",eval=requireNamespace("microbenchmark", quietly = TRUE)----
library(checkmate)
library(ggplot2)
library(microbenchmark)

x = TRUE
r = function(x, na.ok = FALSE) { stopifnot(is.logical(x), length(x) == 1, na.ok || !is.na(x)) }
cm = function(x) assertFlag(x)
cmq = function(x) qassert(x, "B1")

## ----fig.width=6,fig.height=4,eval=requireNamespace("microbenchmark", quietly = TRUE)----
x = runif(1000)
r = function(x) stopifnot(is.numeric(x), length(x) == 1000, all(!is.na(x) & x >= 0 & x <= 1))
cm = function(x) assertNumeric(x, len = 1000, any.missing = FALSE, lower = 0, upper = 1)
cmq = function(x) qassert(x, "N1000[0,1]")

## ----fig.width=6,fig.height=4,eval=requireNamespace("microbenchmark", quietly = TRUE)----
x = sample(letters, 10000, replace = TRUE)
r = function(x) stopifnot(is.character(x), !any(is.na(x)), all(nchar(x) > 0))
cm = function(x) assertCharacter(x, any.missing = FALSE, min.chars = 1)
cmq = function(x) qassert(x, "S+[1,]")
mb = microbenchmark(r(x), cm(x), cmq(x))

## ----fig.width=6,fig.height=4,eval=requireNamespace("microbenchmark", quietly = TRUE)----
N = 10000
x = data.frame(a = runif(N), b = sample(letters[1:5], N, replace = TRUE), c = sample(c(FALSE, TRUE), N, replace = TRUE))
r = function(x) is.data.frame(x) && !any(sapply(x, function(x) any(is.na(x))))
cm = function(x) testDataFrame(x, any.missing = FALSE)
cmq = function(x) qtest(x, "D")

# checkmate tries to stop as early as possible
x$a[1] = NA

## ----fig.width=6,fig.height=4,eval=requireNamespace("microbenchmark", quietly = TRUE)----
N = 10000
x.altrep = seq_len(N) # this is an ALTREP in R version >= 3.5.0
x.sexp = c(x.altrep)  # this is a regular SEXP OTOH
r = function(x) stopifnot(is.integer(x), !any(is.na(x)), !is.unsorted(x))
cm = function(x) assertInteger(x, any.missing = FALSE, sorted = TRUE)

## -----------------------------------------------------------------------------
checkSquareMatrix = function(x, mode = NULL) {
  # check functions must return TRUE on success
  # and a custom error message otherwise
  res = checkMatrix(x, mode = mode)
  if (!isTRUE(res))
    return(res)
  if (nrow(x) != ncol(x))
    return("Must be square")
  return(TRUE)
}

# a quick test:
X = matrix(1:9, nrow = 3)
checkSquareMatrix(X)
checkSquareMatrix(X, mode = "character")
checkSquareMatrix(X[1:2, ])

## -----------------------------------------------------------------------------
# For assertions:
assert_square_matrix = assertSquareMatrix = makeAssertionFunction(checkSquareMatrix)
print(assertSquareMatrix)

# For tests:
test_square_matrix = testSquareMatrix = makeTestFunction(checkSquareMatrix)
print(testSquareMatrix)

# For expectations:
expect_square_matrix = makeExpectationFunction(checkSquareMatrix)
print(expect_square_matrix)

## ----eval = FALSE, hilang = "c"-----------------------------------------------
#  SEXP qassert(SEXP x, const char *rule, const char *name);
#  Rboolean qtest(SEXP x, const char *rule);

## ----eval = FALSE, hilang = "c"-----------------------------------------------
#  #include <checkmate.h>
#  #include <checkmate_stub.c>

## -----------------------------------------------------------------------------
sessionInfo()

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.show = "hide")
library(ggplot2)

## -----------------------------------------------------------------------------
mpg_drv_summary <- function() {
  ggplot2::ggplot(ggplot2::mpg) + 
    ggplot2::geom_bar(ggplot2::aes(x = .data$drv)) + 
    ggplot2::coord_flip()
}

## ---- include=FALSE-----------------------------------------------------------
# make sure this function runs!
mpg_drv_summary()

## -----------------------------------------------------------------------------
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip
mpg_drv_summary <- function() {
  ggplot(ggplot2::mpg) + 
    geom_bar(aes(x = drv)) + 
    coord_flip()
}

## ---- include=FALSE-----------------------------------------------------------
# make sure this function runs!
mpg_drv_summary()

## -----------------------------------------------------------------------------
mpg_drv_summary <- function() {
  ggplot(ggplot2::mpg) + 
    geom_bar(aes(x = drv)) + 
    coord_flip()
}

## -----------------------------------------------------------------------------
mpg_drv_summary <- function() {
  ggplot(ggplot2::mpg) + 
    geom_bar(aes(x = .data$drv)) + 
    coord_flip()
}

## -----------------------------------------------------------------------------
col_summary <- function(df, col) {
  ggplot(df) + 
    geom_bar(aes(x = .data[[col]])) + 
    coord_flip()
}

col_summary(mpg, "drv")

## ---- eval = (packageVersion("rlang") >= "0.3.4.9003")------------------------
col_summary <- function(df, col) {
  ggplot(df) + 
    geom_bar(aes(x = {{ col }})) + 
    coord_flip()
}

col_summary(mpg, drv)

## -----------------------------------------------------------------------------
mpg_drv_dist <- structure(
  c(
    "4" = 103 / 234,
    "f" = 106 / 234,
    "r" = 25 / 234
  ),
  class = "discrete_distr"
)

## -----------------------------------------------------------------------------
discrete_distr_data <- function(x) {
  tibble::tibble(
    value = names(x),
    probability = as.numeric(x)
  )
}

discrete_distr_data(mpg_drv_dist)

## -----------------------------------------------------------------------------
#' @importFrom ggplot2 autoplot
autoplot.discrete_distr <- function(object, ...) {
  plot_data <- discrete_distr_data(object)
  ggplot(plot_data, aes(.data$value, .data$probability)) +
    geom_col() +
    coord_flip() +
    labs(x = "Value", y = "Probability")
}

## -----------------------------------------------------------------------------
#' @importFrom graphics plot
plot.discrete_distr <- function(x, ...) {
  print(autoplot(x, ...))
}

## -----------------------------------------------------------------------------
#' @importFrom ggplot2 %+replace%
theme_custom <- function(...) {
  theme_grey(...) %+replace% 
    theme(
      panel.border = element_rect(linewidth = 1, fill = NA),
      panel.background = element_blank(),
      panel.grid = element_line(colour = "grey80")
    )
}

mpg_drv_summary() + theme_custom()

## -----------------------------------------------------------------------------
default_theme <- function() {
  theme_custom()
}

mpg_drv_summary2 <- function() {
  mpg_drv_summary() + default_theme()
}

## -----------------------------------------------------------------------------
theme_custom <- function(...) {
  `%+replace%` <- ggplot2::`%+replace%`
  
  ggplot2::theme_grey(...) %+replace% 
    ggplot2::theme(panel.background = ggplot2::element_blank())
}

## ---- include=FALSE-----------------------------------------------------------
# make sure this function runs!
mpg_drv_summary() + theme_custom()

## ---- eval=FALSE--------------------------------------------------------------
#  .onLoad <- function(...) {
#    if (requireNamespace("ggplot2", quietly = TRUE)) {
#      vctrs::s3_register("ggplot2::autoplot", "discrete_distr")
#    }
#  }


