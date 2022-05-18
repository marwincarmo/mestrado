TrueCohensD <- function (x = NULL, y = NULL, data = NULL, method = "pooled", 
          mu = 0, formula = NULL) 
{
  userInput <- !(c(x = missing(x), y = missing(y), data = missing(data), 
                   method = missing(method), mu = missing(mu), formula = missing(formula)))
  scenario <- "bad"
  if (all(userInput == c(TRUE, FALSE, FALSE, FALSE, FALSE, 
                         FALSE))) {
    if (is(x, "numeric")) {
      scenario <- "one.sample"
    }
    if (is(x, "formula")) {
      scenario <- "two.sample.formula"
      y <- try(eval(model.frame(formula = x), envir = parent.frame()), 
               silent = TRUE)
      if (is(y, "try-error")) {
        stop("variables specified in the formula do not exist or are different lengths")
      }
    }
  }
  if (all(userInput == c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))) {
    if (is(x, "numeric")) {
      if (is(mu, "numeric") & length(mu) == 1) {
        scenario <- "one.sample"
      }
    }
  }
  if (all(userInput == c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))) {
    if (is(x, "numeric") & is(y, "numeric")) {
      scenario <- "two.sample.grouped"
    }
    if (is(x, "formula") & is(y, "data.frame")) {
      scenario <- "two.sample.formula"
    }
  }
  if (all(userInput == c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))) {
    if (is(x, "formula") & is(data, "data.frame")) {
      y <- data
      data <- NULL
      scenario <- "two.sample.formula"
    }
  }
  if (all(userInput == c(FALSE, FALSE, TRUE, FALSE, FALSE, 
                         TRUE))) {
    if (is(formula, "formula") & is(data, "data.frame")) {
      y <- data
      data <- NULL
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
    }
  }
  if (all(userInput == c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))) {
    if (is(x, "formula") & is(method, "character") & length(method) == 
        1) {
      scenario <- "two.sample.formula"
      if (method == "paired") {
        warning("calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
      y <- try(eval(model.frame(formula = x), envir = parent.frame()), 
               silent = TRUE)
      if (is(y, "try-error")) {
        stop("variables specified in the formula do not exist or are different lengths")
      }
    }
  }
  if (all(userInput == c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))) {
    if (is(x, "numeric") & is(y, "numeric") & is(method, 
                                                 "character") & length(method) == 1) {
      scenario <- "two.sample.grouped"
    }
    if (is(x, "formula") & is(y, "data.frame") & is(method, 
                                                    "character") & length(method) == 1) {
      scenario <- "two.sample.formula"
      if (method == "paired") {
        warning("calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }
  if (all(userInput == c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))) {
    if (is(x, "formula") & is(data, "data.frame") & is(method, 
                                                       "character") & length(method) == 1) {
      y <- data
      data <- NULL
      scenario <- "two.sample.formula"
      if (method == "paired") {
        warning("calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }
  if (all(userInput == c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))) {
    if (is(formula, "formula") & is(data, "data.frame") & 
        is(method, "character") & length(method) == 1) {
      y <- data
      data <- NULL
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      if (method == "paired") {
        warning("calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }
  if (all(userInput == c(FALSE, FALSE, FALSE, TRUE, FALSE, 
                         TRUE))) {
    if (is(formula, "formula") & is(method, "character") & 
        length(method) == 1) {
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      if (method == "paired") {
        warning("calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
      y <- try(eval(model.frame(formula = x), envir = parent.frame()), 
               silent = TRUE)
      if (is(y, "try-error")) {
        stop("variables specified in the formula do not exist or are different lengths")
      }
    }
  }
  if (all(userInput == c(FALSE, FALSE, FALSE, FALSE, FALSE, 
                         TRUE))) {
    if (is(formula, "formula")) {
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      y <- try(eval(model.frame(formula = x), envir = parent.frame()), 
               silent = TRUE)
      if (is(y, "try-error")) {
        stop("variables specified in the formula do not exist or are different lengths")
      }
    }
  }
  if (all(userInput == c(TRUE, FALSE, FALSE, FALSE, FALSE, 
                         TRUE))) {
    if (is(formula, "formula") & is(x, "data.frame")) {
      y <- x
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
    }
  }
  if (all(userInput == c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))) {
    if (is(formula, "formula") & is(x, "data.frame") & is(method, 
                                                          "character") & length(method) == 1) {
      y <- x
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      if (method == "paired") {
        warning("calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }
  if (scenario == "bad") {
    stop("arguments specified do not appear to correspond to a meaningful cohen's d calculation")
  }
  if (scenario == "one.sample") {
    x <- x[!is.na(x)]
    d <- mean(x) - mu/sd(x)
    return(d)
  }
  if (scenario == "two.sample.formula") {
    outcome <- eval(x[[2]], y)
    group <- eval(x[[3]], y)
    group <- as.factor(group)
    if (nlevels(group) != 2L) {
      stop("grouping factor must have exactly 2 levels")
    }
    x <- split(outcome, group)
    y <- x[[2]]
    x <- x[[1]]
    scenario <- "two.sample.grouped"
  }
  if (scenario == "two.sample.grouped") {
    if (!(method %in% c("x.sd", "y.sd", "pooled", "corrected", 
                        "raw", "paired", "unequal"))) {
      stop("\"method\" must be \"x.sd\",\"y.sd\",\"pooled\",\"corrected\",\"raw\",\"paired\" or \"unequal\"")
    }
    if (method == "paired") {
      if (length(x) != length(y)) {
        stop("paired samples cohen's d requires samples of the same size")
      }
      ind <- !is.na(x) & !is.na(y)
      x <- x[ind]
      y <- y[ind]
    }
    else {
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]
    }
    pooledSD <- function(x, y, debias = TRUE) {
      sq.devs <- (c(x - mean(x), y - mean(y)))^2
      n <- length(sq.devs)
      if (debias) {
        psd <- sqrt(sum(sq.devs)/(n - 2))
      }
      else {
        psd <- sqrt(sum(sq.devs)/n)
      }
      return(psd)
    }
    mean.diff <- mean(x) - mean(y)
    sd.est <- switch(EXPR = method, x.sd = sd(x), y.sd = sd(y), 
                     pooled = pooledSD(x, y), corrected = pooledSD(x, 
                                                                   y), raw = pooledSD(x, y, FALSE), paired = sd(x - 
                                                                                                                  y), unequal = sqrt((var(x) + var(y))/2))
    d <- mean.diff/sd.est
    if (method == "corrected") {
      n <- length(x) + length(y)
      d <- d * (n - 3)/(n - 2.25)
    }
    return(d)
  }
  stop("how did I get here?")
}
