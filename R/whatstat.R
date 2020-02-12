#' @export
whatstat <- function(y,x,...) {
  standardGeneric("whatstat")
}

setMethod("whatstat", c("formula", "data.frame"), function(y, x, ...) {
  stopifnot(length(y) == 3)

  frame <- function(i, df=model.frame(y[-i], x)) switch(ncol(df) + 1, NULL, df[[1]], df)

  whatstat(frame(3), frame(2), ...)

})

setMethod("whatstat", c("numeric", "factor"), function(y, x, ...) {
  x <- droplevels(x)

  if(nlevels(x) == 2) {
    y <- split(y, x)
    t.test(y[[1]], y[[2]], ...)
  } else if (nlevels(x) > 2) {
    summary(aov(y~x, ...))
  } else stop("less than two levels in x")
})

setMethod("whatstat", c("ordered", "factor"), function(y, x, ...) {
  x <- droplevels(x)

  if(nlevels(x) == 2) {
    y <- split(y, x)
    wilcox.test(y[[1]], y[[2]], ...)
  } else if(nlevels(x) > 2) {
    kruskal.test(y,x, ...)
  }
})



setMethod("whatstat", c("factor", "factor"), function(y, x, ...) {
  y <- droplevels(y)
  x <- droplevels(x)

  if(nlevels(y) == 2) {
    prop.test(table(x,y), ...)
  } else if(nlevels(y) == 3) {
    chisq.test(x,y, ...)
  }
})

setMethod("whatstat", c("data.frame", "NULL"), function(y, x, ...) {
  if(ncol(y) == 2) {
    x <- y[[2]]
    y <- y[[1]]
    if(is.factor(x) && is.factor(y)) {
      return(chisq.test(x,y, ...))
    }
    if(is.ordered(x) && is.ordered(y)) {
      return(cor.test(y,x, method='spearman', ...))
    }
    if(is.numeric(x) && is.numeric(y)) {
      return(cor.test(y,x, method='pearson', ...))
    }

  }


  stop("Method not implemented for ", class(x), " x ", class(y))
})



setMethod("whatstat", c("ANY", "ANY"), function(y, x) {
  if(interactive())browser()
  stop("Method not implemented for ", class(x), " x ", class(y))
})
