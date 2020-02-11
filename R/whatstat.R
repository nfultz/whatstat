#' @export
whatstat <- function(y,x,...) {
  standardGeneric("whatstat")
}

setMethod("whatstat", c("formula", "data.frame"), function(y, x, ...) {

  frame <- model.frame(y, x)

  y <- model.response(frame)
  frame <- frame[,-attr(terms(frame), "response")]

  whatstat(y, frame, ...)

})

setMethod("whatstat", c("numeric", "factor"), function(y, x, ...) {
  x <- droplevels(x)

  if(nlevels(x) == 2) {
    y <- split(y, x)
    t.test(y[[1]], y[[2]], ...)
  } else if (nlevels(x) > 2) {
    summary(aov(y~x))
  } else stop("less than two levels in x")
})

setMethod("whatstat", c("ordered", "factor"), function(y, x, ...) {
  x <- droplevels(x)

  if(nlevels(x) == 2) {
    y <- split(y, x)
    wilcox.test(y[[1]], y[[2]], ...)
  } else if(nlevels(x) > 2) {
    kruskal.test(y,x)
  }
})



setMethod("whatstat", c("factor", "factor"), function(y, x, ...) {
  y <- droplevels(y)
  x <- droplevels(x)

  if(nlevels(y) == 2) {
    prop.test(table(x,y))
  } else if(nlevels(y) == 3) {
    chisq.test(x,y)
  }
})


setMethod("whatstat", c("ANY", "ANY"), function(y, x) {
  if(interactive())browser()
  stop("Method not implemented for ", class(x), " x ", class(y))
})
