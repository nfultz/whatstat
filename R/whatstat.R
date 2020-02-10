#' @export
whatstat <- function(y,x,...) {
  standardGeneric("whatstat")
}

#' @export
setMethod("whatstat", c("formula", "data.frame"), function(y, x, ...) {

  frame <- model.frame(y, x)

  y <- model.response(frame)
  frame <- frame[,-attr(terms(frame), "response")]

  whatstat(y, frame, ...)

})

setMethod("whatstat", c("numeric", "factor"), function(y, x, ...) {
  if(interactive())browser()
  x <- droplevels(x)

  if(nlevels(x) == 2) {
    y <- split(y, x)
    t.test(y[[1]], y[[2]], ...)
  } else if (nlevels(x) > 2) {
    summary(aov(y~x))
  } else stop("less than two levels in x")
})


setMethod("whatstat", c("ANY", "ANY"), function(y, x) {
  if(interactive())browser()
})
