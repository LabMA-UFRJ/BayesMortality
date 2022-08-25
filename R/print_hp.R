#' @export
print.HP <- function(fit){
  catf <- function(x, ...) cat(sprintf(x, ...), end = '\n')

  catf("HP curve estimation")
  catf("Model: %s", fit$info$model)
}

#' @export
print.ClosedHP <- function(fit){
  catf <- function(x, ...) cat(sprintf(x, ...), end = '\n')

  catf("HP closure curve estimation")
  catf("Method: %s", fit$method)
  catf("Min. age: %s", min(fit$data$x, na.rm = T))
  catf("Max. age: %s", max(fit$data$x, na.rm = T))
}
