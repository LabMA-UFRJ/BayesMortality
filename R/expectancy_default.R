#' @export
expectancy = function(x, ...) UseMethod("expectancy")
expectancy.default = function(x) print(x)
