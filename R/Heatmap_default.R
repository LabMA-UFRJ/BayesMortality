#' @export
Heatmap = function(x, ...) UseMethod("Heatmap")
Heatmap.default = function(x) print(x)