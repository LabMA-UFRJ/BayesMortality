#' @export
summary.HP <- function(fit) return(fit$summary)

#' @export
summary.ClosedHP <- function(fit){
  resumo = data.frame(
    x = min(fit$data$x):max(fit$data$x),
    x1 = apply(fit$qx, 2, mean),
    x2 = apply(fit$qx, 2, sd),
    t(apply(fit$qx, 2, quantile, probs = c(0.025, 0.5, 0.975))))
  colnames(resumo) <- c("age", "mean", "sd", "2.5%", "50.0%", "97.5%")
  return(resumo)
}
