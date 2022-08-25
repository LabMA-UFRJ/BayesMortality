#' @name expectancy
#' @rdname expectancy
#'
#' @title HP Model: Life expectancy
#'
#' @description This function calculates the life expectancy for each age in fitted HP model.
#'
#' @usage
#' expectancy(fit, Ex = NULL, age = NULL, graph = TRUE,
#'  max_age = 110, prob = 0.95)
#'
#' @param fit Object of the class 'HP' or 'ClosedHP' fitted by the hp() or hp_close() functions.
#' @param Ex Numeric vector with the exposure by age. This argument is only necessary when using poisson and binomial models. The default value is NULL.
#' @param age Numeric vector specifying the ages to calculate the life expectancy. The default is a sequence (0, 10, 20, ...) until the last decade used in the fitted model.
#' @param graph Logical value (TRUE ou FALSE). If TRUE, it also returns a plot. The default value is TRUE.
#' @param max_age Positive number indicating  the last age to be used in the calculation of the life expectancy. The default value is 110.
#' @param prob A number specifying the probability of credible interval The default value is 0.95.
#'
#' @details This function calculates the life expectancy using the truncated life expectancy formula:
#'
#'  \eqn{e_x = \sum tp_x}
#'
#' where:
#'
#'  \eqn{tp_x = p_0 x p_1 x ... x p_x}
#'
#' @return A data.frame and (if graph = TRUE) a plot.
#'
#' @examples
#' ## Importing mortality data from USA
#' data(USA)
#'
#' # Example 1: --------------------------------
#'
#' USA1990 = USA[USA$Year == 1990,]
#'
#' Ex = USA1990$Ex.Total[1:91]
#' Dx = USA1990$Dx.Total[1:91]
#' x = 0:90
#'
#' fit <- hp(x, Ex, Dx, model = "binomial")
#' exp <- expectancy(fit)
#'
#' exp$plot
#' exp$tabua
#' exp
#'
#'
#' # Example 2: -------------------------------
#'
#' # Using some arguments:
#'
#' Ex = USA1990$Ex.Total[1:106]
#'
#' exp <- expectancy(fit, Ex = Ex, age = c(0,20,30,60,105),
#'                   max_age = 105, prob = 0.99, graph = FALSE)
#'
#' exp
#'
#' @include qx_ci.R
#' @include fitted_hp.R
#'
#' @import ggplot2
#'
#' @export
expectancy.HP <- function(fit,
                   Ex = NULL,
                   age = NULL,
                   graph = TRUE,
                   max_age = 110,
                   prob = 0.95){
  ## Checking age
  if(is.null(age)){ age = seq(0, max(fit$data$x),by = 10) }

  ##calculo do qx e px estimados.
  #fechando a tabua::
    qx_est <- fitted(fit, age = 0:max_age)
    px_est <- 1 - qx_est$qx_fitted

    ##IC
    if(fit$info$model %in% c("binomial","poisson")){
      if(is.null(Ex)){
        Ex <- c(fit$data$Ex, rep(fit$data$Ex[length(fit$data$Ex)], (max_age+1)-length(fit$data$Ex)))
        est_IC <- qx_ci(fit, age = 0:max_age, Ex = Ex, prob = prob)
        ps_est <- 1 - est_IC$qi
        pi_est <- 1 - est_IC$qs
      }else{
        est_IC <- qx_ci(fit, age = 0:max_age, Ex = Ex, prob = prob)
        ps_est <- 1 - est_IC$qi
        pi_est <- 1 - est_IC$qs
      }
    }else{
      est_IC <- qx_ci(fit, age = 0:max_age, prob = prob)
      ps_est <- 1 - est_IC$qi
      pi_est <- 1 - est_IC$qs
    }


  #funcao iteracao_exp
  iteracao_exp <- function(x){
    ex_est <- c(x[1])
    for(i in 2:length(x)){
      ex_est[i] <- ex_est[i-1]*x[i]
    }
    exp_total <- c(sum(ex_est))
    for(i in 2:length(x)){
      ex_est <- ex_est/ex_est[i-1]
      exp_total[i] <- sum(ex_est[i:length(x)])
    }
    return(exp_total)
  }

  ### IC superior:
  exp_sup <- round(iteracao_exp(ps_est),2)
  ### IC inferior:
  exp_inf <- round(iteracao_exp(pi_est),2)
  ### Expectativa:
  exp_est <- round(iteracao_exp(px_est),2)

  tab <- data.frame(x = 0:max(age),
                    exp_est[1:(max(age)+1)],
                    exp_inf[1:(max(age)+1)],
                    exp_sup[1:(max(age)+1)])
  colnames(tab) <- c("Age","Expectancy","Lower CI","Upper CI")

  if(graph == TRUE){
    p <-  ggplot(data=tab) + theme_light() +
      geom_line(aes(x=Age,y=Expectancy)) +
      geom_ribbon(aes(x=Age, ymin=`Lower CI`, ymax=`Upper CI`), alpha=0.3)
    return(list(tabua=tab[tab$Age %in% age,],
                plot=p))
  }else{
    return(tab[tab$Age %in% age,])
  }
}


#' @export
#'
expectancy.ClosedHP <- function(fit,
                          Ex = NULL,
                          age = seq(0, max(fit$data$x),by = 10),
                          graph = TRUE,
                          max_age = 110,
                          prob = 0.95){
  ##calculo do qx e px estimados.
  #fechando a tabua::
  if(length(fit$qx[1,]) < max_age){
    warning("Closure do not reach max_age, it is not possible get an accurate estimate. Verify the fitting range.")
    qx_est <- fitted(fit)
    px_est <- 1 - qx_est$qx_fitted
    #####IC
    est_IC <- qx_ci(fit, prob = prob)
    ps_est <- 1 - est_IC$qi
    pi_est <- 1 - est_IC$qs
  }else{
    qx_est <- fitted(fit, age = 0:max_age)
    px_est <- 1 - qx_est$qx_fitted
    #####IC
    est_IC <- qx_ci(fit, age = 0:max_age, prob = prob)
    ps_est <- 1 - est_IC$qi
    pi_est <- 1 - est_IC$qs
  }
  #funcao iteracao_exp
  iteracao_exp <- function(x){
    ex_est <- c(x[1])
    for(i in 2:length(x)){
      ex_est[i] <- ex_est[i-1]*x[i]
    }
    exp_total <- c(sum(ex_est))
    for(i in 2:length(x)){
      ex_est <- ex_est/ex_est[i-1]
      exp_total[i] <- sum(ex_est[i:length(x)])
    }
    return(exp_total)
  }

  ### IC superior:
  exp_sup <- round(iteracao_exp(ps_est),2)
  ### IC inferior:
  exp_inf <- round(iteracao_exp(pi_est),2)
  ### Expectativa:
  exp_est <- round(iteracao_exp(px_est),2)

  tab <- data.frame(x = 0:max(age),
                    exp_est[1:(max(age)+1)],
                    exp_inf[1:(max(age)+1)],
                    exp_sup[1:(max(age)+1)])
  colnames(tab) <- c("Age","Expectancy","Lower CI","Upper CI")

  if(graph == TRUE){
    p <-  ggplot(data=tab) + theme_light() +
      geom_line(aes(x=Age,y=Expectancy)) +
      geom_ribbon(aes(x=Age, ymin=`Lower CI`, ymax=`Upper CI`), alpha=0.3)
    return(list(tabua=tab[tab$Age %in% age,],
                plot=p))
  }else{
    return(tab[tab$Age %in% age,])
  }
}
