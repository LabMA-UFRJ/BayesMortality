#' @name plot
#' @rdname plot
#'
#' @title HP Model: Plotting the life tables.
#'
#' @description Function that returns a log-scale ggplot of the 'HP' and 'ClosedHP' objects returned by the hp() and hp_close() functions.
#'
#' @usage
#' plot(fits, age = NULL, Ex = NULL, plotIC = TRUE,
#'  plotData = TRUE, labels = NULL, colors = NULL,
#'  prob = 0.95)
#'
#' @param fits Object or a list of objects of the class 'HP' or 'ClosedHP' returned by the hp() or hp_close() functions.
#' @param age Vector with the ages to plot the life table(s).
#' @param Ex Vector with the exposures in the age period, having it's length equal to the age vector. This argument is only necessary when plotting poisson and/or binomial models.
#' @param plotIC Logical. If TRUE, plots the credible intervals (prediction intervals). Default: TRUE.
#' @param plotData Logical. If TRUE, plots the data used in the modelling as dots. Default: TRUE.
#' @param labels Vector with the name of the labels to be used in the legend, having it's length equal to the fits argument (Optional).
#' @param colors Vector with the colors to be used in the plot, having it's length equal to the fits argument (Optional).
#' @param prob Probability of the credible intervals. Default: 0.95
#'
#' @return A ggplot object with fitted life table(s).
#'
#' @examples
#' ## Selecting the exposure and the death count of the year 1990, ranging from 0 to 90 years old:
#' USA1990 = USA[USA$Year == 1990,]
#' x = 0:90
#' Ex = USA1990$Ex.Male[x+1]
#' Dx = USA1990$Dx.Male[x+1]
#'
#' ## Fitting a poisson and a lognormal model:
#' fit = hp(x = x, Ex = Ex, Dx = Dx, model = "poisson",
#'          M = 40000, bn = 10000, thin = 30)
#' fit2 = hp(x = x, Ex = Ex, Dx = Dx, model = "lognormal",
#'           M = 40000, bn = 10000, thin = 30)
#'
#' ## Plotting the life tables:
#' plot(fit)
#' plot(fit2, age = 0:110, plotIC = TRUE)
#' plot(list(fit, fit2),
#'      age = 0:110, Ex = USA1990$Ex.Male,
#'      plotIC = FALSE, colors = c("red", "blue"),
#'      labels = c("Poisson", "Lognormal"))
#'
#' @include qx_ci.R
#' @include fitted_hp.R
#' @include fun_aux.R
#'
#' @import ggplot2
#' @import scales
#'
#' @export
plot.HP <- function(fits, age = NULL, Ex = NULL, plotIC = TRUE, plotData = TRUE, labels = NULL, colors = NULL, prob = 0.95){

  qx_fit = fitted(fits, age = age)
  qx_ci = qx_ci(fits, age = age, Ex = Ex, prob = prob)

  ## Customizing the plot
  if(is.null(colors)) { colors = "seagreen" }
  if(is.null(labels)) { labels = "HP fitted" }

  ## Organizing data
  data = fits$data
  data$Model = "data"
  data <- na.omit(data)
  if(plotData){
    new_labels <- append(labels, "data", 0)
    new_colors <- append(colors, "gray10", 0)
  }else{
    new_labels = labels
    new_colors = colors
  }

  ## Life table's lower limit:
  li_y <- decimal(min(c(qx_ci$qi[qx_ci$qi > 0], data$qx[data$qx > 0], qx_fit$qx_fitted[qx_fit$qx_fitted > 0]), na.rm = T))

  if(!is.null(age)) { data = data[data$x %in% age, ] }

  ## Plot base:
  g <- ggplot2::ggplot() +
    ggplot2::scale_y_continuous(trans = 'log10', breaks = 10^-seq(li_y,0), limits = 10^-c(li_y,0), labels = scales::comma) +
    ggplot2::scale_x_continuous(breaks = seq(0, 200, by = 10), limits = c(NA, NA)) +
    ggplot2::xlab("Age") + ggplot2::ylab("Qx") + ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 1.2),
                   axis.title.x = ggplot2::element_text(color = 'black', size = 12),
                   axis.title.y = ggplot2::element_text(color = 'black', size = 12),
                   axis.text = ggplot2::element_text(color = 'black', size = 12),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.position = "bottom")

  if(plotIC){
    if(plotData){
      g + ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = qx, col = "data"), alpha = 1, size = 0.8) +
        ggplot2::geom_ribbon(data = qx_ci, ggplot2::aes(x = age, ymin = qi, ymax = qs, fill = "fitted"), alpha = 0.3) +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels) +
        ggplot2::scale_fill_manual(name = NULL, values = colors) + ggplot2::guides(fill = "none")
    }else{
      g +
        ggplot2::geom_ribbon(data = qx_ci, ggplot2::aes(x = age, ymin = qi, ymax = qs, fill = "fitted"), alpha = 0.3) +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels) +
        ggplot2::scale_fill_manual(name = NULL, values = colors) + ggplot2::guides(fill = "none")
    }
  }else{
    if(plotData){
      g + ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = qx, col = "data"), alpha = 1, size = 0.8) +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels)
    }else{
      g +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels)
    }
  }
}

#' @export
plot.ClosedHP <- function(fits, age = NULL, plotIC = TRUE, plotData = TRUE, labels = NULL, colors = NULL, prob = 0.95){

  qx_fit = na.omit(fitted(fits, age = age))
  qx_ci = na.omit(qx_ci(fits, age = age, Ex = Ex, prob = prob))

  ## Customizing the plot
  if(is.null(colors)) { colors = "seagreen" }
  if(is.null(labels)) { labels = "HP fitted" }

  ## Organizing data
  data = fits$data
  data$Model = "data"
  data <- na.omit(data)
  if(plotData){
    new_labels <- append(labels, "data", 0)
    new_colors <- append(colors, "gray10", 0)
  }else{
    new_labels = labels
    new_colors = colors
  }

  ## Life table's lower limit:
  li_y <- decimal(min(c(qx_ci$qi[qx_ci$qi > 0], data$qx[data$qx > 0], qx_fit$qx_fitted[qx_fit$qx_fitted > 0]), na.rm = T))

  if(!is.null(age)) { data = data[data$x %in% age, ] }

  ## Plot base:
  g <- ggplot2::ggplot() +
    ggplot2::scale_y_continuous(trans = 'log10', breaks = 10^-seq(li_y,0), limits = 10^-c(li_y,0), labels = scales::comma) +
    ggplot2::scale_x_continuous(breaks = seq(0, 200, by = 10), limits = c(NA, NA)) +
    ggplot2::xlab("Age") + ggplot2::ylab("Qx") + ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 1.2),
                   axis.title.x = ggplot2::element_text(color = 'black', size = 12),
                   axis.title.y = ggplot2::element_text(color = 'black', size = 12),
                   axis.text = ggplot2::element_text(color = 'black', size = 12),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.position = "bottom")

  if(plotIC){
    if(plotData){
      g + ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = qx, col = "data"), alpha = 1, size = 0.8) +
        ggplot2::geom_ribbon(data = qx_ci, ggplot2::aes(x = age, ymin = qi, ymax = qs, fill = "fitted"), alpha = 0.3) +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels) +
        ggplot2::scale_fill_manual(name = NULL, values = colors) + ggplot2::guides(fill = "none")
    }else{
      g +
        ggplot2::geom_ribbon(data = qx_ci, ggplot2::aes(x = age, ymin = qi, ymax = qs, fill = "fitted"), alpha = 0.3) +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels) +
        ggplot2::scale_fill_manual(name = NULL, values = colors) + ggplot2::guides(fill = "none")
    }
  }else{
    if(plotData){
      g + ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = qx, col = "data"), alpha = 1, size = 0.8) +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels)
    }else{
      g +
        ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = "fitted"), size = 0.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels)
    }
  }
}

#' @export
plot.list <- function(fits, age = NULL, Ex = NULL, plotIC = TRUE, plotData = TRUE, labels = NULL, colors = NULL, prob = 0.95){

  if(class(fits) == "list" && all(unlist(lapply(fits, class)) %in% c("HP", "ClosedHP"))){

    n_models <- length(fits)

    ## Checking ages
    if(is.null(age)) {
      age <- list(); data_aux = list()
      for(i in 1:n_models){ age[[i]] = fits[[i]]$data$x; data_aux[[i]] = fits[[i]]$data }
    } else {
      aux = age; age = list(); data_aux = list()
      for(i in 1:n_models){
        age[[i]] = aux
        data_aux[[i]] = fits[[i]]$data[fits[[i]]$data$x %in% age[[i]], ]
      }
    }

    ## qx fit and ic
    qx_fit = qx_cin = n_aux = NULL
    for(i in 1:n_models){
      aux1 = fitted(fits[[i]], age = age[[i]])
      aux2 = qx_ci(fits[[i]], age = age[[i]], Ex = Ex, prob = prob)
      qx_fit <- rbind(qx_fit, aux1)
      qx_cin <- rbind(qx_cin, aux2)
      n_aux[i] <- nrow(aux1)
    }
    qx_fit$Model = paste("Model", rep(1:n_models, n_aux))
    qx_cin$Model = paste("Model", rep(1:n_models, n_aux))
    qx_fit = na.omit(qx_fit)
    qx_cin = na.omit(qx_cin)

    oneModel = F

    ## Customizing the plot
    if(n_models == 1){
      oneModel = T
      if(is.null(colors)) { colors = "seagreen" }
      if(is.null(labels)) { labels = "HP fitted" }
    }

    ## checking if there are color or label inputs
    if(is.null(labels)){ labels <- unique(qx_fit$Model) }

    if(length(labels) != n_models) {
      warning("The number of labels does not match the number of models to plot.")
      labels <- unique(qx_fit$Model)
    }

    if(is.null(colors)){ colors = scales::hue_pal()(n_models) }

    if(length(colors) != n_models) {
      warning("The number of selected colors does not match the number of models to plot.")
      colors = scales::hue_pal()(n_models)
    }

    ## Organizing data
    if(!oneModel){
      data = NULL
      for(i in 1:n_models){
        data_aux[[i]]$Model = paste("Model", i)
        data <- rbind(data, data_aux[[i]])
      }
      new_labels = labels
      new_colors = colors
      data <- na.omit(data)
      data$Model <- factor(data$Model, labels = new_labels, levels = paste("Model", 1:n_models))
      qx_cin$Model <- factor(qx_cin$Model, labels = new_labels, levels = paste("Model", 1:n_models))
      qx_fit$Model <- factor(qx_fit$Model, labels = new_labels, levels = paste("Model", 1:n_models))
    }else{
      data = data_aux[[1]]
      data$Model = "data"
      data <- na.omit(data)
      if(plotData){
        new_labels <- append(labels, "data", 0)
        new_colors <- append(colors, "gray10", 0)
      }else{
        new_labels = labels
        new_colors = colors
      }
    }

    ## Life table's lower limit:
    li_y <- decimal(min(c(qx_ci$qin[qx_cin$qi > 0], data$qx[data$qx > 0], qx_fit$qx_fitted[qx_fit$qx_fitted > 0]), na.rm = T))

    ## Plot base:
    g <- ggplot2::ggplot() +
      ggplot2::scale_y_continuous(trans = 'log10', breaks = 10^-seq(li_y,0), limits = 10^-c(li_y,0), labels = scales::comma) +
      ggplot2::scale_x_continuous(breaks = seq(0, 200, by = 10), limits = c(NA, NA)) +
      ggplot2::xlab("Age") + ggplot2::ylab("Qx") + ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 1.2),
                     axis.title.x = ggplot2::element_text(color = 'black', size = 12),
                     axis.title.y = ggplot2::element_text(color = 'black', size = 12),
                     axis.text = ggplot2::element_text(color = 'black', size = 12),
                     legend.text = ggplot2::element_text(size = 12),
                     legend.position = "bottom")

    if(plotIC){
      if(plotData){
        g + ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = qx, col = Model), alpha = 1, size = 0.8) +
          ggplot2::geom_ribbon(data = qx_cin, ggplot2::aes(x = age, ymin = qi, ymax = qs, fill = Model), alpha = 0.3) +
          ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = Model), size = 0.8, alpha = 0.8) +
          ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels) +
          ggplot2::scale_fill_manual(name = NULL, values = colors) + ggplot2::guides(fill = "none")
      }else{
        g +
          ggplot2::geom_ribbon(data = qx_cin, ggplot2::aes(x = age, ymin = qi, ymax = qs, fill = Model), alpha = 0.3) +
          ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = Model), size = 0.8, alpha = 0.8) +
          ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels) +
          ggplot2::scale_fill_manual(name = NULL, values = colors) + ggplot2::guides(fill = "none")
      }
    }else{
      if(plotData){
        g + ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = qx, col = Model), alpha = 1, size = 0.8) +
          ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = Model), size = 0.8, alpha = 0.8) +
          ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels)
      }else{
        g +
          ggplot2::geom_line(data = qx_fit, ggplot2::aes(x = age, y = qx_fitted, col = Model), size = 0.8, alpha = 0.8) +
          ggplot2::scale_colour_manual(name = NULL, values = new_colors, labels = new_labels)
      }
    }
  }else{
    stop("fits argument must be an object or a list of HP or ClosedHP objects.")
  }
}
