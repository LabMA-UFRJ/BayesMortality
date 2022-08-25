#' @name Heatmap
#' @rdname Heatmap
#'
#' @title HP Model: Heatmap for the life expectancy.
#'
#' @description This function plots a heatmap for the life expectancy of the fitted HP models.
#'
#' @usage
#' Heatmap(fits, x_lab, age = 0:90, max_age = 110, color = c("red","white","blue"))
#'
#' @param fits Object or a list of objects of the class 'HP' or 'ClosedHP' returned by the hp() or close_hp() functions.
#' @param x_lab Vector with the identification of each object in "fits", in the same order.
#' @param age Vector with the ages to plot the heatmap.
#' @param max_age Positive number indicating  the last age to be used in the calculation of the life expectancy. The default value is 110.
#' @param color A length 3 vector with the colors used in the heatmap.
#'
#' @return Heatmap of the life expectancy.
#'
#' @examples
#' ## Importing mortality data from USA
#' data(USA)
#'
#' # Example 1: -------------------------------
#'
#' USA2010 = USA[USA$Year == 2010,]
#'
#' ExF = USA2010$Ex.Female[1:91]
#' DxF = USA2010$Dx.Female[1:91]
#' x <- 0:90
#'
#' fitF <- hp(x, ExF, DxF, model = "lognormal")
#'
#' Heatmap(fitF, x_lab = "Female expec. 2010 USA")
#'
#'
#' # Example 2: -------------------------------
#'
#' # A plot with more than one fitted model:
#'
#' ExM = USA2010$Ex.Male[1:91]
#' DxM = USA2010$Dx.Male[1:91]
#'
#' fitM <- hp(x, ExM, DxM, model = "lognormal")
#'
#' fits <- list(fitF = fitF, fitM = fitM)
#'
#' Heatmap(fits, x_lab = c("Female 2010 USA","Male 2010 USA"),
#'         age = 15:85, max_age = 100, color = c("pink","black","green"))
#'
#' @include expectancy_hp.R
#'
#' @import ggplot2
#' @export
#'
Heatmap.HP <- function(fits, x_lab, age = 0:90, max_age = 110, color = c("red","white","blue")){
  #checks de integridade:
  if(class(fits) == "list"){
    if(length(fits) != length(x_lab)){stop("Number of fitted models is different of the x_lab's length.")}
    }
  if(length(color) != 3){stop("The argument color must be a 3 length vector.")}

  #calculando a exp_vida
  if(class(fits) == "list"){
    lista_exp <- lapply(fits, expectancy, graph = FALSE, age = age, max_age = max_age)
    exps = NULL
    for(i in 1:length(lista_exp)){
      exps <- rbind(exps,lista_exp[[i]])
    }
  }else{
    exps <- expectancy(fits, graph = FALSE, age = age, max_age = max_age)
  }

  #criando dataframe para heatmap:
  exp <- exps$Expectancy
  ano <- c()
  for(i in 1:length(x_lab)){
    ano <- c((rep(x_lab[i],length(age))),ano)
  }
  idade <- exps$Age
  df <- data.frame(
    "age" = idade,
    "year" = rev(as.character(ano)),
    "exp" = exp)

  #plotando heatmap
  p <- ggplot(df) + theme_light() +
    geom_raster(aes(x = year, y = age, fill = exp),interpolate = TRUE) +
    labs(x="",
         y="Age",
         title = "Life expectancy") +
    scale_fill_gradient2(name = "Expectancy (years)",
                         low = color[1],
                         mid = color[2],
                         high = color[3],
                         midpoint = 40)

  return(p)
}

#' @export
#'
Heatmap.list <- function(fits, x_lab, age = 0:90, max_age = 110, color = c("red","white","blue")){
  #checks de integridade:
  if(class(fits) == "list"){
    if(length(fits) != length(x_lab)){stop("Number of fitted models is different of the x_lab's length.")}
  }
  if(length(color) != 3){stop("The argument color must be a 3 length vector.")}

  #calculando a exp_vida
  if(class(fits) == "list"){
    lista_exp <- lapply(fits, expectancy, graph = FALSE, age = age, max_age = max_age)
    exps = NULL
    for(i in 1:length(lista_exp)){
      exps <- rbind(exps,lista_exp[[i]])
    }
  }else{
    exps <- expectancy(fits, graph = FALSE, age = age, max_age = max_age)
  }

  #criando dataframe para heatmap:
  exp <- exps$Expectancy
  ano <- c()
  for(i in 1:length(x_lab)){
    ano <- c((rep(x_lab[i],length(age))),ano)
  }
  idade <- exps$Age
  df <- data.frame(
    "age" = idade,
    "year" = rev(as.character(ano)),
    "exp" = exp)

  #plotando heatmap
  p <- ggplot(df) + theme_light() +
    geom_raster(aes(x = year, y = age, fill = exp),interpolate = TRUE) +
    labs(x="",
         y="Age",
         title = "Life expectancy") +
    scale_fill_gradient2(name = "Expectancy (years)",
                         low = color[1],
                         mid = color[2],
                         high = color[3],
                         midpoint = 40)

  return(p)
}

#' @export
#'
Heatmap.ClosedHP <- function(fits, x_lab, age = 0:90, max_age = 110, color = c("red","white","blue")){
  #checks de integridade:
  if(class(fits) == "list"){
    if(length(fits) != length(x_lab)){stop("Number of fitted models is different of the x_lab's length.")}
  }
  if(length(color) != 3){stop("The argument color must be a 3 length vector.")}

  #calculando a exp_vida
  if(class(fits) == "list"){
    lista_exp <- lapply(fits, expectancy, graph = FALSE, age = age, max_age = max_age)
    exps = NULL
    for(i in 1:length(lista_exp)){
      exps <- rbind(exps,lista_exp[[i]])
    }
  }else{
    exps <- expectancy(fits, graph = FALSE, age = age, max_age = max_age)
  }

  #criando dataframe para heatmap:
  exp <- exps$Expectancy
  ano <- c()
  for(i in 1:length(x_lab)){
    ano <- c((rep(x_lab[i],length(age))),ano)
  }
  idade <- exps$Age
  df <- data.frame(
    "age" = idade,
    "year" = rev(as.character(ano)),
    "exp" = exp)

  #plotando heatmap
  p <- ggplot(df) + theme_light() +
    geom_raster(aes(x = year, y = age, fill = exp),interpolate = TRUE) +
    labs(x="",
         y="Age",
         title = "Life expectancy") +
    scale_fill_gradient2(name = "Expectancy (years)",
                         low = color[1],
                         mid = color[2],
                         high = color[3],
                         midpoint = 40)

  return(p)
}
