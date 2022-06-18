#' @title
#' Mean plot with error bars
#'
#' @description
#' Plots group means with error bars. Error bars can be standard deviations,
#' standard errors, or confidence intervals. Optionally, plots can be based on
#' robust statistics.
#'
#' @param y a numeric response variable.
#' @param x a categorical explanatory variable. 
#' @param by a second categorical explanatory variable (optional).
#' @param data a data frame.
#' @param pointsize numeric. Point size (default = 2).
#' @param dodge numeric. If a \code{by} variable is included, points and error bars are
#' dodged by this amount in order to avoid overlap (default = 0.2).
#' @param lines logical. If \code{TRUE}, group means are connected.
#' @param width numeric. Width of the error bars (default = 0.2). 
#' Set to 0 to produce pointranges instead of error bars.
#' @param error_type character. Error bars represents either standard deviations
#'  \code{(sd)}, standard errors of the means  \code{(se)}, or confidence intervals
#'  \code{(ci)}. The default is the standard error.
#' @param percent numeric. if \code{error_type = "ci"}, this indicates the size of the confidence
#' interval. The default is \code{0.95} or a 95 percent confidence interval for the mean.
#' @param robust logical. If \code{TRUE}, the means, standard deviations, standard errors,
#' and confidence intervals are based on robust statistics. See \code{Details}. 
#' The default is \code{FALSE}.
#' @import dplyr
#' @import ggplot2
#' @importFrom stats qt quantile
#' @return a \code{ggplot2} graph.
#' @export
#' @details
#' Robust statistics are based on deciles, the nine values that divide the response variable into 
#' 10 equal groups (where each group contains roughly the same fraction of cases). The robust 
#' mean is the mean of these nine decile values. The robust standard deviation is the sample 
#' standard deviation of the nine decile values. The standard error and confidence interval are 
#' calculated in the normal way, but use the robust mean and standard deviation in 
#' their calculations. See Abu-Shawiesh et al (2022).
#' @references 
#' Ahmed Abu-Shawiesh, M., Sinsomboonthong, J., & Kibria, B. (2022). A modified robust
#' confidence interval for the population mean of distributrion baed on deciles. 
#' \emph{Statistics in Transition}, vol. \emph{23 (1)}.
#' \href{https://tinyurl.com/bdz7umj8}{pdf}
#' @examples
#' mean_plot(cars74, mpg, cyl)
#' mean_plot(cars74, mpg, cyl, am)
#' mean_plot(cars74, mpg, cyl, am, 
#'           error_type = "ci", percent = 0.9,
#'           width = 0, lines = FALSE, robust = TRUE)
mean_plot <- function(data, y, x, by, 
                      pointsize = 2,
                      dodge = 0.2,
                      lines = TRUE,
                      width = .2,
                      error_type = c("se", "sd", "ci"), 
                      percent=.95,
                      robust = FALSE){
  x <- enquo(x)
  y <- enquo(y)
  by <- enquo(by)
  error_type <- match.arg(error_type)
  if (percent > 1) percent <- percent/100
  
  # calculate means and standard errors by x and optional by variable
  if (as_label(by) == "<empty>"){
    dodge = 0
    plotdata <- data %>% group_by(!!x)
  } else {
    plotdata <- data %>% group_by(!!x, !!by)
  }
  
  if (robust == FALSE){
    plotdata <- plotdata %>%
      summarize(n = n(),
                mean = mean(!!y),
                sd = sd(!!y),
                se = sd/sqrt(n),
                ci = qt(1 - (1-percent)/2, df = n - 1) * sd / sqrt(n),
                .groups = "drop")
  }
  else {
    # calculate decile based statistics
    deciles <- NA  # for CRAN
    plotdata <- plotdata %>%
      summarize(n = n(),
                deciles = quantile(!!y, probs = seq(.1, .9, by = .1)),
                .groups = "keep")
    plotdata <- plotdata %>%
      summarize(mean = mean(deciles),
                sd = sqrt(sum((deciles - mean)^2)/8),
                se = sd/sqrt(n),
                ci = qt(1 - (1-percent)/2, df = n - 1) * sd / sqrt(n),
                .groups = "drop") %>%
      unique()
  }
  
  # specify error  to use
  if (error_type == "sd") plotdata$error <- plotdata$sd
  if (error_type == "se") plotdata$error <- plotdata$se
  if (error_type == "ci") plotdata$error <- plotdata$ci
  
  # plot the means and  errors bars by sex (dodged)
  pd <- position_dodge(dodge)
  
  # is there a "by" variable?
  if (as_label(by) == "<empty>"){
    p <- ggplot(plotdata, aes(x = !!x,  y = mean, group = 1))
  } else {
    p <- ggplot(plotdata, aes(x = !!x, y = mean, group = !!by, color = !!by))
  }
  
  # points
  p <- p + geom_point(position = pd, size = pointsize)
  
  # lines
  if (lines) {
    p <- p + geom_line(position = pd)
  }
  
  # error bars
  p <- p + geom_errorbar(aes(ymin = mean - error, 
                             ymax = mean + error), 
                         width = width,
                         position = pd)
  
  # theme 
  p <- p + theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank())
  
  # labels
  if (error_type == "sd") caption <- "Mean \u00B1 standard deviation"
  if (error_type == "se") caption <- "Mean \u00B1 standard error"
  if (error_type == "ci") caption <- paste0("Mean \u00B1 ", percent*100, "% confidence interval")
  if (robust == TRUE) caption <- paste(caption, "\nbased on robust statistics")
  p <- p + labs(y = as_label(y),
                caption = caption)
  return(p)
}