#' @title Get help on a package
#'
#' @description
#' \code{phelp} provides help on an installed package.
#'
#' @details
#' This function provides help on an installed package. The
#' package does not have to be loaded. The package name does
#' not need to be entered with quotes.
#'
#' @param pckg The name of a package
#' @export
#' @examples
#' # phelp(ggplot2)
#'
phelp <- function(pckg) {
  utils::help(package=deparse(substitute(pckg)))
}
