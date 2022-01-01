#' @title Clear/reset an RStudio session
#' @description
#' Clear/reset an RStudio session
#' @export
#' @importFrom grDevices dev.list dev.off
#' @details
#' The \code{clear()} function:
#' \enumerate{
#' \item Clears the console window
#' \item Deletes objects from the global environment
#' \item Deletes plots and plot history from the Plots window
#' \item Unloads all but base packages (unless \code{detach=FALSE})
#' \item Releases memory (garbage collection)
#' }
#' R scripts and Rmarkdown files are left untouched.
#' No objects saved to disk are affected.
#' @param detach logical. If \code{TRUE}, unload
#' all but base packages.
#' @note
#' This is a good way to start a fresh R session without
#' having to quit and restart RStudio. Additionally, any script
#' or Rmarkdown files remain open, the current working directory
#' is unchanged, and you remain in any open project.
#' @examples
#' \dontrun{
#' clear()
#' }
#'
clear <- function(detach=TRUE){
  rm(list = ls(all.names=TRUE, envir = .GlobalEnv), envir=.GlobalEnv)
  gc()
  cat("\014")
  # Clear all plots
  try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
  try(dev.off(),silent=TRUE)
  if (detach){
    base.packages <- c("package:stats",
                        "package:graphics",
                        "package:grDevices",
                        "package:utils",
                        "package:datasets",
                        "package:methods",
                        "package:base")

    loaded.packages <- search()[ifelse(unlist(gregexpr("package:",
                                                 search()))==1,TRUE,FALSE)]

    nonbase.packages <- setdiff(loaded.packages, base.packages)

    if (length(nonbase.packages)>0){
      for (i in nonbase.packages){
        detach(i, character.only=TRUE)
      }
    }
  }
}

