#' Recode one or more variables
#'
#' \code{recodes} recodes the values of one or more variables in
#' a data frame
#'
#' @param data a data frame.
#' @param vars character vector of variable names.
#' @param from a vector of values or conditions (see Details).
#' @param to a vector of replacement values.
#'
#' @details
#' \itemize{
#' \item For each variable in the \code{vars} parameter, values
#' are checked against the list of values in the \code{from} vector.
#' If a value matches, it is replaced with the corresponding
#' entry in the \code{to} vector.
#' \item Once a given observation's value matches a \code{from} value, it is
#' recoded. That particular observation will not be recoded again by
#' that \code{recodes()} statement (i.e., no chaining).
#' \item One or more values in the \code{from} vector can be an expression,
#' using the dollar sign ($) to represent the variable being recoded.
#' If the expression
#' evaluates to \code{TRUE}, the corresponding \code{to} value is
#' returned.
#' \item If the number of values in the \code{to} vector is less than
#' the \code{from} vector, the values are recycled. This lets you
#' convert several values to a single outcome value (e.g., \code{NA}).
#' \item If the \code{to} values are numeric, the resulting recoded variable
#' will be numeric. If the variable being recoded is a factor and the
#' \code{to} values are character values, the resulting variable will
#' remain a factor. If the variable being recoded is a character variable
#' and the \code{to} values are character values, the resulting
#' variable will remain a character variable.
#' }
#'
#' @note
#' See the vignette for a worked example.
#
#' @export
#' @return a data frame
#' @examples
#' \dontrun{
#' # For variables A, B and C, convert 86, 99, and 999 to missing
#' df <- recodes(df, vars=c("A", "B", "C"), from=c(86, 99, 999), to=NA)
#'
#' # For variables A, B, and C convert NA to 0
#' df <- recodes(df, vars=c("A", "B", "C"), from=NA, to=0)
#'
#' # For variable X1, X2, and X3 convert 1 to Yes and 2 to No
#' df <- recodes(df, vars=c("X1", "X2", "X3"),
#'               from=c(1, 2), to=c("Yes", "No"))
#'
#' # For variable SEX convert m to Male and f to Female
#' df <- recodes(df, vars="SEX",
#'               from=c("m", "f"), to=c("Male", "Female"))
#'
#' # For variable OUTCOME convert Live to 0 and Die to 1
#' df <- recodes(df, vars="OUTCOME",
#'               from=c("live", "die"), to=c(0, 1))
#'
#' # For variable AGE convert age <= 20 to young,
#' #    20 < age < =60 to middle, and age > 60 to old
#' df <- recodes(df, vars="AGE",
#'               from=c("$ < = 20", "$ > 20 & $ <= 60", "$  > 60"),
#'               to=c("young", "middle", "old"))
#'
#' # Reverse code variable Rating from 1 to 10, to 10 to 1
#' df <- recodes(df, vars="Rating", from=1:10, to=10:1)
#'
#' }
#'
recodes <- function(data, vars, from, to){
  df <- as.character(substitute(data))

  # special character representing the variable being coded when
  # the 'from' vector contains conditions rather than values

  pattern <- "\\$"

  # recycle 'from' to match 'to' in length
  if (length(from) > length(to)){
    to <- rep_len(to, length(from))
    message("Note: 'from' is longer than 'to', so 'to' was recycled.")
  }

  for(i in vars){

    # set indicator for factors
    factorflag <- FALSE

    # iterate over variables to be coded
    if (i %in% names(data)){

      # convert factors to character
      if (is.factor(data[[i]])){
        data[[i]] <- as.character(data[[i]])
        factorflag <- TRUE
      }

      # has a value been changed?
      # we only want to change a given value once
      changed <- rep_len(FALSE, nrow(data))

      # set x to current variable
      x <- data[[i]]

      # iterate thru 'from' values
      for(j in seq_along(from)){

        f <- from[j] # current 'from' value
        t <- to[j]   # corresponding 'to' value


        ## TODO !!!!!
        # if from is NA what should changed be?
        # test this

        # evaluate whether values have been changed
        changed   <- x != data[[i]]

        # deal with possible missing values in x or data[[i]]
        changedNA <- is.na(x) != is.na(data[[i]])
        for (k in seq_along(changed)){
          if(is.na(changed[k])){
            changed[k] <- changedNA[k]
          }
        }

        # f is NA
        if (is.na(f)){
          x <- ifelse(is.na(x) & !changed, t, x)

          # f is an condition on x
        } else if (grepl(pattern, f)) {
          replacement <- paste0(df,"[['", i, "']]")
          condition <- gsub(pattern, replacement, f)
          x <- ifelse(eval(parse(text = condition),
                           envir=parent.frame()) & !changed,
                      t, x)
          # otherwise
        } else {
          x <- ifelse(x == f & !changed, t, x)

        }

      }

      # output new variable vector
      data[[i]] <- x

      # if x was factor, convert back to factor
      if (factorflag){
        data[[i]] <- factor(data[[i]])
      }

      # if 'to' value is numeric, convert x to numeric
      if (is.numeric(to)){
        data[[i]] <- as.numeric(data[[i]])
      }
    }
  }
  return(data)
}
