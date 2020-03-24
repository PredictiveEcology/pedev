#' Remove objects that start with "dot" "underscore"
#'
#' A quick wrapper for removing a objects with specials names,
#' \code{"._xxx"}.
#'
#' @param envir The environment from with to rm objects, defaults to
#'   \code{.GlobalEnv}
#' @export
rmDotUnderline <- function(envir = .GlobalEnv)
  rm(list = ls(all.names = TRUE, envir = envir)[startsWith(ls(all.names = TRUE, envir = envir), "._")],
     envir = envir)

