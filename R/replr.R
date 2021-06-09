#' An R REPL
#'
#' A simple - not yet fully functional - R REPL.
#'
#' @param prompt A string. The text prompt.
#' @param continue A string. The text prompt when from the second line onward if
#'   a multiline expression is passed.
#'
#' @export
replr <- function(prompt = ">>>> ", continue = "++++ ") {

  replr_env <- new.env(parent = as.environment("Autoloads"))
  attr(replr_env, "name") <- "replr_global"

  # base function rewritten to work with the REPL

  replr_env$search <- custom_search

  # the REPL itself

  suppressWarnings(

    while (TRUE) {

      input <- readline(prompt)

      if (input == "q()") break

      expr <- tryCatch(
        parse(text = input),
        error = function(cnd) cnd
      )

      while ("error" %in% class(expr) &&
             (grepl("INCOMPLETE_STRING", expr$message) |
              grepl("end of input", expr$message))) {

        cont_input <- readline(continue)

        input <- paste(input, cont_input, sep = "\n")

        expr <- tryCatch(
          parse(text = input),
          error = function(cnd) cnd
        )

      }

      if ("error" %in% class(expr)) {

        message("Error: ", expr$message)

      } else {

        result <- withVisible(
          withCallingHandlers(
            tryCatch(
              eval(expr, envir = replr_env),
              error = function(cnd) message("Error: ", cnd$message)
            ),
            warning = function(cnd) message("Warning: ", cnd$message)
          )
        )

        if (result$visible)
          print(result$value)

      }

    }

  )

}
