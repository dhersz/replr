#' An R REPL
#'
#' A not yet fully function R REPL.
#'
#' @param prompt A string. The text prompt.
#'
#' @export
replr <- function(prompt = ">>>> ") {

  suppressWarnings(

    while (TRUE) {

      input <- readline(prompt)

      if (input == "q()") break

      expr <- tryCatch(
        parse(text = input),
        error = function(cnd) cnd
      )

      if ("error" %in% class(expr)) {

        message("Error: ", expr$message)

      } else {

        result <- withVisible(
          withCallingHandlers(
            tryCatch(
              eval(expr),
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
