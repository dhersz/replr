custom_search <- function(){

  env <- parent.frame()

  packages <- character()

  while (!identical(env, emptyenv())) {

    packages <- c(packages, environmentName(env))
    env <- parent.env(env)

  }

  # base -> package:base
  packages <- sub("^base$", "package:base", packages)

  return(packages)

}
