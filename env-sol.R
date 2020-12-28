# a) ----
# In environments every name must be unique and the names are not ordered. Also 
# an environment has a parent and environments are not copied when modified.

# b) ----
# Current environment

# c) ----
# <- :  assigns into the environment in which it is evaluated
# <<- : search through parent environments; if variable is found and binding not
#       locked, value is redefined; otherwise assignment in global environment

# d) ----

#' Function that returns all environments in the search path that contain a 
#' variable of the given name in the form of a list. 
#'
#' @param name name to look for
#' @param env environment to start at. Defaults to the calling environment of 
#' this function
#'
#' @return list of environments
anywhere <- function(name, env = parent.frame()) {
  
  checkmate::assert_environment(env)
  checkmate::assert_character(name)
  
  stopifnot(is.character(name), length(name) == 1)
  env <- pryr:::to_env(env)
  
  env_list <- list()
  
  
  if (identical(env, emptyenv())) {
    return(env_list)
  }
  
  if (exists(name, env, inherits = FALSE)) {
    env_list <- c(env_list, env)
  }
  
  env_list <- c(env_list, anywhere(name, parent.env(env)))
}

source("test-env-anywhere.R")
