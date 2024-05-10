# global file set up for removing CRAN check messages
# "no visible binding for global variable"
# source: https://community.rstudio.com/t/how-to-solve-no-visible-binding-
#         for-global-variable-note/28887
my_globals <- c(
  "nome_completo"
)

utils::globalVariables(my_globals)
