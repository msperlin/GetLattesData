get_awards <- function(my_xml) {

  awards <- fetch_df(my_xml, ".//PREMIO-TITULO")

  return(awards)
}
