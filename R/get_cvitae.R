fetch_df <- function(this_xml, xpath) {
  this_nodes <- xml2::xml_find_all(this_xml, xpath)

  if (length(this_nodes) == 0) {
    df_out <- tibble::tibble()
  } else {
    df_out <- this_nodes |>
      purrr::map_df(
        .f = function(x) tibble::as_tibble(janitor::clean_names(t(xml2::xml_attrs(x))))
      )
  }

  return(df_out)
}

get_cvitae <- function(my_xml) {
  # cvitae ----


  cvitae <- fetch_df(my_xml, ".//DADOS-GERAIS")
  extra_info <- fetch_df(my_xml, "//CURRICULO-VITAE")
  areas <- fetch_df(my_xml, ".//AREA-DE-ATUACAO")
  main_area <- areas[1, ] # only keep first area

  cvitae <- cvitae |>
    dplyr::bind_cols(main_area) |>
    dplyr::bind_cols(extra_info)

  return(cvitae)
}
