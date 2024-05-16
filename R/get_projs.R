get_projs <- function(my_xml) {


  projs <- fetch_df(my_xml, ".//PROJETO-DE-PESQUISA") |>
    dplyr::select(-dplyr::contains("descricao_do_projeto"))

  return(projs)
}
