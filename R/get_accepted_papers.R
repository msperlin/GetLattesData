get_accepted_papers <- function(my_xml) {

  node_papers <- xml2::xml_find_all(my_xml, ".//ARTIGO-ACEITO-PARA-PUBLICACAO")

  accepted_papers <- tibble::tibble()
  for (i_node  in node_papers) {

    temp_attrs <- xml2::xml_find_all(i_node, "DADOS-BASICOS-DO-ARTIGO")[[1]] |>
      xml2::xml_attrs() |>
      t() |>
      tibble::as_tibble() |>
      janitor::clean_names()

    nodes_detalhes <- xml2::xml_find_all(i_node, "DETALHAMENTO-DO-ARTIGO")[[1]] |>
      xml2::xml_attrs() |>
      t() |>
      tibble::as_tibble() |>
      janitor::clean_names()

    accepted_papers <- dplyr::bind_rows(
      accepted_papers,
      dplyr::bind_cols(
        temp_attrs,
        nodes_detalhes
      )
    )

  }

  return(accepted_papers)
}
