get_employment <- function(my_xml) {

  all_prof <- xml2::xml_find_all(my_xml,
                                 ".//ATUACOES-PROFISSIONAIS//ATUACAO-PROFISSIONAL")

  at_prof <- tibble::tibble()
  for (i_node in all_prof) {

    temp_attrs <- i_node |>
      xml2::xml_attrs() |>
      t() |>
      tibble::as_tibble() |>
      janitor::clean_names()

    nodes_vinculos <- xml2::xml_find_all(i_node, ".//VINCULOS")

    for (i_vinc in nodes_vinculos) {

      temp_df <- xml2::xml_attrs(i_vinc) |>
        t() |>
        tibble::as_tibble() |>
        janitor::clean_names() |>
        dplyr::bind_cols(temp_attrs)

      at_prof <- dplyr::bind_rows(
        at_prof,
        temp_df
      )
    }

  }

  return(at_prof)
}
