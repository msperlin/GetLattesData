get_superv <- function(my_xml) {

  # parse each type of supervision separately, then combine them. For each type,
  # parse the parent node individually so that missing basic/detail sub-nodes do
  # not misalign rows (see get_books for the same pattern).
  fetch_superv <- function(parent_tag, basic_tag, detail_tag, type_label) {

    nodes <- xml2::xml_find_all(my_xml, paste0(".//", parent_tag))

    if (length(nodes) == 0) return(tibble::tibble())

    df <- dplyr::bind_rows(
      lapply(nodes, function(node) {
        fetch_node_df(
          node,
          c(paste0(".//", basic_tag), paste0(".//", detail_tag))
        )
      })
    )

    df$supervision_type <- type_label

    return(df)
  }

  superv_others <- fetch_superv(
    parent_tag = "OUTRAS-ORIENTACOES-CONCLUIDAS",
    basic_tag = "DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS",
    detail_tag = "DETALHAMENTO-DE-OUTRAS-ORIENTACOES-CONCLUIDAS",
    type_label = "others"
  )

  superv_msc <- fetch_superv(
    parent_tag = "ORIENTACOES-CONCLUIDAS-PARA-MESTRADO",
    basic_tag = "DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO",
    detail_tag = "DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO",
    type_label = "msc"
  )

  superv_phd <- fetch_superv(
    parent_tag = "ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO",
    basic_tag = "DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO",
    detail_tag = "DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO",
    type_label = "phd"
  )

  superv_all <- dplyr::bind_rows(
    superv_others,
    superv_msc,
    superv_phd
  )

  return(superv_all)
}
