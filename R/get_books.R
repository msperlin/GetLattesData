get_books <- function(my_xml) {

  book_nodes <- xml2::xml_find_all(my_xml, ".//LIVRO-PUBLICADO-OU-ORGANIZADO")

  if (length(book_nodes) == 0) return(tibble::tibble())

  # parse each book individually so that missing sub-nodes do not misalign rows
  books <- dplyr::bind_rows(
    lapply(book_nodes, function(node) {
      fetch_node_df(
        node,
        c(".//DADOS-BASICOS-DO-LIVRO", ".//DETALHAMENTO-DO-LIVRO")
      )
    })
  )

  return(books)
}
