get_books <- function(my_xml) {

  book_1 <- fetch_df(my_xml, ".//LIVRO-PUBLICADO-OU-ORGANIZADO//DADOS-BASICOS-DO-LIVRO")
  book_2 <- fetch_df(my_xml, ".//LIVRO-PUBLICADO-OU-ORGANIZADO//DETALHAMENTO-DO-LIVRO")

  books <- dplyr::bind_cols(book_1, book_2)

  return(books)
}
