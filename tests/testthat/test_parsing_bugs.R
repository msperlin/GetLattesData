library(testthat)
library(GetLattesData)

test_that('get_books does not recycle detail rows when a book lacks DETALHAMENTO', {

  x <- xml2::read_xml('
  <CURRICULO-VITAE>
    <PRODUCAO-BIBLIOGRAFICA>
      <LIVROS-E-CAPITULOS>
        <LIVRO-PUBLICADO-OU-ORGANIZADO>
          <DADOS-BASICOS-DO-LIVRO TITULO-DO-LIVRO="Book A" ANO="2020"/>
          <DETALHAMENTO-DO-LIVRO ISBN="111"/>
        </LIVRO-PUBLICADO-OU-ORGANIZADO>
        <LIVRO-PUBLICADO-OU-ORGANIZADO>
          <DADOS-BASICOS-DO-LIVRO TITULO-DO-LIVRO="Book B" ANO="2021"/>
        </LIVRO-PUBLICADO-OU-ORGANIZADO>
      </LIVROS-E-CAPITULOS>
    </PRODUCAO-BIBLIOGRAFICA>
  </CURRICULO-VITAE>')

  books <- GetLattesData:::get_books(x)

  expect_equal(nrow(books), 2)
  expect_equal(books$titulo_do_livro, c('Book A', 'Book B'))
  expect_equal(books$isbn, c('111', NA_character_))
})

test_that('get_published_papers keeps rows aligned when a paper lacks DETALHAMENTO', {

  x <- xml2::read_xml('
  <CURRICULO-VITAE>
    <PRODUCAO-BIBLIOGRAFICA>
      <ARTIGO-PUBLICADO>
        <DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="Paper A" ANO-DO-ARTIGO="2020"/>
        <DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="Journal A"/>
      </ARTIGO-PUBLICADO>
      <ARTIGO-PUBLICADO>
        <DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="Paper B" ANO-DO-ARTIGO="2021"/>
      </ARTIGO-PUBLICADO>
    </PRODUCAO-BIBLIOGRAFICA>
  </CURRICULO-VITAE>')

  papers <- GetLattesData:::get_published_papers(x)

  expect_equal(nrow(papers), 2)
  expect_equal(papers$titulo_do_artigo, c('Paper A', 'Paper B'))
  expect_equal(papers$titulo_do_periodico_ou_revista, c('Journal A', NA_character_))
})

test_that('get_superv keeps rows aligned when a supervision lacks DETALHAMENTO', {

  x <- xml2::read_xml('
  <CURRICULO-VITAE>
    <OUTRA-PRODUCAO>
      <ORIENTACOES-CONCLUIDAS>
        <ORIENTACOES-CONCLUIDAS-PARA-MESTRADO>
          <DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO TITULO="S A" ANO="2020"/>
          <DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO NOME-DO-ORIENTADO="Student A"/>
        </ORIENTACOES-CONCLUIDAS-PARA-MESTRADO>
        <ORIENTACOES-CONCLUIDAS-PARA-MESTRADO>
          <DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO TITULO="S B" ANO="2021"/>
        </ORIENTACOES-CONCLUIDAS-PARA-MESTRADO>
      </ORIENTACOES-CONCLUIDAS>
    </OUTRA-PRODUCAO>
  </CURRICULO-VITAE>')

  superv <- GetLattesData:::get_superv(x)

  expect_equal(nrow(superv), 2)
  expect_equal(superv$titulo, c('S A', 'S B'))
  expect_equal(superv$nome_do_orientado, c('Student A', NA_character_))
  expect_equal(unique(superv$supervision_type), 'msc')
})

test_that('identifiers are not coerced to numeric by gld_read_zip2', {

  f <- system.file('extdata/3262699324398819.zip', package = 'GetLattesData')

  l_out <- suppressMessages(gld_read_zip2(f))

  expect_type(l_out$info$numero_identificador, 'character')
  expect_type(l_out$supervisions$numero_id_orientado, 'character')
  expect_type(l_out$awards$ano_da_premiacao, 'double')
})
