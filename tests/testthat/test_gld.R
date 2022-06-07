library(testthat)
library(GetLattesData)

test_that(desc = 'Test of main function',{

  f_1 <- system.file("extdata/3262699324398819.zip", package = 'GetLattesData')
  f_2 <- system.file("extdata/8373564643000623.zip", package = 'GetLattesData')

  l_lattes <- gld_get_lattes_data_from_zip(c(f_1, f_2))

  expect_true(is.list(l_lattes))
  expect_true(length(l_lattes) > 0)

})

