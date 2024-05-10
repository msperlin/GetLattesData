library(testthat)
library(GetLattesData)

test_that(desc = 'Test of read_zip 2',{

  f_1 <- system.file("extdata/3262699324398819.zip", package = 'GetLattesData')
  f_2 <- system.file("extdata/8373564643000623.zip", package = 'GetLattesData')

  for (i_f in c(f_1, f_2)) {
    l_out <- gld_read_zip2(i_f)

    expect_true(is.list(l_out))
    expect_true(length(l_out) > 0)
  }

})

