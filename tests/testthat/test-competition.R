test_that("TDM", {
  expect_equal(TDM(30,c(30,30)),0)
  expect_equal(TDM(52,c(16,22,30,18)),0.5865385,tolerance=1e-6)
  expect_true(is.na(TDM(30,numeric(0))))
})

test_that("SCI", {
  skip_if_not_installed("geometry")
  expect_equal(SCI(c(0,5,5,5),c(0,0,120,240),rep(20,4)),1)
  expect_equal(SCI(c(0,14,6.2,9.8,11.3),c(0,259,45,130,310),c(52,16,22,30,18)),4.861908,tolerance=1e-6)
  expect_true(is.na(SCI(14,259,16)))
  expect_true(is.na(SCI(c(0,5,10),c(0,90,90),c(1,2,3))))
})
