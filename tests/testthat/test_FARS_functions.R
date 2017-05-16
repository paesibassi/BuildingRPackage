test_that("Fars function tests", {

  # make_filename()
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")

  # fars_read()
  datafile <- system.file("extdata", "accident_2013.csv.bz2", package = "buildRpackage")

  expect_equal(nrow(fars_read(datafile)), 30202) # check number of rows
  expect_equal(sum(fars_read(datafile)$MONTH), 203624) # check sum of columns
  expect_equal(unique(fars_read(datafile)$YEAR), 2013) # check year

})
