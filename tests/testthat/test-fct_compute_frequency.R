testthat::test_that("frequency computation works", {
  GermlineVarDB::compute_frequency(db_path = NULL, prefix = NULL,attribute = "constant_group")
  #dbDisconnect()
  #GermlineVarDB::compute_frequency(db_path = "~/testdb/", prefix = "splitted_onco_twofiles",attribute = "run")
})
  
