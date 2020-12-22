test_that("SCOTUS_opinions_2019_session has valid values for type", {
  tester <- SCOTUS_opinions_2019_session %>%
    dplyr::filter(type == "Juice")

  expect_equal(nrow(tester), 0)
})

