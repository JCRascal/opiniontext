test_that("SCOTUS_opinions_2019_session has valid values for type", {
  known_types <- tibble::tibble(type = c("Syllabus", "Majority", "Dissenting", "Concurring"))
  tester <- SCOTUS_opinions_2019_session %>%
    dplyr::anti_join(known_types, by = "type")

  expect_equal(nrow(tester), 0)
})

test_that("SCOTUS_opinions_2019_session has valid values for author", {
  known <- read.csv(system.file("extdata", "authors.csv", package = "opiniontext"))

  test <- dplyr::anti_join(SCOTUS_opinions_2019_session, known, by = "author")

  expect_equal(nrow(test), 0)
})

test_that("quilt_pdf returns correct values for June Medical Services L. L. C. v. Russo", {
  test <- quilt_pdf(system.file("extdata", "18-1323_c07d.pdf", package = "opiniontext"), "June Medical Services L. L. C. v. Russo")

  known_auth <- c("Syllabus", "Breyer", "Roberts", "Thomas", "Alito", "Gorsuch", "Kavanaugh")
  known_type <- c("Syllabus", "Majority", "Concurring", "Dissenting", "Dissenting", "Dissenting", "Dissenting")

  expect_identical(test$author, known_auth)
  expect_identical(test$type, known_type)
})
