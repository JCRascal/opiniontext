test_that("SCOTUS_opinions_2019_session has valid values for type", {
  known_types <- tibble::tibble(type = c("Syllabus", "Majority", "Dissenting", "Concurring", "Statement"))
  tester <- SCOTUS_opinions_2019_session %>%
    dplyr::anti_join(known_types, by = "type")

  expect_equal(nrow(tester), 0)
})

test_that("SCOTUS_opinions_2019_session has valid values for author", {
  known <- read.csv(system.file("extdata", "authors.csv", package = "opiniontext"))

  test <- dplyr::anti_join(SCOTUS_opinions_2019_session, known, by = "author")

  expect_equal(nrow(test), 0)
})

test_that("SCOTUS_opinions_2019_session.csv has valid values for author and type", {
  known_auth <- read.csv(system.file("extdata", "authors.csv", package = "opiniontext"))
  known_types <- tibble::tibble(type = c("Syllabus", "Majority", "Dissenting", "Concurring", "Statement"))

  test_data <- read.csv(system.file("data-raw", "SCOTUS_opinions_2019_session.csv", package = "opiniontext")) %>%
    dplyr::select(2:5)

  test_auth <- dplyr::anti_join(test_data, known_auth, by = "author")
  test_type <- dplyr::anti_join(test_data, known_types, by = "type")

  expect_equal(nrow(test_auth), 0)
  expect_equal(nrow(test_type), 0)
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
