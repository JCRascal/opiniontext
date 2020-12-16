test_that("pdf_list correctly identifies target urls of all pdfs on a webpage", {
  tester <- pdf_list("https://www.supremecourt.gov/opinions/slipopinion/19")

  known <- c("https://www.supremecourt.gov/opinions/19pdf/20a8_970e.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-1107_o759.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-9526_9okb.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-715_febh.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-635_o7jq.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-267_1an2.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-431_5i36.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-518_6k47.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-631_2d93.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-465_i425.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-46_8n59.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1195_g314.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-177_b97c.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1323_c07d.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-7_new_bq7d.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-161_g314.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1501_8n5a.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-587_5ifl.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-9674_2dp3.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-1618_hfci.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1584_igdj.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-8369_3dq3.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1432_e2pg.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-1712_0971.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-6943_k5fm.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1334_new1_1a7d.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1048_8ok0.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-1268_c07d.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1086_new_5ifl.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1059_e2p3.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-67_n6io.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-280_ba7d.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1023_m64o.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1150_new_d18e.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-725_f2bh.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1233_5he6.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-260_jifl.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-5924_n6io.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-916_new_g3bi.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-1498_8mjp.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19a1016_o759.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-882_3ebh.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-556_e1pf.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-565_3d93.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-5421_o7jq.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1171_4425.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-6135_j4ek.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-877_dc8f.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-776_8759.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-834_k53l.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1116_h3cj.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-7739_9q7h.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-6662_c0ne.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1109_5i36.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1269_h3dj.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/17-1678_m6io.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-935_new_fd9g.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-921_2cp3.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-1165_4gcj.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-938_l6gn.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-801_o758.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/18-328_pm02.pdf",
             "https://www.supremecourt.gov/opinions/19pdf/19-122_k536.pdf")

  expect_identical(tester, known)
})

test_that("prep_text_h1 correctly groups the pages of a slip opinion pdf", {
  known <-  c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                      2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                      4, 4, 5, 5, 5, 5, 5, 5, 5)

  tester <- prep_text_h1(pdftools::pdf_text(system.file("extdata", "19-631_2d93.pdf", package = "opiniontext")))

  expect_identical(tester$group_no, known)

})

test_that("prep_text returns a list with length equal to the number of groups", {
  tester <- prep_text(pdftools::pdf_text(system.file("extdata", "19-631_2d93.pdf", package = "opiniontext")))

  expect_equal(length(tester), 5)
})

test_that("opinion_author correctly identifies the author of each pdf sub-document", {
  test_in <- pdftools::pdf_text(system.file("extdata", "19-631_2d93.pdf", package = "opiniontext"))

  known <- c("Syllabus", "Kavanaugh", "Sotomayor", "Breyer", "Gorsuch")

  expect_identical(opinion_author(test_in), known)

  test_in <- pdftools::pdf_text(system.file("extdata", "18-938_l6gn.pdf", package = "opiniontext"))

  known <- c("Syllabus", "Ginsburg")

  expect_identical(opinion_author(test_in), known)

  test_in <- pdftools::pdf_text(system.file("extdata", "18-725_f2bh.pdf", package = "opiniontext"))

  known <- c("Syllabus", "Kavanaugh", "Sotomayor")

  expect_identical(opinion_author(test_in), known)
})

test_that("opinion_type correctly identifies the type of each pdf sub-document", {
  test_in <- pdftools::pdf_text(system.file("extdata", "19-631_2d93.pdf", package = "opiniontext"))

  known <- c("Syllabus", "Majority", "Concurring", "Concurring", "Concurring")

  expect_identical(opinion_type(test_in), known)

  test_in <- pdftools::pdf_text(system.file("extdata", "18-938_l6gn.pdf", package = "opiniontext"))

  known <- c("Syllabus", "Majority")

  expect_identical(opinion_type(test_in), known)

  test_in <- pdftools::pdf_text(system.file("extdata", "18-725_f2bh.pdf", package = "opiniontext"))

  known <- c("Syllabus", "Majority", "Dissenting")

  expect_identical(opinion_type(test_in), known)
})
