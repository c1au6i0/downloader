
to_download <- c(
  "https://www.stats.govt.nz/assets/Uploads/Business-employment-data/Business-employment-data-September-2022-quarter/Download-data/Business-employment-data-september-2022-quarter-csv.zip",
  "www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2022-quarter/Download-data/business-financial-data-september-2022-quarter-csv.zip"
)
logs_download <- retry_download(conn  = to_download,
                                max_attempts = 5,
                                sleep_time = 1,
                                dest_path = tempdir()
)


test_that("multydownload works", {
  expect_snapshot(retry_download(conn  = to_download,
                                 max_attempts = 5,
                                 sleep_time = 1,
                                 dest_path = tempdir()
  ))
})
