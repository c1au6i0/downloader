# multydownload works

    Code
      retry_download(conn = to_download, max_attempts = 5, sleep_time = 1, dest_path = tempdir())
    Message <cliMessage>
      -- Download  attempt 1 ---------------------------------------------------------
      i file already present
      
      x File 'Business-employment-data-september-2022-quarter-csv.zip' not downloaded.
      
      -- Download  attempt 1 ---------------------------------------------------------
      i file already present
      
      x File 'business-financial-data-september-2022-quarter-csv.zip' not downloaded.
      
    Output
                                                                                                                                                                                       url_file
      1 https://www.stats.govt.nz/assets/Uploads/Business-employment-data/Business-employment-data-September-2022-quarter/Download-data/Business-employment-data-september-2022-quarter-csv.zip
      2            www.stats.govt.nz/assets/Uploads/Business-financial-data/Business-financial-data-September-2022-quarter/Download-data/business-financial-data-september-2022-quarter-csv.zip
                                                           file    out
      1 Business-employment-data-september-2022-quarter-csv.zip failed
      2  business-financial-data-september-2022-quarter-csv.zip failed
                     details retry
      1 file already present     5
      2 file already present     5

