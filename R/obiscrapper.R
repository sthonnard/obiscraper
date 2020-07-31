connectobi <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA)
{
  .init(path_to_firefox,username, password, obilink)
  .connect_obi()
}

disconnectobi <- function()
{
  .obiescrapper.globals$rd$closeall()
  .obiescrapper.globals$rd$closeServer()
}


# Submit a query to obi and download the result
query_obi <- function(query = 'SELECT
                                  ...
                                  FROM "..."
                                  ORDER BY 3 ASC NULLS LAST, 4 ASC NULLS LAST, 2 ASC NULLS LAST
                                  FETCH FIRST 10000000 ROWS ONLY')
{
  go_url <- paste0(.obiescrapper.globals$analytics_url,'saw.dll?Go&SQL=',URLencode(query),'&Format=CSV')


  curr_time <- Sys.time()

  if (!.is_connected())
  { # Reconnect and submit the query again, in case user was disconnected
    .connect_obi()
    .obiescrapper.globals$rd$navigate(go_url)
  }

  .obiescrapper.globals$rd$navigate(go_url)


  # Wait for the file to be downloaded
  # Fetch last CSV file written
  attempts=0
  while(attempts<=100)
  {
    # Ensure the query submitted by the user is well formed
    ErrorMessage <- NULL
    tryCatch({
      .silence({ErrorMessage <- .obiescrapper.globals$rd$findElement(using = 'class', value = 'ErrorMessage')})
    }, warning,error=function(e){})

    if (!is.null(ErrorMessage))
    {
      stop(ErrorMessage$getElementText()[[1]])
    }

    files <- data.frame(filepath=list.files(path=paste0(path.expand('~'),"/tmp"),pattern="*.CSV",full.names = TRUE,recursive = TRUE, include.dirs = TRUE))
    #dirs <- dirname(files)

    files %>% dplyr::rowwise() %>% dplyr::mutate(edit_date=file.mtime(as.character( filepath))) %>%
      dplyr::filter(edit_date > curr_time & grepl("Ana",filepath)>0) -> last_csv

    # File download is not over:
    last_csv %>% dplyr::filter(grepl(".part",filepath) > 0) -> part

    if (nrow(last_csv) == 0 | nrow(part) > 0)
    {
      Sys.sleep(2)
      attempts <- attempts + 1
    }
    else
    {
      csv <- utils::read.csv(as.character(last_csv$filepath))
      file.remove(as.character(last_csv$filepath))
      return(csv)
    }
  }

  stop("Unable to retrieve the result")

}

get_obi_client <- function()
{
  return(.obiescrapper.globals$rd)
}
