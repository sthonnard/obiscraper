#' obiscraper: Query the Oracle Business Intelligence server by web scraping the analytics portal.
#'
#' Submit queries to Oracle Business Intelligence Enterprise Edition (OBIEE) by web scraping the analytics portal with RSelenium.
#' The portal login form is handled, even when behind an extranet.
#' Then queries to the presentation or physical layer are submitted by using
#' the OBI GO URL (eg saw.dll?Go&SQL=select+Region,Euro+from+SupplierSalesEurope) or by creating a narrative view.
#' Queries result returned as R Data Frame.
#'
#' Prerequisite:\cr
#' -Java
#' -CRAN packages rJava, RSelenium\cr
#' -Firefox
#'
#' Bugs report:\cr
#'  \url{https://github.com/sthonnard/obiscraper}
#'
#'
#' @section obiscraper functions:
#' \strong{connectobi(path_to_firefox, username, password, obilink)}\cr
#' Open Firefox, browse the OBI web portal and provide your username in password to the formular so you are logged in.
#'
#' \strong{submit_query(query)}\cr
#' Submit a logical query to the OBI web portal by using the GO URL and return the result as a Data Frame.\cr
#'
#' \strong{submit_physical_sql(sql_query, connection_pool )}\cr
#' Submit a physical query to the provided connection pool and return the result as a Data Frame.\cr
#'
#' \strong{disconnectobi()}\cr
#' Disconnect from OBI and close the web browser.\cr
#'
#' \strong{get_rs_client()}\cr
#' Return the RSelenium driver client, enabling more control and debugging.
#'
#'
#' @docType package
#' @name obiscraper
#'

source("./R/f_obiscraper.R")
#' connectobi
#'
#' Open Firefox, browse the OBI web portal and fill the login form with your username and password so you are logged in.
#'
#' @param path_to_firefox Optional path to Firefox in case it is not available in the default path. Default NA.
#' @param username  Username to access the OBI server. If not provided, it will be prompted.
#' @param password User password to access the OBI server. If not provided, it will be prompted.
#' @param obilink  Mandatory link to the analytics web portal. Eg "https://mycompany.int/analytics/".
#' @param extranet_elem_id  Optional. Id of the HTML element for user and password, in case you use an extranet before analytics server. Example: list("user"="usr", "password"="passwd")
#'
#' @return None.
#' @export
#'
#' @examples
#' # Connect without providing the password to the function. Password will be prompted and hidden (using package getPass)
#' # connectobi(username="myusername", obilink="https://mycompany.int/analytics/")
#'
#' # Connect and provide the password to the function. Nothing will be prompted.
#' # Useful for batch mode.
#' # connectobi(username="myusername", password="myfancypassword", obilink="https://mycompany.int/analytics/")
connectobi <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA,
                       extranet_elem_id = NA, debuglevel = 0, errstop = TRUE)
{
  if (!is.na(obilink))
  {
    init(path_to_firefox,username, password, obilink, debuglevel)
    init_extranet(extranet_elem_id)
    connect_obi()
    login_obi(errstop)
  }

}

#' disconnectobi
#'
#' Close the session, RSelenium server, and Firefox browser
#'
#' @return None.
#' @export
#'
#' @examples
#' disconnectobi()
disconnectobi <- function()
{
  tryCatch(
    {
      obiescraper.globals$rd$closeall()
      obiescraper.globals$rd$closeServer()
    },warning,error=function(e){message(e)
      }
  )

}


#' submit_query
#'
#' Submit a query to the Oracle Business Intelligence Presentaion Layer and get the result as a Data Frame.
#'
#' This procedure will use the Oracle Business Intelligence GO URL facility (eg saw.dll?Go&SQL=select+Region,Euro+from+SupplierSalesEurope)
#' in order to download the result as a flat file in your temp folder, which will then be parsed as a Data Frame in your R session.
#'
#' @param query The query that will be sent to Oracle Business Intelligence Presentation Layer.
#' @importFrom magrittr %>%
#' @return Data Frame containing the query result.
#' @export
#'
#' @examples
#' # submit_query('SELECT "MyModel"."Flights"."Departure Airport" s_1 FROM "MyModel" ORDER BY 1 ASC NULLS LAST FETCH FIRST 10000000 ROWS ONL')
submit_query <- function(query = 'SELECT ... FROM "..." ORDER BY 1 ASC NULLS LAST FETCH FIRST 10000000 ROWS ONLY')
{
  rd <- get_rs_client()
  go_url <- paste0(obiescraper.globals$analytics_url,'saw.dll?Go&SQL=',URLencode(query),'&Format=CSV')

  curr_time <- Sys.time()

  log(paste("GO URL:",go_url))

  reconnect() # Ensure user is connected before submitting the query

  #.obiescraper.globals$rd$navigate(go_url)
  download_file(go_url)
  if (!is_connected()) # Reconnect in case user is sent back to the login page after navigation to go URL
  {
    reconnect()
    download_file(go_url)
  }

  # Wait for the file to be downloaded
  # Fetch last CSV file written
  attempts <- 0
  while (attempts <= 100)
  {
    # Ensure the query submitted by the user is well formed
    ErrorMessage <- NULL
    tryCatch({
      silence({ErrorMessage <- obiescraper.globals$rd$findElement(using = 'class', value = 'ErrorMessage')})
    }, warning,error = function(e){})

    if (!is.null(ErrorMessage))
    {
      stop(ErrorMessage$getElementText()[[1]])
    }

    files <- data.frame(filepath = list.files(path = obiescraper.globals$tempdir, pattern = "*.CSV", full.names = TRUE,recursive = TRUE, include.dirs = TRUE))
    #dirs <- dirname(files)

    files %>% dplyr::rowwise() %>% dplyr::mutate(edit_date = file.mtime(as.character(filepath))) %>%
    dplyr::filter(edit_date > curr_time & grepl("Ana",filepath) > 0) -> last_csv

    # File download is not over:
    last_csv %>% dplyr::filter(grepl(".part", filepath) > 0) -> part

    if (nrow(last_csv) == 0 | nrow(part) > 0)
    {
      Sys.sleep(2) # Wait for the file to be completed
      attempts <- attempts + 1
    }
    else
    {# CSV file completed
      csv <- utils::read.csv(as.character(last_csv$filepath))
      file.remove(as.character(last_csv$filepath))
      return(csv)
    }
  }

  stop("Unable to retrieve the result")

}

#' get_rs_client
#'
#' Return the RSelenium client, for debugging
#'
#' @return RSelenium::rsDriver[['client']]
#' @export
#'
#' @examples
#' get_rs_client()
get_rs_client <- function()
{
  if (!obiescraper.globals$rd_created)
  {
    stop("Run connectobi first!")
  }
  return(obiescraper.globals$rd)
}


#' submit_physical_sql
#'
#' Sumbit SQL statement to the OBI physical layer and return the result as a Data Frame. Experimental!
#' @param psql SQL statement that will be submitted to connection pool pconnectionpool of the OBI physical layer.
#' @param pconnectionpool  Name of the connetion pool.
#' @param clean_final_result  Clean final result (TRUE/FALSE). Will removed ",00" from integers.
#'
#' @return Data Frame containing the query result.
#' @export
#'
#' @examples
#' run_physical_sql(psql="select distinct icao_code from dwh.airport", pconnectionpool="my_dwh")
submit_physical_sql <- function(psql="select distinct icao_code from dwh.airport", pconnectionpool="my_dwh", clean_final_result=TRUE)
{
  corr_sql <- stringr::str_remove_all(psql,"\n")
  expected_records <- run_physical_query(paste0("select count(*) as NB_REC from (", corr_sql, ")"), expected_nrow = 1, pconnectionpool)
  expected_records <- as.double(stringr::str_replace(expected_records$NB_REC,",","."))
  print(paste("Expecting",expected_records,"observations"))
  res <- run_physical_query(corr_sql, expected_nrow = expected_records, pconnectionpool)

  if (clean_final_result)
  {
    res %>% dplyr::rowwise() %>%
            dplyr::mutate_all(clean_zeros) -> res
  }
  return(res)
}


