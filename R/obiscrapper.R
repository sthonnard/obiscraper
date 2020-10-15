#' obiscrapper: A Tool For Submitting Queries to Oracle Business Intelligence by webscrapping the analytics server
#'
#' Execute OBI queries by web scrapping the analytics server with RSelenium. The analytics login form is handled,
#' even when behind an extranet. Then queries are submitted by using
#' the OBI GO URL (saw.dll?Go&SQL=select+Region,Euro+from+SupplierSalesEurope).
#' Queries result returned as R Data Frame.
#'
#' Prerequisite:\cr
#' -CRAN package RSelenium\cr
#' -Firefox
#'
#' Bugs report:\cr
#'  \url{https://github.com/sthonnard/obiscrapper}
#'
#'
#' @section obiscrapper functions:
#' \strong{connectobi(path_to_firefox, username, password, obilink)}\cr
#' Open Firefox, browse the OBI web portal and provide your username in passward to the formular so you are logged in.
#'
#' \strong{submit_query(query)}\cr
#' Submit a query to the OBI web portal by using the GO URL and return the result as a Data Frame.\cr
#'
#' \strong{disconnectobi()}\cr
#' Disconnect from OBI and close the web browser.\cr
#'
#' \strong{get_obi_client()}\cr
#' Return the RSelenium driver client, for debugging.
#'
#'
#' @docType package
#' @name obiscrapper
#'

source("./R/f_obiscrapper.R")
#' connectobi
#'
#' Open Firefox, browse the OBI web portal and provide your username and password to the formular so you are logged in.
#'
#' @param path_to_firefox Optional path to Firefox in case it is not available in your path. Default NA.
#' @param username  Username to access the OBI server. If not provided, it will be prompted.
#' @param password User password to access the OBI server. If not provided, it will be prompted.
#' @param obilink  Mandatory link to the analytics web portal. Eg https://mycompany.int/analytics/".
#' @param extranet_elem_id  Optional. Id of the HTML element for user and password, in case you use an extranet before analytics server. Example: list("user"="rad_usr", "password"="rad_pw")
#'
#' @export
#'
#' @examples
#' # Connect without providing the password to the function. Password will be prompted and hidden (using package getPass)
#' connectobi(username="myusername", obilink="https://mycompany.int/analytics/")
#'
#' # Connect and provide the password to the function. Nothing will be prompted.
#' # Useful for batch mode.
#' connectobi(username="myusername", password="myfancypassword", obilink="https://mycompany.int/analytics/")
connectobi <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA,
                       extranet_elem_id = NA, debuglevel = 0)
{
  .init(path_to_firefox,username, password, obilink, debuglevel)
  .init_extranet(extranet_elem_id)
  .connect_obi()
  .login_obi()
}

#' disconnectobi
#'
#' Close the session and RSelenium server
#'
#' @export
#'
#' @examples
#' disconnectobi()
disconnectobi <- function()
{
  .obiescrapper.globals$rd$closeall()
  .obiescrapper.globals$rd$closeServer()
}


#' submit_query
#'
#' Submit a query to OBI and get the result as a dataframe
#'
#' @param query The query that will be sent to OBI
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' submit_query('SELECT "MyModel"."Flights"."Departure Airport" s_1
#' FROM "MyModel"
#' ORDER BY 1 ASC NULLS LAST
#' FETCH FIRST 10000000 ROWS ONL')
submit_query <- function(query = 'SELECT ... FROM "..." ORDER BY 1 ASC NULLS LAST FETCH FIRST 10000000 ROWS ONLY')
{
  go_url <- paste0(.obiescrapper.globals$analytics_url,'saw.dll?Go&SQL=',URLencode(query),'&Format=CSV')

  curr_time <- Sys.time()

  .log(paste("GO URL:",go_url))

  .reconnect() # Ensure user is connected before sumbitting the query

  #.obiescrapper.globals$rd$navigate(go_url)
  .download_file(go_url)
  if (!.is_connected()) # Reconnect in case user is sent back to the login page after navigation to go URL
  {
    .reconnect()
    .download_file(go_url)
  }

  # Wait for the file to be downloaded
  # Fetch last CSV file written
  attempts <- 0
  while (attempts <= 100)
  {
    # Ensure the query submitted by the user is well formed
    ErrorMessage <- NULL
    tryCatch({
      .silence({ErrorMessage <- .obiescrapper.globals$rd$findElement(using = 'class', value = 'ErrorMessage')})
    }, warning,error = function(e){})

    if (!is.null(ErrorMessage))
    {
      stop(ErrorMessage$getElementText()[[1]])
    }

    files <- data.frame(filepath = list.files(path = .obiescrapper.globals$tempdir, pattern = "*.CSV", full.names = TRUE,recursive = TRUE, include.dirs = TRUE))
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

#' get_obi_client
#'
#' Return the RSelenium client, for debugging
#'
#' @export
#'
#' @examples
#' get_obi_client()
get_obi_client <- function()
{
  return(.obiescrapper.globals$rd)
}
