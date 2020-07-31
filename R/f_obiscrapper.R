.obiescrapper.globals <- new.env()

.obiescrapper.globals$username <- NA
.obiescrapper.globals$password <- NA
.obiescrapper.globals$firefoxpath <- "/Applications/Firefox.app"

.obiescrapper.globals$firefox_config_prefs <- NA # prefs
.obiescrapper.globals$fprof <- NA # profile

.obiescrapper.globals$rs <- NA #driver
.obiescrapper.globals$rd <- NA # client

.obiescrapper.globals$analytics_url <- NA # Link to obi

# Initialize the path to firefox and profile
.init_firefoxpath <- function(path_to_firefox)
{
  if (is.na(path_to_firefox))
  {
    if (.Platform$OS.type == "windows")
    { # Windows
      .obiescrapper.globals$firefoxpath <- "%APPDATA%\\Mozilla\\Firefox\\Firefox.exe"
    }
    else
    {
      if (as.character(Sys.info()[1])=="Darwin")
      { # macOs
        .obiescrapper.globals$firefoxpath <- "/Applications/Firefox.app"
      }
      else
      { # Linux
        .obiescrapper.globals$firefoxpath <- "firefox"
      }
    }
  }
  else
  {
    .obiescrapper.globals$firefoxpath <- path_to_firefox
  }

  if (!file.exists(.obiescrapper.globals$firefoxpath ))
  {
    stop(paste(.obiescrapper.globals$firefoxpath,"does not exist!"))
  }

  .obiescrapper.globals$firefox_config_prefs <- list(
    browser.download.downloadDir="/tmp/",
    browser.download.dir="/tmp/",
    browser.download.defaultFolder="/tmp/",
    browser.download.folderList=2L, # Custom directory
    browser.download.manager.showWhenStarting = FALSE,
    browser.helperApps.neverAsk.openFile = "text/csv",
    browser.helperApps.neverAsk.saveToDisk = "text/csv",
    browser.helperApps.alwaysAsk.force = FALSE,
    browser.download.manager.showAlertOnComplete = FALSE,
    browser.download.manager.closeWhenDone = FALSE )

  .obiescrapper.globals$fprof <- makeFirefoxProfile(.obiescrapper.globals$firefox_config_prefs)

}

# Initialize credentials to OBI
.init_credential <- function(username, password)
{
  if (is.na(username))
  {
    .obiescrapper.globals$username <- readline(prompt="Username? ")
  }
  else
  {
    .obiescrapper.globals$username <- username
  }

  if (is.na(password))
  {
    .obiescrapper.globals$password <- getPass("Password? ")
  }
  else
  {
    .obiescrapper.globals$password <- password
  }

}

.init_obilink <- function(obilink)
{
  if (is.na(obilink))
  {
    stop("Link to obi shall be provided! Eg https://mycompany.int/analytics/")
  }
  else
  {
    .obiescrapper.globals$analytics_url <- obilink
  }
}

# Initialize everything
.init <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA)
{
  .init_firefoxpath(path_to_firefox)
  .init_credential(username, password)
  .init_obilink(obilink)
}

# Silence the message returned by RSelenium
.silence <- function(x)
{
  suppressWarnings(suppressMessages(suppressPackageStartupMessages(x)))
}



# Check if an element exists by id/class
.is_element_exists <- function(elem, using = "id")
{
  el <- NULL
  tryCatch({
    .silence({el <- .obiescrapper.globals$rd$findElement(using = using, value = elem)})
    if(is.null(el))
    {
      return(FALSE)
    }else
    {
      return(TRUE)
    }
  }, warning,error=function(e){return(FALSE)})
}

.is_connected <- function()
{ # If element logout exists on the page, it means the user is connected to obiee
  return(.is_element_exists(elem="logout", using="id"))
}
.is_signinin <- function()
{ # If element signingin exists on the page, it means obiee is connecting
  return(.is_element_exists(elem="signingin", using="id"))
}

.is_browser_openned <- function()
{ # Check if the Browser is already openned or not
  tryCatch({
    if (typeof(.obiescrapper.globals$rd)!=logical)
    {
      activeURL <- .obiescrapper.globals$rd$getCurrentUrl() # Test if it works
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  }, warning,error=function(e){return(FALSE)})
}

# Open firefox and go to the obi URL
.connect_obiee <- function()
{

  if (!.is_browser_openned())
  {
    # Path to Firefox
    ff64 <- .obiescrapper.globals$firefoxpath
    pr64 <- list(`moz:firefoxOptions` = list(binary = ff64), pageLoadStrategy = 'none')

    try_port <- 4564
    # Try 500 ports before failing
    while (try_port >= 4000)
    {

      tryCatch(
        {
          #.silence({rs <- rsDriver(browser = "firefox", port = as.integer(try_port), extraCapabilities = c(ff64,
          #                                                                                                pr64,
          #                                                                                                .obiescrapper.globals$fprof ))})
          .obiescrapper.globals$rs <- rsDriver(port = as.integer(try_port), browser = "firefox",
                         verbose = TRUE, check = TRUE, extraCapabilities = .obiescrapper.globals$fprof)
          break
        },
        warning,error=function(e){
          error <- e
        })
      try_port <- try_port - 1
    }
    if (try_port < 4000)
    {
      message("Cannot open port!")
      stop(e)
    }

    .obiescrapper.globals$rd <- .obiescrapper.globals$rs[['client']]
    #.obiescrapper.globals$rd$open() # Open the browser
  }

  # Navigate to obi URL
  .obiescrapper.globals$rd$navigate(.obiescrapper.globals$analytics_url)

}

# Login to obi
.login_obiee <- function()
{
  # Fill the login form of the extranet and login once available
  attempts=1
  while (!.is_connected())
  {
    tryCatch(
      {
        silence(
          {
            username_elem <- .obiescrapper.globals$rd$findElement(using = 'id', value = 'rad_usr')
            username_elem$sendKeysToElement(list(.obiescrapper.globals$username))
            Sys.sleep(1)
            pass_elem <- .obiescrapper.globals$rd$findElement(using = 'id', value = 'rad_pw')
            pass_elem$sendKeysToElement(list(.obiescrapper.globals$password))
            Sys.sleep(1)
          }
        )
      },warning,error=function(e){ # Error occurs when element not found
        tryCatch(
          {
            silence({logout <- .obiescrapper.globals$rd$findElement(using = 'id', value = 'logout')})
            # User already connected, break
            break
          }, warning,error=function(err){})
        error <- e
        Sys.sleep(3*attempts)
        attempts <- attempts + 1}
    )
    if (attempts==6)
    {
      stop(error)
    }
    tryCatch(
      {
        .silence({
          login_button <- .obiescrapper.globals$rd$findElement(using = 'class', value = "btn")
          login_button$clickElement()
          Sys.sleep(2)
        })
      }, warning,error=function(e){})

    if (.is_signinin())
    { # Siginin in, wait until user is actually connected to obiee
      while(!.is_connected()){Sys.sleep(1)}
    }
  }

}



# Submit a query to obi and download the result
query_obiee <- function(query = 'SELECT
                                  ...
                                  FROM "..."
                                  ORDER BY 3 ASC NULLS LAST, 4 ASC NULLS LAST, 2 ASC NULLS LAST
                                  FETCH FIRST 10000000 ROWS ONLY')
{
  go_url <- paste0(.obiescrapper.globals$analytics_url,'saw.dll?Go&SQL=',URLencode(query),'&Format=CSV')


  curr_time <- Sys.time()

  if (!.is_connected())
  { # Reconnect and submit the query again, in case user was disconnected
    .connect_obiee()
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

    files <- data.frame(filepath=list.files(path=paste0(path.expand('~'),"/Downloads"),pattern="*.CSV",full.names = TRUE,recursive = TRUE, include.dirs = TRUE))
    #dirs <- dirname(files)

    files %>% rowwise() %>% mutate(edit_date=file.mtime(as.character( filepath))) %>%
      filter(edit_date > curr_time & grepl("Ana",filepath)>0) -> last_csv

    # File download is not over:
    last_csv %>% filter(grepl(".part",filepath) > 0) -> part

    if (nrow(last_csv) == 0 | nrow(part) > 0)
    {
      Sys.sleep(2)
      attempts <- attempts + 1
    }
    else
    {
      csv <- read.csv(as.character(last_csv$filepath))
      file.remove(as.character(last_csv$filepath))
      return(csv)
    }
  }

  stop("Unable to retrieve the result")

}

.disconnectobi <- function()
{
  .obiescrapper.globals$rd$closeall()
  .obiescrapper.globals$rd$closeServer()
}
