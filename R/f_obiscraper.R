obiescraper.globals <- new.env()

obiescraper.globals$debuglevel <- 0

obiescraper.globals$username <- NA
obiescraper.globals$password <- NA
obiescraper.globals$firefoxpath <- "/Applications/Firefox.app"

obiescraper.globals$firefox_config_prefs <- NA # prefs
obiescraper.globals$fprof <- NA # profile

obiescraper.globals$rs <- NA #driver
obiescraper.globals$rd <- NA # client

obiescraper.globals$analytics_url <- NA # Link to obi

obiescraper.globals$tempdir <- paste0(tempdir(),'/obiscraper/')


obiescraper.globals$extranet_elem_id <- list()
obiescraper.globals$obi_form <- list("user" = "sawlogonuser", "password" = "sawlogonpwd", "invalid_user" = "An invalid User Name or Password was entered")
obiescraper.globals$active_form <- obiescraper.globals$obi_form

# Initialize the path to firefox and profile
init_firefoxpath <- function(path_to_firefox)
{
  # Needed on linux
  if (!dir.exists(obiescraper.globals$tempdir)){
    log(paste("Temp directory",obiescraper.globals$tempdir,"will be created!"),1)
    dir.create(obiescraper.globals$tempdir, recursive = TRUE)
    if (!dir.exists(obiescraper.globals$tempdir))
    {
      disconnectobi()
      stop("Cannot create temp directory!")
    }
  }

  if (is.na(path_to_firefox))
  {
    obiescraper.globals$firefox_config_prefs <- list(
      browser.download.downloadDir = obiescraper.globals$tempdir,
      browser.download.dir = obiescraper.globals$tempdir,
      browser.download.defaultFolder = obiescraper.globals$tempdir,
      browser.download.folderList = 2L, # Custom directory
      browser.download.manager.showWhenStarting = FALSE,
      browser.helperApps.neverAsk.openFile = "text/csv",
      browser.helperApps.neverAsk.saveToDisk = "text/csv",
      browser.helperApps.alwaysAsk.force = FALSE,
      browser.download.manager.showAlertOnComplete = FALSE,
      browser.download.manager.closeWhenDone = TRUE,
      browser.download.manager.useWindow = FALSE,
      browser.download.manager.focusWhenStarting = FALSE,
      pdfjs.disabled = TRUE
    )

    if (.Platform$OS.type == "windows")
    { # Windows
      log("Detected Windows. Assuming Firefox is in %APPDATA%\\Mozilla\\Firefox\\Firefox.exe",1)
      obiescraper.globals$firefoxpath <- "%APPDATA%\\Mozilla\\Firefox\\Firefox.exe"
    }
    else
    {
      if (as.character(Sys.info()[1]) == "Darwin")
      {# macOs
        log("Detected macos. Assuming Firefox is in /Applications/Firefox.app",1)
        obiescraper.globals$firefoxpath <- "/Applications/Firefox.app"
      }
      else
      {# Linux
        log("Detected Linux. Assuming Firefox is in /usr/bin/firefox",1)
        obiescraper.globals$firefoxpath <- "/usr/bin/firefox"
      }
    }
  }
  else
  {
    obiescraper.globals$firefoxpath <- path_to_firefox
  }

  if (!file.exists(obiescraper.globals$firefoxpath ))
  {
    stop(paste(obiescraper.globals$firefoxpath,"does not exist!"))
  }


  obiescraper.globals$fprof <- RSelenium::makeFirefoxProfile(obiescraper.globals$firefox_config_prefs)

}

# Initialize credentials to OBI
init_credential <- function(username, password)
{
  if (is.na(username))
  {
    obiescraper.globals$username <- base::readline(prompt = "Username? ")
  }
  else
  {
    obiescraper.globals$username <- username
  }

  if (is.na(password))
  {
    obiescraper.globals$password <- getPass::getPass(paste0("Password for ",obiescraper.globals$username, "?"))
  }
  else
  {
    obiescraper.globals$password <- password
  }

}

init_extranet <- function(extranet_elem_id)
{
  if (is.list(extranet_elem_id))
  {
    obiescraper.globals$extranet_elem_id <- extranet_elem_id
  }

}
init_obilink <- function(obilink)
{
  if (is.na(obilink))
  {
    stop("Link to obi shall be provided! Eg https://mycompany.int/analytics/")
  }
  else
  {
    obiescraper.globals$analytics_url <- obilink
  }
}

# Initialize everything
init <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA, debuglevel = NA)
{
  log("Start initialization", 1)
  obiescraper.globals$debuglevel <- debuglevel
  init_firefoxpath(path_to_firefox)
  init_credential(username, password)
  init_obilink(obilink)
  log("End initialization", 1)
}

# Silence the message returned by RSelenium, unless debug level is < 2
silence <- function(x)
{
  if (obiescraper.globals$debuglevel >= 2)
  {
    x
  }
  else
  {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(x)))
  }

}



# Check if an element exists by id/class
is_element_exists <- function(elem, using = "id")
{
  el <- NULL
  tryCatch({
    silence({el <- obiescraper.globals$rd$findElement(using = using, value = elem)})
    if (is.null(el))
    {
      return(FALSE)
    }else
    {
      return(TRUE)
    }
  }, warning,error = function(e){return(FALSE)})
}

is_connected <- function()
{ # If element logout exists on the page, it means the user is connected to obi
  return(is_element_exists(elem = "logout", using = "id"))
}
is_signinin <- function()
{ # If element signingin exists on the page, it means obi is connecting
  return(is_element_exists(elem = "signingin", using = "id"))
}

is_browser_openned <- function()
{ # Check if the Browser is already openned or not
  tryCatch({
    if (typeof(obiescraper.globals$rd) != logical)
    {
      activeURL <- obiescraper.globals$rd$getCurrentUrl() # Test if it works
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  }, warning,error = function(e){return(FALSE)})
}

reconnect <- function()
{
  if (!is_connected())
  { # Reconnect in case user was disconnected
    connect_obi()
  }
}
# Open firefox and go to the obi URL
connect_obi <- function()
{

  if (!is_browser_openned())
  {
    # Path to Firefox
    #ff64 <- obiescraper.globals$firefoxpath
    #pr64 <- list(`moz:firefoxOptions` = list(binary = ff64), pageLoadStrategy = 'none')

    try_port <- 4564
    e <- ""
    # Try 500 ports before failing
    while (try_port >= 4000)
    {

      tryCatch(
        {
          #silence({rs <- rsDriver(browser = "firefox", port = as.integer(try_port), extraCapabilities = c(ff64,
          #                                                                                                pr64,
          #                                                                                                obiescraper.globals$fprof ))})
          silence(obiescraper.globals$rs <- RSelenium::rsDriver(port = as.integer(try_port), browser = "firefox",
                         verbose = TRUE, check = TRUE, extraCapabilities = obiescraper.globals$fprof))
          break
        },
        warning=function(w){
          if (obiescraper.globals$debuglevel >= 2)
          {
            warning(w)
          }
        },error = function(e){
          if (obiescraper.globals$debuglevel >= 2)
          {
            message(e)
          }
          error <- e
        })
      try_port <- try_port - 1
    }
    if (try_port < 4000)
    {
      message("Cannot open port!")
      stop(e)
    }

    obiescraper.globals$rd <- obiescraper.globals$rs[['client']]
    #obiescraper.globals$rd$open() # Open the browser
  }

  # Navigate to obi URL
  obiescraper.globals$rd$navigate(obiescraper.globals$analytics_url)

}

# Login to obi
login_obi <- function()
{
  log("Login to obi", 1)
  # Fill the login form of the extranet and login once available
  attempts = 1
  while (!is_connected())
  {

    # Determine if logon form is an extranet or OBI form
    tryCatch({silence(obiescraper.globals$rd$findElement(using = 'id', value = obiescraper.globals$obi_form$user))
             log("Detected obi login form", 1)
             },
                        warning,error = function(e){ # Error occurs when element not found
                          log("Detected extranet form", 1)
                          if (length(obiescraper.globals$extranet_elem_id) == 0) {
                            disconnectobi()
                            stop("OBI login form not found. Seems you are connecting from an extranet portal. Please provide extranet_elem_id to connectobi.")
                          }
                          else
                          {
                            log("form info", 1)
                            if (!is.list(obiescraper.globals$extranet_elem_id))
                            {
                              disconnectobi()
                              stop("OBI login form not found. Seems you are connecting from an extranet portal. Please provide extranet_elem_id to connectobi.")
                            }
                            log(obiescraper.globals$extranet_elem_id, 1)
                            obiescraper.globals$active_form <- obiescraper.globals$extranet_elem_id
                          }
                        }
    )

    # Stop if incorrect username or password
    if (stringr::str_detect(obiescraper.globals$rd$getPageSource(), obiescraper.globals$active_form$invalid_user))
    {
      disconnectobi()
      stop("Incorrect username or password!")
    }

    log(paste("Login to obi attemp",attempts), 1)
    log(obiescraper.globals$active_form, 1)
    error <- ""
    tryCatch(
      {
        silence(
          {
            username_elem <- obiescraper.globals$rd$findElement(using = 'id', value = obiescraper.globals$active_form$user)
            username_elem$sendKeysToElement(list(""))
            username_elem$sendKeysToElement(list(obiescraper.globals$username))
            Sys.sleep(1)
            pass_elem <- obiescraper.globals$rd$findElement(using = 'id', value = obiescraper.globals$active_form$password)
            pass_elem$sendKeysToElement(list(""))
            pass_elem$sendKeysToElement(list(obiescraper.globals$password))
            Sys.sleep(1)
            log("Login, form has been filled", 1)
          }
        )
      },warning,error = function(e){ # Error occurs when element not found
        tryCatch(
          {
            log(paste("Login error ",e$message), 1)
            silence({logout <- obiescraper.globals$rd$findElement(using = 'id', value = 'logout')})
            # User already connected to obiee, break
            break
          }, warning,error = function(err){log(paste("Login error elem logout ",err$message), 1)})
        error <- e
        Sys.sleep(3*attempts)
        attempts <- attempts + 1}
    )
    if (attempts == 6)
    {
      stop(error)
    }
    tryCatch(
      {
        silence({
          #login_button <- obiescraper.globals$rd$findElement(using = 'class', value = "btn")
          #login_button$clickElement()
          # Logging by pressing enter
          pass_elem$sendKeysToElement(list(key = "enter"))
          Sys.sleep(2)
        })
      }, warning,error = function(e){})

    if (is_signinin())
    { # Siginin in, wait until user is actually connected to obi
      while (!is_connected()) {Sys.sleep(1)}
    }
  }
  log("login finished", 1)
}

log <- function(message, loglevel = 0)
{
  if (loglevel <= obiescraper.globals$debuglevel )
  {
    print(message)
  }
}

# Workaround for downloading CSV with RSelenium/Firefox.
# Issue was: Firefox is blocked after one download - https://github.com/SeleniumHQ/selenium-ide/issues/898
# Workaround from https://stackoverflow.com/questions/3749231/download-file-using-javascript-jquery using js injection
download_file <- function(url)
{
  log(paste(".download_file:",url))

  obiescraper.globals$rd$executeScript(paste0('
  var link=document.createElement("a");
  document.body.appendChild(link);
  link.href="',url,'" ;
  link.click();'))

}

# Clean final zeros ",00" that could be displayed when gathering numbers in physical queries
clean_zeros <- function(toclean)
{
  if (is.character(toclean))
  {
    if (stringr::str_sub(toclean,nchar(toclean)-2)==",00")
    {

      return(stringr::str_sub(toclean,1,nchar(toclean)-3))
    }
  }

  return(toclean)

}


# Submit a query to the physical layer by creating a narrative view over direct SQL
# psql is the SQL query that shall be executed
# expected_nrows is the number of records that are expected in the final output
# pconnectionpool is the name of the connection pool
run_physical_query <- function(psql="sselect airport_icao,longitude from my_dwh.airports",
                         expected_nrow=1,
                         pconnectionpool="my_database")
{

  # Add final ;
  sql <- paste0(psql,";")

  rd <- obiescraper.globals$rd


  # Create a new SQL query
  # Navigate to the page dedicaced to creating a physical query
  rd$navigate(paste0(obiescraper.globals$analytics_url, 'saw.dll?Answers&criteriatype=physical'))


  while(TRUE)
  {
    tryCatch({
      connectionpool <- rd$executeScript(paste0('
t=document.querySelectorAll(\'[name="connectionPool"]\');
t[0].value="',pconnectionpool,'";
t=document.querySelectorAll(\'[name="sqlStatement"]\');
t[0].value="',sql,'";
PhysicalCriteriaEditor.onGetColumns(\'idReport\');
                        '))


      tryCatch({
        rd$findElement(using = 'class', value = 'SelectName')
      },error=function(e){
        # If error:
        tryCatch({
          err <- rd$findElement(using = 'id', value = 'idSqlErrorCell')
          message(err$getElementText())
          stop("Unable to retrieve SQL result")
        }, warning=function(w){}, error=function(e){
          print(e$message)
          if (stringr::str_detect(e$message,"Unable to retrieve SQL result"))
          {
            stop(e$message)
          }
        })

      }
      )

      sqlcol <- rd$findElement(using = 'id', value = 'sqlColumns')
      sqlcolstr <- stringr::str_trim(stringr::str_split( sqlcol$getElementText()[[1]],'\n')[[1]])
      # Get the list of columns for that query, and their data type
      sqlcoldf <- data.frame(matrix(sqlcolstr[which(sqlcolstr != "")], ncol = 2, byrow = TRUE))
      if (nrow(sqlcoldf) > 0)
      {
        colnames(sqlcoldf) <- c("Column", "Type")
        print("Query columns:")
        print(sqlcoldf)
        break
      }
    },error = function(e){
      print(e$message)
      if (stringr::str_detect(e$message,"Unable to retrieve SQL result"))
      {
        stop(e$message)
      }
      Sys.sleep(2)}, warning = function(w){
        print(w$message)
        Sys.sleep(2)}
    )
  }

  # Elem found -> query ok



  # Go to SQL result
  res <- rd$executeScript('$("#resultsTab_tab").click()')

  # Click on Add new analysis
  while (TRUE)
  {
    tryCatch({
      rd$executeScript('document.querySelectorAll(\'[id="listViewPaneHeaderToolbar_newView_image"]\')[0].click();')
      break},
      error = function(e){Sys.sleep(2)})

  }

  # Click on Other views
  o <- rd$findElement(using = 'class', value = 'contextMenuOptionNoIcon')
  o$clickElement()

  # Click on Narrative view
  rd$executeScript('
var x = document.getElementsByTagName("img");
var i;
for (i = 0; i < x.length; i++) {
  if (x[i].src.includes("narrative_ena.png"))
  {
    x[i].click();
  }
}')


  current_nrow <- NA
  # Loop until we get the result
  while (is.na(current_nrow) | current_nrow != expected_nrow)
  {
    # Fill the narrative items @1 @...
    while (TRUE)
    {
      tryCatch({
        print(sqlcoldf)
        collist <- paste0("@",seq(1,nrow(sqlcoldf)), collapse = "|")
        o <- rd$findElement(using = 'tag', value = 'textarea')
        rd$executeScript(paste0('
                     t = document.getElementsByTagName("textarea");
                     if (t[2].value==""){
                     document.getElementsByClassName("XUIPromptEntry")[3].childNodes[0].value=10000000
                      t[2].value="|',collist,'";
                      XUIPanel.onChange(\'idView\');
                     }

                     '))
        break
      },error = function(e){Sys.sleep(2)}, warning = function(w){Sys.sleep(2)}
      )
    }


    tryCatch({

      query_result <- rd$findElement(using = 'class', value = 'ViewTable')
      str_res <- query_result$getElementText()[[1]]
      str_col <- stringr::str_split(str_res,"\\|")[[1]]
      str_col <- str_col[2:length(str_col)]

      query_result_df <- data.frame(matrix(str_col, ncol = nrow(sqlcoldf), byrow = TRUE))
      # Add column name
      colnames(query_result_df) <- sqlcoldf$Column
      current_nrow <- nrow(query_result_df)
    },error = function(e){Sys.sleep(2)}, warning = function(w){Sys.sleep(2)}
    )
  }

  return(query_result_df)
}
