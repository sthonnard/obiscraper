.obiescrapper.globals <- new.env()

.obiescrapper.globals$debuglevel <- 0

.obiescrapper.globals$username <- NA
.obiescrapper.globals$password <- NA
.obiescrapper.globals$firefoxpath <- "/Applications/Firefox.app"

.obiescrapper.globals$firefox_config_prefs <- NA # prefs
.obiescrapper.globals$fprof <- NA # profile

.obiescrapper.globals$rs <- NA #driver
.obiescrapper.globals$rd <- NA # client

.obiescrapper.globals$analytics_url <- NA # Link to obi

.obiescrapper.globals$tempdir <- paste0(tempdir(),'/obiscrapper/')


.obiescrapper.globals$extranet_elem_id <- list()
.obiescrapper.globals$obi_form <- list("user" = "sawlogonuser", "password" = "sawlogonpwd", "invalid_user" = "An invalid User Name or Password was entered")
.obiescrapper.globals$active_form <- .obiescrapper.globals$obi_form

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
      if (as.character(Sys.info()[1]) == "Darwin")
      {# macOs
        .obiescrapper.globals$firefoxpath <- "/Applications/Firefox.app"
      }
      else
      {# Linux
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
    browser.download.downloadDir = .obiescrapper.globals$tempdir,
    browser.download.dir = .obiescrapper.globals$tempdir,
    browser.download.defaultFolder = .obiescrapper.globals$tempdir,
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

  .obiescrapper.globals$fprof <- RSelenium::makeFirefoxProfile(.obiescrapper.globals$firefox_config_prefs)

}

# Initialize credentials to OBI
.init_credential <- function(username, password)
{
  if (is.na(username))
  {
    .obiescrapper.globals$username <- base::readline(prompt = "Username? ")
  }
  else
  {
    .obiescrapper.globals$username <- username
  }

  if (is.na(password))
  {
    .obiescrapper.globals$password <- getPass::getPass(paste0("Password for ",.obiescrapper.globals$username, "?"))
  }
  else
  {
    .obiescrapper.globals$password <- password
  }

}

.init_extranet <- function(extranet_elem_id)
{
  .obiescrapper.globals$extranet_elem_id <- extranet_elem_id
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
.init <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA, debuglevel = NA)
{
  .log("Start initialization", 1)
  .obiescrapper.globals$debuglevel <- debuglevel
  .init_firefoxpath(path_to_firefox)
  .init_credential(username, password)
  .init_obilink(obilink)
  .log("End initialization", 1)
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
    if (is.null(el))
    {
      return(FALSE)
    }else
    {
      return(TRUE)
    }
  }, warning,error = function(e){return(FALSE)})
}

.is_connected <- function()
{ # If element logout exists on the page, it means the user is connected to obi
  return(.is_element_exists(elem = "logout", using = "id"))
}
.is_signinin <- function()
{ # If element signingin exists on the page, it means obi is connecting
  return(.is_element_exists(elem = "signingin", using = "id"))
}

.is_browser_openned <- function()
{ # Check if the Browser is already openned or not
  tryCatch({
    if (typeof(.obiescrapper.globals$rd) != logical)
    {
      activeURL <- .obiescrapper.globals$rd$getCurrentUrl() # Test if it works
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  }, warning,error = function(e){return(FALSE)})
}

.reconnect <- function()
{
  if (!.is_connected())
  { # Reconnect in case user was disconnected
    .connect_obi()
  }
}
# Open firefox and go to the obi URL
.connect_obi <- function()
{

  if (!.is_browser_openned())
  {
    # Path to Firefox
    #ff64 <- .obiescrapper.globals$firefoxpath
    #pr64 <- list(`moz:firefoxOptions` = list(binary = ff64), pageLoadStrategy = 'none')

    try_port <- 4564
    e <- ""
    # Try 500 ports before failing
    while (try_port >= 4000)
    {

      tryCatch(
        {
          #.silence({rs <- rsDriver(browser = "firefox", port = as.integer(try_port), extraCapabilities = c(ff64,
          #                                                                                                pr64,
          #                                                                                                .obiescrapper.globals$fprof ))})
          .silence(.obiescrapper.globals$rs <- RSelenium::rsDriver(port = as.integer(try_port), browser = "firefox",
                         verbose = TRUE, check = TRUE, extraCapabilities = .obiescrapper.globals$fprof))
          break
        },
        warning,error = function(e){
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
.login_obi <- function()
{
  .log("Login to obi", 1)
  # Fill the login form of the extranet and login once available
  attempts = 1
  while (!.is_connected())
  {

    # Determine if logon form is an extranet or OBI form
    tryCatch({.silence(.obiescrapper.globals$rd$findElement(using = 'id', value = .obiescrapper.globals$obi_form$user))
             .log("Detected obi login form", 1)
             },
                        warning,error = function(e){ # Error occurs when element not found
                          .log("Detected extranet form", 1)
                          if (length(.obiescrapper.globals$extranet_elem_id) == 0) {
                            stop("OBI login form not found. Please provide extranet_elem_id to connectobi.")
                          }
                          else
                          {
                            .log("form info", 1)
                            .log(.obiescrapper.globals$extranet_elem_id, 1)
                            .obiescrapper.globals$active_form <- .obiescrapper.globals$extranet_elem_id
                          }
                        }
    )

    # Stop if incorrect username or password
    if (stringr::str_detect(.obiescrapper.globals$rd$getPageSource(), .obiescrapper.globals$active_form$invalid_user))
    {
      stop("Incorrect username or password!")
    }

    .log(paste("Login to obi attemp",attempts), 1)
    .log(.obiescrapper.globals$active_form, 1)
    error <- ""
    tryCatch(
      {
        .silence(
          {
            username_elem <- .obiescrapper.globals$rd$findElement(using = 'id', value = .obiescrapper.globals$active_form$user)
            username_elem$sendKeysToElement(list(""))
            username_elem$sendKeysToElement(list(.obiescrapper.globals$username))
            Sys.sleep(1)
            pass_elem <- .obiescrapper.globals$rd$findElement(using = 'id', value = .obiescrapper.globals$active_form$password)
            pass_elem$sendKeysToElement(list(""))
            pass_elem$sendKeysToElement(list(.obiescrapper.globals$password))
            Sys.sleep(1)
            .log("Login, form has been filled", 1)
          }
        )
      },warning,error = function(e){ # Error occurs when element not found
        tryCatch(
          {
            .log(paste("Login error ",e$message), 1)
            .silence({logout <- .obiescrapper.globals$rd$findElement(using = 'id', value = 'logout')})
            # User already connected to obiee, break
            break
          }, warning,error = function(err){.log(paste("Login error elem logout ",err$message), 1)})
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
        .silence({
          #login_button <- .obiescrapper.globals$rd$findElement(using = 'class', value = "btn")
          #login_button$clickElement()
          # Logging by pressing enter
          pass_elem$sendKeysToElement(list(key = "enter"))
          Sys.sleep(2)
        })
      }, warning,error = function(e){})

    if (.is_signinin())
    { # Siginin in, wait until user is actually connected to obi
      while (!.is_connected()) {Sys.sleep(1)}
    }
  }
  .log("login finished", 1)
}

.log <- function(message, loglevel = 0)
{
  if (loglevel <= .obiescrapper.globals$debuglevel )
  {
    print(message)
  }
}

# Workaround for downloading CSV with RSelenium/Firefox.
# Issue was: Firefox is blocked after one download - https://github.com/SeleniumHQ/selenium-ide/issues/898
# Workaround from https://stackoverflow.com/questions/3749231/download-file-using-javascript-jquery using js injection
.download_file <- function(url)
{
  .log(paste(".download_file:",url))

  .obiescrapper.globals$rd$executeScript(paste0('
  var link=document.createElement("a");
  document.body.appendChild(link);
  link.href="',url,'" ;
  link.click();'))

}
