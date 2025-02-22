% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obiscraper.R
\docType{package}
\name{obiscraper}
\alias{obiscraper}
\title{obiscraper: Query the Oracle Business Intelligence server by web scraping the analytics portal.}
\description{
Execute OBI queries by web scraping the analytics server with RSelenium. The portal login form is handled,
even when behind an extranet. Then queries to the presentation or physical layer are submitted by using
the OBI GO URL (eg saw.dll?Go&SQL=select+Region,Euro+from+SupplierSalesEurope) or by creating a narrative view.
Queries result returned as R Data Frame.
}
\details{
Prerequisite:\cr
-Java
-CRAN packages rJava, RSelenium\cr
-Firefox

Bugs report:\cr
 \url{https://github.com/sthonnard/obiscraper}
}
\section{obiscraper functions}{

\strong{connectobi(path_to_firefox, username, password, obilink)}\cr
Open Firefox, browse the OBI web portal and provide your username in password to the formular so you are logged in.

\strong{submit_query(query)}\cr
Submit a logical query to the OBI web portal by using the GO URL and return the result as a Data Frame.\cr

\strong{submit_physical_sql(sql_query, connection_pool )}\cr
Submit a physical query to the provided connection pool and return the result as a Data Frame.\cr

\strong{disconnectobi()}\cr
Disconnect from OBI and close the web browser.\cr

\strong{get_rs_client()}\cr
Return the RSelenium driver client, enabling more control and debugging.
}

