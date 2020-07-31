library(RSelenium)
library(dplyr)
library(getPass)


connectobi <- function(path_to_firefox = NA, username = NA, password = NA, obilink = NA)
{
  .init(path_to_firefox,username, password, obilink)
  .connect_obiee()
}

connectobi(username="sthonnar", password="test",obilink="https://people.sc.fsu.edu/~jburkardt/data/csv/csv.html")


#cc <- "https://people.sc.fsu.edu/~jburkardt/data/csv/snakes_count_10000.csv"

#.obiescrapper.globals$rd$navigate(cc)

#.disconnectobi()
