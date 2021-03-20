# obiscraper

## Introduction

This tool submits logical or physical queries by scraping the OBIEE 12c (Oracle Business Intelligence Enterprise Edition 12c) analytics portal and returns the result as R Data Frame.

## Technical description

This is basically a convenient interface to Selenium for web scraping the OBIEE portal. This package will also take care of the login form, **even if the login form is not the default OBIEE page**. For instance when the login form is handled by your organization.

-   Login

The tool will gather all HTML input elements of the login page, fill the password in the first element that has type *password*, the *username* in the element just before, and finally will click on the first button that is just after the password element. 

-   Logical query

When submitting a logical query, *obiscraper* uses the Oracle Business Intelligence GO URL in order to download the output as a text file in your temp folder. Then this text file is parsed in R and converted into a Data Frame.

-   Physical query

When submitting a physical SQL query, *obiscraper* creates a narrative view based on that query and then extracts the content from the page. **Experimental.**

## Dependencies

Java

Firefox

rJava

RSelenium

## Installation 
### Installation of rJava on Linux (Ubuntu)

In case Java is not installed on your system, you can install it with the command below in your terminal:

    sudo apt install openjdk-11-jdk

    sudo R CMD javareconf

Then in the R console:

    install.packages("rJava")

### Installation of obiscraper

    library(devtools)
    install_github("sthonnard/obiscraper")

## Examples

```{r}
library(obiscraper)

# Open Firefox and connect to https://my_fancy_company.com/analytics/
# Password will be prompted
connectobi(username = "kenny", obilink = "https://my_fancy_company.com/analytics/")

# Submit logical query
customers <- obiscraper::submit_query('
                          SELECT
                          "FancyDwh"."Customers"."Customer Name" s_1
                          FROM "FancyDwh"
                          ORDER BY 1 ASC NULLS LAST
                          FETCH FIRST 10 ROWS ONLY')

# Sumbit physical query to connection pool live_dwh
customers_2 <- obiscraper::submit_physical_sql('select customer_name from fancy_dwh.dim_customer where rownum<10','live_dwh')


# Close Firefox
obiscraper::disconnectobi()


```

## Limitations

-   Might not work properly when OBIEE is not in **English**

-   Not tested on Windows yet. Tested on Ubuntu and macOS.


