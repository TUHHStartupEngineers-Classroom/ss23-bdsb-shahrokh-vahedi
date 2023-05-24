# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  read_html() %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>%
  as_tibble(.name_repair = "minimal")
