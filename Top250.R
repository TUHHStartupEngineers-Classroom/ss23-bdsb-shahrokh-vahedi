

url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
library(rvest)
html <- url %>% 
  read_html()

rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()

title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

