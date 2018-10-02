# load packages ----------------------------------------------------------------
library(tidyverse)
library(rvest)

# read in http://www.imdb.com/chart/tvmeter ------------------------------------
page <- read_html("___")

# years ------------------------------------------------------------------------
years <- page %>%
  html_nodes("___") %>%
  html_text() %>%
  ___

# scores -----------------------------------------------------------------------
scores <- page %>%
  ___

# names ------------------------------------------------------------------------
names <- ___

# tvshows dataframe ------------------------------------------------------------
tvshows <- tibble(
  rank = 1:100,
  ___
)