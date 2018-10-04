# load packages ----------------------------------------------------------------
library(tidyverse)
library(rvest)

# read in http://www.imdb.com/chart/tvmeter ------------------------------------
page <- read_html("http://www.imdb.com/chart/tvmeter")

# years ------------------------------------------------------------------------
years <- page %>%
  html_nodes("a+ .secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%
  str_remove("\\)") %>%
  as.numeric()

# scores -----------------------------------------------------------------------
scores <- page %>%
  html_nodes("#main strong") %>%
  html_text() %>%
  as.numeric()

# names ------------------------------------------------------------------------
names <- page %>%
  html_nodes(".titleColumn a") %>%
  html_text() 

# reading top 3 url ------------------------------------------------------------
maniac <- read_html("https://www.imdb.com/title/tt5580146/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=332cb927-0342-42b3-815c-f9124e84021d&pf_rd_r=J56BZZ82YE1ZHMBW68B0&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=tvmeter&ref_=chttvm_tt_1")
ahs <- read_html("https://www.imdb.com/title/tt1844624/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=332cb927-0342-42b3-815c-f9124e84021d&pf_rd_r=J56BZZ82YE1ZHMBW68B0&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=tvmeter&ref_=chttvm_tt_2")
shameless <- read_html("https://www.imdb.com/title/tt1586680/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=332cb927-0342-42b3-815c-f9124e84021d&pf_rd_r=J56BZZ82YE1ZHMBW68B0&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=tvmeter&ref_=chttvm_tt_3")

# making the genres dataframe --------------------------------------------------
  
genres <- tibble(
  genres = maniac %>%
    html_nodes(".subtext a+ a , .subtext a:nth-child(4)") %>%
    html_text() %>%
    str_remove("\\\n") %>%
    str_remove("\\ ") %>%
    paste(collapse = ", ")
)

genres <- add_row(genres, 
                  genres = ahs %>% 
                    html_nodes(".subtext a+ a , .subtext a:nth-child(4)") %>% 
                    html_text() %>%
                    str_remove("\\\n") %>%
                    str_remove("\\ ") %>%
                    paste(collapse = ", "), 
)

genres <- add_row(genres, 
                  genres = shameless %>% 
                    html_nodes(".subtext a+ a , .subtext a:nth-child(4)") %>% 
                    html_text() %>%
                    str_remove("\\\n") %>%
                    str_remove("\\ ") %>%
                    paste(collapse = ", ")
)

genres <- add_column(genres, rank = 1:3)


# making the runtime dataframe -------------------------------------------------------------

runtime <- tibble(
  runtime = maniac %>% 
    html_nodes("#titleDetails time") %>% 
    html_text()
)

runtime <- add_row(runtime,
                   runtime = ahs %>% 
                     html_nodes("#titleDetails time") %>% 
                     html_text(),
)

runtime <- add_row(runtime, 
                   runtime = shameless %>%
                     html_nodes("#titleDetails time") %>% 
                     html_text()
)

runtime <- add_column(runtime, rank = 1:3)

# making the episodes dataframe ---------------------------------------------------------

episodes <- tibble(
  episodes = maniac %>% 
    html_nodes(".np_right_arrow .bp_sub_heading") %>% 
    html_text() %>%
    str_remove("\\ episodes")
)

episodes <- add_row(episodes, 
                    episodes = ahs %>% 
                      html_nodes(".np_right_arrow .bp_sub_heading") %>% 
                      html_text() %>%
                      str_remove("\\ episodes")
)

episodes <- add_row(episodes,
                    episodes = shameless %>%
                      html_nodes(".np_right_arrow .bp_sub_heading") %>% 
                      html_text() %>%
                      str_remove("\\ episodes")
)

episodes <- add_column(episodes, rank = 1:3)
# making keywords dataframe -----------------------------------------------------------
keywords <- tibble(
  keywords = maniac %>%
                     html_nodes(".itemprop") %>%
                     html_text() %>%
                     paste(collapse = ", ")
)
  
keywords <- add_row(keywords, keywords = ahs %>% 
          html_nodes(".itemprop") %>% 
          html_text() %>%
          paste(collapse = ", ")
)
  
keywords <- add_row(keywords, keywords = shameless %>% 
                      html_nodes(".itemprop") %>%
                      html_text() %>%
                      paste(collapse = ", ")
)

keywords <- add_column(keywords, rank = 1:3)

# tvshows dataframe ------------------------------------------------------------
tvshows <- tibble(
  rank = 1:100,
  names, 
  scores, 
  years
)

# joining dataframes ------------------------------------------------------------

tvshows <- left_join(tvshows, genres)
tvshows <- left_join(tvshows, runtime)     
tvshows <- left_join(tvshows, episodes)
tvshows <- left_join(tvshows, keywords) 
