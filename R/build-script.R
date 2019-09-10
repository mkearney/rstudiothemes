r <- gh.com::scrape_rstudio_themes()

library(dplyr)

## define function for scraping README images
imgurls <- function(url) {
  h <- xml2::read_html(url)
  h %>%
    rvest::html_node(".markdown-body.entry-content") %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("src") %>%
    xml2::url_absolute(xml2::xml_url(h)) ->
    img
  h %>%
    rvest::html_node(".markdown-body.entry-content") %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("data-canonical-src") ->
    img2
  if (length(img2) > 0 && any(!is.na(img2))) {
    img2 <- img2 %>% xml2::url_absolute(xml2::xml_url(h))
    img <- c(img, img2)
  }
  list(img = unique(img))
}

## scrape images from URLs
i <- r %>%
  pull(url) %>%
  dapr::lap(imgurls) %>%
  unlist(recursive = FALSE)

## add them as list (of characters)
r$img <- i


write_entry <- function(x) {
  link <- glue::glue_data(x, "## [{repo}]({url})")
  des <- x$description
  if (length(x$img) > 0) {
    img <- paste0(glue::glue_data(list(img = unlist(x$img)), 
      '<p style="text-align:center"><img src="{img}" style="max-width:80%!important"></p>'), collapse = "\n")
    x <- c(link, des, img)
  } else {
    x <- c(link, des)
  }
  paste(x, collapse = "\n\n")
}

## filter/out image paths that don't seem useful
r$img <- dapr::lap(r$img, ~ grep("[[:punct:]](png|gif|jpg|jpeg)", .x, value = TRUE, ignore.case = TRUE) %>%
    grep("hex|logo|orobrju86", ., invert = TRUE, value = TRUE, ignore.case = TRUE))

## write markdown and save as readME
r %>%
  filter(!grepl("^cran/|^mkearney/rstudiothemes", repo)) %>%
  dapr::lapr(write_entry) %>%
  paste(collapse = "\n\n") %>%
  paste0('---
title: "RStudio themes"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

', .) %>%
  cat(file = "README.Rmd")
