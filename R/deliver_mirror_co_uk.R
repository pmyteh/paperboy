#' @export
pb_deliver_paper.mirror_co_uk <- function(x, verbose = NULL, pb, ...) {

  # updates progress bar
  pb_tick(x, verbose, pb)

  # raw html is stored in column content_raw
  html <- rvest::read_html(x$content_raw)

  # datetime
  datetime <- html %>%
    rvest::html_element("meta[property='article:published_time") %>%
    rvest::html_attr("content") %>%
    lubridate::as_datetime()
#    rvest::html_element("[data-testid='byline-publishedDate']") %>%
#    rvest::html_text2() %>%
#    lubridate::as_datetime(format="%H:%M, %d %b %Y")

  # headline
  headline <- html %>%
    rvest::html_element("h1") %>%
    rvest::html_text2()

  # author
  author <- html %>%
    rvest::html_elements("meta[name='author']") %>%
    rvest::html_attr("content") %>%
    stringr::str_trim() %>%
    toString()

  # text
  text <- html %>%
    rvest::html_elements("article p[class^='Paragraph_paragraph-text'") %>%
    rvest::html_text2() %>%
    paste(collapse = "\n")

  # the helper function safely creates a named list from objects
  s_n_list(
    datetime,
    author,
    headline,
    text
  )

}
