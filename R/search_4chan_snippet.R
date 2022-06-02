#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param start_date PARAM_DESCRIPTION, Default: ''
#' @param end_date PARAM_DESCRIPTION, Default: ''
#' @param boards PARAM_DESCRIPTION
#' @param text PARAM_DESCRIPTION, Default: ''
#' @param subject PARAM_DESCRIPTION, Default: ''
#' @param filename PARAM_DESCRIPTION, Default: ''
#' @param ghost PARAM_DESCRIPTION, Default: 'all'
#' @param user_id PARAM_DESCRIPTION, Default: ''
#' @param tripcode PARAM_DESCRIPTION, Default: ''
#' @param type PARAM_DESCRIPTION, Default: 'posts'
#' @param username PARAM_DESCRIPTION, Default: ''
#' @param country PARAM_DESCRIPTION, Default: ''
#' @param results PARAM_DESCRIPTION, Default: ''
#' @param show_only PARAM_DESCRIPTION, Default: ''
#' @param deleted PARAM_DESCRIPTION, Default: 'all'
#' @param capcode PARAM_DESCRIPTION, Default: 'all'
#' @param order PARAM_DESCRIPTION, Default: 'asc'
#' @param cool PARAM_DESCRIPTION, Default: 0
#' @param page PARAM_DESCRIPTION, Default: 1
#' @param result_type PARAM_DESCRIPTION, Default: 'snippet'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{modify_url}}, \code{\link[httr]{user_agent}}, \code{\link[httr]{GET}}, \code{\link[httr]{http_type}}, \code{\link[httr]{content}}, \code{\link[httr]{http_error}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{map2}}
#'  \code{\link[stringr]{str_extract}}
#' @rdname search_4chan_snippet
#' @export
#' @importFrom httr modify_url user_agent GET http_type content http_error
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map map2
#' @importFrom stringr str_extract
#' @importFrom dplyr %>%

search_4chan_snippet <- function(start_date = "", end_date = "", boards, text = "", subject = "", filename = "", ghost = "all", user_id = "", tripcode = "", type = "posts",
                                 username = "", country = "", results = "", show_only = "", deleted = "all", capcode = "all", order = "asc", cool = 0, page = 1, result_type = "snippet") {

  filter <- NA
  if(show_only == "text"){
    filter <- "image"
  }
  if(show_only == "image"){
    filter <- "text"
  }
  if(show_only == ""){
    filter <- "all"
  }
  if(all(boards %in% c("adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo"))){
    boards <- paste0(boards, collapse = ".")
  }else{stop("Invalid board name(s). Please choose from:\n 'adv', 'plebs', 'hr', 'tg', 'tv', 'x', 's4s', 'pol', 'o', 'trv', 'f', 'sp', 'mlpol', 'mo'")}

  match.arg(deleted, c("all", "not-deleted", "deleted"))
  match.arg(order, c("asc", "desc"))
  match.arg(ghost, c("all", "only", "none"))
  match.arg(type, c("all", "sticky", "op", "posts"))
  match.arg(filter, c("image", "text", "all"))
  match.arg(capcode, c("all", "user", "ver", "mod", "dev", "admin", "manager", "founder"))
  match.arg(result_type, c("snippet", "results_num"))

  path <- sprintf("_/api/chan/search/?boards=%s&email=&username=%s&tripcode=%s&capcode=%s&subject=%s&text=%s&uid=%s&country=%s&filename=%s&image=&deleted=%s&ghost=%s&filter=%s&type=%s&start=%s&end=%s&results=%s&order=%s&page=%i",
                  boards, username, tripcode, capcode, subject, text, user_id, country, filename, deleted, ghost, filter, type, start_date, end_date, results, order, page)
  url <- httr::modify_url("http://archive.4plebs.org/", path = path)
  ua <- httr::user_agent("4Rplebs API")
  resp <- httr::GET(url, ua)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  if (httr::http_error(resp)|is.null(parsed[["error"]]) == F) {
    stop(
      sprintf(
        "4plebs.org API request failed [%s]\n%s",
        httr::status_code(resp),
        parsed$error
      ),
      call. = FALSE
    )
  }

  if(result_type == "snippet"){
    comments <- parsed[[1]][["posts"]] %>%
      purrr::map("comment") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    comments[which(comments == F)] <-  parsed[[1]][["posts"]] %>%
      purrr::map("comment") %>%
      unlist() %>%
      unname()
    comments[which(comments == T)] <- NA

    doc_id <- parsed[[1]][["posts"]] %>%
      purrr::map("doc_id") %>%
      unlist() %>%
      unname()

    num <- parsed[[1]][["posts"]] %>%
      purrr::map("num") %>%
      unlist() %>%
      unname()

    subnum <- parsed[[1]][["posts"]] %>%
      purrr::map("subnum") %>%
      unlist() %>%
      unname()

    media_link <- parsed[[1]][["posts"]] %>%
      purrr::map2("doc_id", "media") %>%
      purrr::map("media_link") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    media_link[which(media_link == F)] <-  parsed[[1]][["posts"]] %>%
      purrr::map2("doc_id", "media") %>%
      purrr::map("media_link") %>%
      unlist(lapply(., is.null))
    media_link[which(media_link == T)] <- NA

    timestamp <- parsed[[1]][["posts"]] %>%
      purrr::map("timestamp") %>%
      unlist() %>%
      unname()

    fourchan_date <- parsed[[1]][["posts"]] %>%
      purrr::map("fourchan_date") %>%
      unlist() %>%
      unname()

    name <- parsed[[1]][["posts"]] %>%
      purrr::map("name") %>%
      unlist() %>%
      unname()

    formatted <- parsed[[1]][["posts"]] %>%
      purrr::map("formatted") %>%
      unlist() %>%
      unname()

    op <- parsed[[1]][["posts"]] %>%
      purrr::map("op") %>%
      unlist() %>%
      unname() %>%
      as.numeric()

    nreplies <- parsed[[1]][["posts"]] %>%
      purrr::map("nreplies") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    nreplies[which(nreplies == F)] <-  parsed[[1]][["posts"]] %>%
      purrr::map("nreplies") %>%
      unlist() %>%
      unname()
    nreplies[which(nreplies == T)] <- NA

    poster_country <- parsed[[1]][["posts"]] %>%
      purrr::map("poster_country") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    poster_country[which(poster_country == F)] <-  parsed[[1]][["posts"]] %>%
      purrr::map("poster_country") %>%
      unlist() %>%
      unname()
    poster_country[which(poster_country == T)] <- NA

    title <- parsed[[1]][["posts"]] %>%
      purrr::map("title") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    title[which(title == F)] <-  parsed[[1]][["posts"]] %>%
      purrr::map("title") %>%
      unlist() %>%
      unname()
    title[which(title == T)] <- NA

    referencing_comment <- rep(NA, length(comments))
    referencing_comment <- gsub(">>| ", "", stringr::str_extract(comments, ">>[0-9]*( |\\n)"))

    comments <- gsub(">>[0-9]*( |\\n)", "", comments)

    thread_id <- parsed[[1]][["posts"]] %>%
      purrr::map("thread_num") %>%
      unlist() %>%
      unname()

    query_result <- data.frame("thread_id" = thread_id,
                               "doc_id" = doc_id,
                               "num" = num,
                               "subnum" = subnum,
                               "op" = op,
                               "timestamp" = timestamp,
                               "fourchan_date" = fourchan_date,
                               "name" = name,
                               "title" = title,
                               "referencing_comment" = referencing_comment,
                               "comments" = comments,
                               "poster_country" = poster_country,
                               "nreplies" = nreplies,
                               "formatted" = formatted,
                               "media_link" = media_link
    )

    if(cool == 0){
      if(parsed$meta$total_found >= parsed$meta$max_results){
        cat(paste("The amount of", parsed$meta$total_found,"search results found exceeds the API limit of 100000.\n"))
      }
      cat(paste("The", (page-1)*25+1, "-", (page-1)*25+nrow(query_result), ifelse(order == "asc", "oldest", "newest"), ifelse(results == "", "posts", "threads"), "of the", ifelse(parsed$meta$total_found <= parsed$meta$max_results, parsed$meta$total_found, parsed$meta$max_results), "total search results","are shown.\n"))
      cat(paste("Scraping all", ifelse(parsed$meta$total_found <= parsed$meta$max_results, parsed$meta$total_found, parsed$meta$max_results),"results would take ~", (ceiling(as.numeric(ifelse(parsed$meta$total_found <= parsed$meta$max_results, parsed$meta$total_found, parsed$meta$max_results))/25)*20)/60), "minutes.")
      query_result
    }else{
      Sys.sleep(cool)
      query_result
    }
  }else{
    Sys.sleep(cool)
    c("total_found" = parsed$meta$total_found, "actual_query_result" = ifelse(parsed$meta$max_results <= parsed$meta$total_found, parsed$meta$total_found, parsed$meta$max_results))
  }
}