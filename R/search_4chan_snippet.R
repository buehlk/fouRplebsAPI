#' @title Seach latest or oldest 4chan posts
#' @description This function returns latest or oldest 4chan posts specified by searchterms
#' @param start_date String, Start date of post search. Format: "YYY-MM-DD", Default: ''
#' @param end_date String, End date of post search. Format: "YYY-MM-DD", Default: ''
#' @param boards String or string vector of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo".
#' @param text String, Searchterms in post text. Regex possible, Default: ''
#' @param subject String, Searchterms in post subject. Regex possible, Default: ''
#' @param filename String, Searchterms in filename. Regex possible, Default: ''
#' @param ghost String, Filter by: "all", "only", "none" , Default: 'all'
#' @param user_id Integer, Filter by user_id, Default: ''
#' @param tripcode String, Filter by tripcode, Default: ''
#' @param type String, Filter by post type: "all", "sticky", "op", "posts", Default: 'all'
#' @param username String, Search username, Default: ''
#' @param country String, Filter by country. Format: ISO 3166-1 Alpha-2 country code, Default: ''
#' @param results String, Return post matches or the thread they are found in. Possible values: "all", "thread", Default: ''
#' @param show_only String, Show only posts containing data types: "all", "text", "image", Default: 'all'
#' @param deleted String, Filter deletion status: "all", "not-deleted", "deleted", Default: 'all'
#' @param capcode String, Filter author type: "all", "user", "ver", "mod", "dev", "admin", "manager", "founder", Default: 'all'
#' @param order String, Return posts ordered by latest or oldest: "asc", "desc", Default: 'asc'
#' @param cool Integer (seconds), The 4plebs API includes an API rate limit of 5 requests/min for the archive search. For multiple searches a cool-down is recommended , Default: 0
#' @param page Index of search result, Default: 1
#' @param result_type String, Return output on the post level or only the number of posts found: "snippet", "results_num", Default: 'snippet'
#' @return Dataframe with details on the most recent/oldest posts found.
#' @details Variables in API output:\cr\cr
#' thread_id: 4chan ID of the thread the post is situated in\cr
#' doc_id: 4chan document ID\cr
#' num: 4chan post ID\cr
#' subnum: Binary, 1: Ghost Post, 0: Non-Ghost Post\cr
#' op: Binary, 1: Opening Post, 0: Reply Post\cr
#' timestamp: Time sent in Posix time\cr
#' fourchan_date: Time sent\cr
#' name: Author name\cr
#' title: Post title\cr
#' referencing_comment: The post_id this post is quoting\cr
#' comments: Text of the post\cr
#' poster_country: Author country\cr
#' nreplies: Number of replies
#' formatted: Boolean, Has this post been formatted?\cr
#' media_link: Download link to the media (e.g. images) that have been shared in the post\cr\cr
#'
#' If result_type is "results_num":\cr
#' total_found: All number of total posts found with this query\cr
#' actual_query_result: The 4plebs API limits the search results shown. This limit is 100,000 results. If the number of total posts found exceeds this limit, the actual_query_results are capped.
#' @examples
#' \dontrun{
#' t <- search_4chan_snippet(boards = "adv", text = "kitties", show_only = "image")
#'
#' search_4chan_snippet(boards = "pol", country = "BR", result_type = "results_num")
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

search_4chan_snippet <- function(start_date = "", end_date = "", boards, text = "", subject = "", filename = "", ghost = "all", user_id = "", tripcode = "", type = "all",
                                 username = "", country = "", results = "all", show_only = "all", deleted = "all", capcode = "all", order = "asc", cool = 0, page = 1, result_type = "snippet") {

  filter <- NA
  if(show_only == "text"){
    filter <- "image"
  }
  if(show_only == "image"){
    filter <- "text"
  }
  if(show_only == "all"){
    filter <- "all"
  }
  if(all(boards %in% c("adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo"))){
    boards <- paste0(boards, collapse = ".")
  }else{stop("Invalid board name(s). Please choose from:\n 'adv', 'plebs', 'hr', 'tg', 'tv', 'x', 's4s', 'pol', 'o', 'trv', 'f', 'sp', 'mlpol', 'mo'")}

  results <- ifelse(results == "all", "all", results)
  match.arg(deleted, c("all", "not-deleted", "deleted"))
  match.arg(order, c("asc", "desc"))
  match.arg(ghost, c("all", "only", "none"))
  match.arg(type, c("all", "sticky", "op", "posts"))
  match.arg(filter, c("image", "text", "all"))
  match.arg(results, c("all", "thread"))
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
      if(parsed$meta$total_found > as.numeric(parsed$meta$max_results)){
        cat(paste("The amount of", parsed$meta$total_found,"search results found exceeds the API limit of 100000.\n"))
      }
      cat(paste("The", (page-1)*25+1, "-", (page-1)*25+nrow(query_result), ifelse(order == "asc", "oldest", "newest"), ifelse(results == "", "posts", "threads"), "of the", ifelse(parsed$meta$total_found <= as.numeric(parsed$meta$max_results), parsed$meta$total_found, as.numeric(parsed$meta$max_results)), "total search results","are shown.\n"))
      cat(paste("Scraping all", ifelse(parsed$meta$total_found < as.numeric(parsed$meta$max_results), parsed$meta$total_found, as.numeric(parsed$meta$max_results)),"results would take ~", round(ceiling(as.numeric(ifelse(parsed$meta$total_found < as.numeric(parsed$meta$max_results), parsed$meta$total_found, as.numeric(parsed$meta$max_results)))/25)*20/60, 2), "minutes."))
      query_result
    }else{
      Sys.sleep(cool)
      query_result
    }
  }else{
    Sys.sleep(cool)
    c("total_found" = parsed$meta$total_found, "actual_query_result" = ifelse(as.numeric(parsed$meta$max_results) <= parsed$meta$total_found, parsed$meta$total_found, as.numeric(parsed$meta$max_results)))
  }
}
