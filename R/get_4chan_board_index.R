#' @title Return threads for specific 4chan board index
#' @description This function returns all 10 threads from a given board page index, starting from the latest thread
#' @param board Character variable of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo".
#' @param page Integer of the board page index
#' @param latest_comments Boolean, TRUE: Return opening posts and all replies, FALSE: Return only opening posts, Default: FALSE
#' @param cool Integer (seconds), The 4plebs API includes an undocumented API rate limit for the board index search. For multiple searches a cool-down is recommended , Default: 0
#' @return Dataframe with details on all posts on a given board page.
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
#' nreplies: Number of replies\cr
#' formatted: Boolean, Has this post been formatted?\cr
#' media_link: Download link to the media (e.g. images) that have been shared in the post
#' @examples
#' \dontrun{
#' get_4chan_board_index(board = "mo", page = 1, latest_comments = TRUE)
#'
#' get_4chan_board_index(board = "mo", page = 1, latest_comments = FALSE)
#' }
#' @seealso
#'  \code{\link[httr]{modify_url}}, \code{\link[httr]{user_agent}}, \code{\link[httr]{GET}}, \code{\link[httr]{http_type}}, \code{\link[httr]{content}}, \code{\link[httr]{http_error}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{map2}}
#'  \code{\link[stringr]{str_extract}}
#' @rdname get_4chan_board_index
#' @export
#' @importFrom httr modify_url user_agent GET http_type content http_error
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map map2
#' @importFrom stringr str_extract
#' @importFrom dplyr %>%

get_4chan_board_index <- function(board, page, latest_comments = F, cool = 0) {

  match.arg(board, c("adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo"))

  path <- sprintf("_/api/chan/index/?board=%s&page=%i&order=by_thread", board, page)
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
        httr:status_code(resp),
        parsed$error
      ),
      call. = FALSE
    )
  }


  query_result <- structure(list("thread_id" = integer(),
                                 "doc_id" = character(),
                                 "num" = character(),
                                 "subnum" = character(),
                                 "op" = integer(),
                                 "timestamp" = integer(),
                                 "fourchan_date" = as.Date(character()),
                                 "name" = character(),
                                 "title" = character(),
                                 "referencing_comment" = character(),
                                 "comments" = character(),
                                 "poster_country" = character(),
                                 "nreplies" = character(),
                                 "formatted" = logical(),
                                 "media_link" = character()),
                            class = "data.frame")

  for(i in 1:length(parsed)){

    comments <- parsed[[i]][["posts"]] %>%
      purrr::map("comment") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    comments[which(comments == F)] <-  parsed[[i]][["posts"]] %>%
      purrr::map("comment") %>%
      unlist() %>%
      unname()
    comments[which(comments == T)] <- NA

    doc_id <- parsed[[i]][["posts"]] %>%
      purrr::map("doc_id") %>%
      unlist() %>%
      unname()

    num <- parsed[[i]][["posts"]] %>%
      purrr::map("num") %>%
      unlist() %>%
      unname()

    subnum <- parsed[[i]][["posts"]] %>%
      purrr::map("subnum") %>%
      unlist() %>%
      unname()

    media_link <- parsed[[i]][["posts"]] %>%
      purrr::map2("doc_id", "media") %>%
      purrr::map("media_link") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    media_link[which(media_link == F)] <-  parsed[[i]][["posts"]] %>%
      purrr::map2("doc_id", "media") %>%
      purrr::map("media_link") %>%
      unlist(lapply(., is.null))
    media_link[which(media_link == T)] <- NA

    timestamp <- parsed[[i]][["posts"]] %>%
      purrr::map("timestamp") %>%
      unlist() %>%
      unname()

    fourchan_date <- parsed[[i]][["posts"]] %>%
      purrr::map("fourchan_date") %>%
      unlist() %>%
      unname()

    name <- parsed[[i]][["posts"]] %>%
      purrr::map("name") %>%
      unlist() %>%
      unname()

    formatted <- parsed[[i]][["posts"]] %>%
      purrr::map("formatted") %>%
      unlist() %>%
      unname()

    op <- parsed[[i]][["posts"]] %>%
      purrr::map("op") %>%
      unlist() %>%
      unname() %>%
      as.numeric()

    nreplies <- parsed[[i]][["posts"]] %>%
      purrr::map("nreplies") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    nreplies[which(nreplies == F)] <-  parsed[[i]][["posts"]] %>%
      purrr::map("nreplies") %>%
      unlist() %>%
      unname()
    nreplies[which(nreplies == T)] <- NA

    poster_country <- parsed[[i]][["posts"]] %>%
      purrr::map("poster_country") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    poster_country[which(poster_country == F)] <-  parsed[[i]][["posts"]] %>%
      purrr::map("poster_country") %>%
      unlist() %>%
      unname()
    poster_country[which(poster_country == T)] <- NA

    title <- parsed[[i]][["posts"]] %>%
      purrr::map("title") %>%
      lapply(., is.null) %>%
      unlist() %>%
      unname()
    title[which(title == F)] <-  parsed[[i]][["posts"]] %>%
      purrr::map("title") %>%
      unlist() %>%
      unname()
    title[which(title == T)] <- NA

    referencing_comment <- rep(NA, length(comments))
    referencing_comment <- gsub(">>| ", "", stringr::str_extract(comments, ">>[0-9]*( |\\n)"))

    comments <- gsub(">>[0-9]*( |\\n)", "", comments)

    thread_id <- parsed[[i]][["posts"]] %>%
      purrr::map("thread_num") %>%
      unlist() %>%
      unname()
    if(latest_comments){
      query_result <- rbind(query_result,
                            rbind(
                              data.frame("thread_id" = parsed[[i]][["op"]]$thread_num,
                                         "doc_id" = parsed[[i]][["op"]]$doc_id,
                                         "num" = parsed[[i]][["op"]]$num,
                                         "subnum" = parsed[[i]][["op"]]$subnum,
                                         "op" = as.numeric(parsed[[i]][["op"]]$op),
                                         "timestamp" = parsed[[i]][["op"]]$timestamp,
                                         "fourchan_date" = parsed[[i]][["op"]]$fourchan_date,
                                         "name" = parsed[[i]][["op"]]$name,
                                         "title" = ifelse(is.null(parsed$title), NA, parsed[[i]][["op"]]$media$title),
                                         "referencing_comment" = NA,
                                         "comments" = ifelse(is.null(parsed[[i]][["op"]]$comment), NA, parsed[[i]][["op"]]$comment),
                                         "poster_country" = ifelse(is.null(parsed[[i]][["op"]]$poster_country), NA, parsed[[i]][["op"]]$poster_country),
                                         "nreplies" = ifelse(is.null(parsed[[i]][["op"]]$nreplies), NA, parsed[[i]][["op"]]$nreplies),
                                         "formatted" = parsed[[i]][["op"]]$formatted,
                                         "media_link" = ifelse(is.null(parsed$media), NA, parsed[[i]][["op"]]$media$media_link)
                              ),
                              data.frame("thread_id" = thread_id,
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
                            )
      )
    }else{
      query_result <- rbind(query_result,
                            data.frame("thread_id" = parsed[[i]][["op"]]$thread_num,
                                       "doc_id" = parsed[[i]][["op"]]$doc_id,
                                       "num" = parsed[[i]][["op"]]$num,
                                       "subnum" = parsed[[i]][["op"]]$subnum,
                                       "op" = as.numeric(parsed[[i]][["op"]]$op),
                                       "timestamp" = parsed[[i]][["op"]]$timestamp,
                                       "fourchan_date" = parsed[[i]][["op"]]$fourchan_date,
                                       "name" = parsed[[i]][["op"]]$name,
                                       "title" = ifelse(is.null(parsed$title), NA, parsed[[i]][["op"]]$media$title),
                                       "referencing_comment" = NA,
                                       "comments" = ifelse(is.null(parsed[[i]][["op"]]$comment), NA, parsed[[i]][["op"]]$comment),
                                       "poster_country" = ifelse(is.null(parsed[[i]][["op"]]$poster_country), NA, parsed[[i]][["op"]]$poster_country),
                                       "nreplies" = ifelse(is.null(parsed[[i]][["op"]]$nreplies), NA, parsed[[i]][["op"]]$nreplies),
                                       "formatted" = parsed[[i]][["op"]]$formatted,
                                       "media_link" = ifelse(is.null(parsed$media), NA, parsed[[i]][["op"]]$media$media_link)
                            )
      )

    }
  }
  Sys.sleep(cool)
  query_result
}
