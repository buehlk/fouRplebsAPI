#' @title Return specific 4chan thread
#' @description Return 4chan thread by looking up its ID
#' @param board Character variable of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol",
#' "o", "trv", "f", "sp", "mlpol", "mo".
#' @param thread_id Dataframe with details of all posts in the thread.
#' @return Dataframe with details on all posts in the thread.
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
#' comments: Text of the post
#' poster_country: Author country\cr
#' nreplies: Number of replies\cr
#' formatted: Boolean, Has this post been formatted?\cr
#' media_link: Download link to the media (e.g. images) that have been shared in
#'  the post
#' @examples
#' \dontrun{
#' get_4chan_thread(board = "adv", thread_id = 21738271)
#'
#' get_4chan_thread(board = "trv", thread_id = 1888747)
#' }
#' @seealso
#'  \code{\link[httr]{modify_url}}, \code{\link[httr]{user_agent}},
#'  \code{\link[httr]{GET}}, \code{\link[httr]{http_type}},
#'  \code{\link[httr]{content}}, \code{\link[httr]{http_error}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{map2}}
#'  \code{\link[stringr]{str_extract}}
#' @rdname get_4chan_thread
#' @export
#' @importFrom httr modify_url user_agent GET http_type content http_error
#' status_code
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map map2
#' @importFrom stringr str_extract
#' @importFrom dplyr %>%
#' @autoglobal

get_4chan_thread <- function(board, thread_id) {
  match.arg(board, c("adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o",
                     "trv", "f", "sp", "mlpol", "mo"))
  path <- sprintf("_/api/chan/thread/?board=%s&num=%i", board, thread_id)
  url <- httr::modify_url("http://archive.4plebs.org/", path = path)
  ua <- httr::user_agent("4Rplebs API")
  resp <- httr::GET(url, ua)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

  if (httr::http_error(resp)|is.null(parsed[["error"]]) == FALSE) {
    statuscode <- httr::status_code(resp)
    stop(
      sprintf(
        "4plebs.org API request failed [%s]\n%s",
        statuscode,
        parsed$error
      ),
      call. = FALSE
    )
  }


  comments <- parsed[[1]][["posts"]] %>%
    purrr::map("comment") %>%
    unlist() %>%
    unname()

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
  media_link[which(media_link == FALSE)] <-  parsed[[1]][["posts"]] %>%
    purrr::map2("doc_id", "media") %>%
    purrr::map("media_link") %>%
    unlist(lapply(., is.null))
  media_link[which(media_link == TRUE)] <- NA

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
  nreplies[which(nreplies == FALSE)] <-  parsed[[1]][["posts"]] %>%
    purrr::map("nreplies") %>%
    unlist() %>%
    unname()
  nreplies[which(nreplies == TRUE)] <- NA

  poster_country <- parsed[[1]][["posts"]] %>%
    purrr::map("poster_country") %>%
    lapply(., is.null) %>%
    unlist() %>%
    unname()
  poster_country[which(poster_country == FALSE)] <-  parsed[[1]][["posts"]] %>%
    purrr::map("poster_country") %>%
    unlist() %>%
    unname()
  poster_country[which(poster_country == TRUE)] <- NA

  title <- parsed[[1]][["posts"]] %>%
    purrr::map("title") %>%
    lapply(., is.null) %>%
    unlist() %>%
    unname()
  title[which(title == FALSE)] <-  parsed[[1]][["posts"]] %>%
    purrr::map("title") %>%
    unlist() %>%
    unname()
  title[which(title == TRUE)] <- NA

  referencing_comment <- rep(NA, length(comments))
  referencing_comment <- gsub(">>| ", "",
                              stringr::str_extract(comments, ">>[0-9]*( |\\n)"))

  comments <- gsub(">>[0-9]*( |\\n)", "", comments)

  rbind(
    data.frame("thread_id" = thread_id,
               "doc_id" = parsed[[1]][["op"]]$doc_id,
               "num" = parsed[[1]][["op"]]$num,
               "subnum" = parsed[[1]][["op"]]$subnum,
               "op" = as.numeric(parsed[[1]][["op"]]$op),
               "timestamp" = parsed[[1]][["op"]]$timestamp,
               "fourchan_date" = parsed[[1]][["op"]]$fourchan_date,
               "name" = parsed[[1]][["op"]]$name,
               "title" = ifelse(is.null(parsed$title), NA,
                                parsed[[1]][["op"]]$media$title),
               "referencing_comment" = NA,
               "comments" = parsed[[1]][["op"]]$comment,
               "poster_country" =
                 ifelse(is.null(parsed[[1]][["op"]]$poster_country), NA,
                        parsed[[1]][["op"]]$poster_country),
               "nreplies" = ifelse(is.null(parsed[[1]][["op"]]$nreplies),
                                   NA, parsed[[1]][["op"]]$nreplies),
               "formatted" = parsed[[1]][["op"]]$formatted,
               "media_link" = ifelse(is.null(parsed$media), NA,
                                     parsed[[1]][["op"]]$media$media_link)
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
}
