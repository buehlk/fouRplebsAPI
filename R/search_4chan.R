#' @title Search all 4chan posts
#' @description This function returns all 4chan posts specified by searchterms
#' @param start_date String, Start date of post search. Format: "YYY-MM-DD",
#' Default: ''
#' @param end_date String, End date of post search. Format: "YYY-MM-DD",
#' Default: ''
#' @param boards String or string vector of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol",
#' "o", "trv", "f", "sp", "mlpol", "mo".
#' @param text String, Searchterms in post text. Regex possible, Default: ''
#' @param subject String, Searchterms in post subject. Regex possible,
#' Default: ''
#' @param filename String, Searchterms in filename. Regex possible, Default: ''
#' @param ghost String, Filter by: "all", "only", "none" , Default: 'all'
#' @param user_id Integer, Filter by user_id, Default: ''
#' @param tripcode String, Filter by tripcode, Default: ''
#' @param type String, Filter by post type: "all", "sticky", "op", "posts",
#' Default: 'all'
#' @param username String, Search username, Default: ''
#' @param country String, Filter by country. Format:
#' ISO 3166-1 Alpha-2 country code, Default: ''
#' @param results String, Return post matches or the thread they are found in.
#' Possible values: "all", "thread", Default: 'all'
#' @param show_only String, Show only posts containing data types: "all",
#' "text", "image", Default: 'all'
#' @param deleted String, Filter deletion status: "all", "not-deleted",
#' "deleted", Default: 'all'
#' @param capcode String, Filter author type: "all", "user", "ver", "mod",
#' "dev", "admin", "manager", "founder", Default: 'all'
#' @param order String, Return posts ordered by latest or oldest: "asc",
#' "desc", Default: 'asc'
#' @param cool Integer (seconds), The 4plebs API includes an API rate limit
#' of 5 requests/min for the archive search. For multiple searches a cool-down
#' is recommended , Default: 20
#' @return Dataframe with details of the posts found.
#' @details Variables in API output:\cr\cr
#' thread_id: 4chan ID of the thread the post is situated in\cr
#' doc_id: 4chan document ID\cr
#' num: 4chan post ID \cr
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
#' media_link: Download link to the media (e.g. images) that have been shared
#' in the post
#' @examples
#' \dontrun{
#' search_4chan(boards = "trv", start_date = "2021-04-20",
#'     end_date = "2022-12-21", text = "mallorca|menorca")
#'
#' search_4chan(boards = "o", text = "simson", show_only = "image")
#' }
#' @rdname search_4chan
#' @export

search_4chan <- function(start_date = "", end_date = "", boards, text = "",
                         subject = "", filename = "", ghost = "all",
                         user_id = "", tripcode = "", type = "all",
                         username = "", country = "", results = "all",
                         show_only = "all", deleted = "all", capcode = "user",
                         order = "asc", cool = 20){
  #API limit of max. 5 requests per minute

  total_results_num <- search_4chan_snippet(start_date = start_date,
                                            end_date = end_date, boards,
                                            text = text, subject = subject,
                                            filename = filename, ghost = ghost,
                                            user_id = user_id,
                                            tripcode = tripcode, type = type,
                                            username = username,
                                            country = country,
                                            results = results,
                                            show_only = show_only,
                                            deleted = deleted,
                                            capcode = capcode,
                                            order = order, cool = 20,
                                            result_type = "results_num",
                                            page = 1)[1]
  search_results_num <- ifelse(
    total_results_num > 100000,
    100000,
    total_results_num
    )

  range <- 1:ceiling(search_results_num/25)
  print(paste("Approximate time:",
              round(ceiling(search_results_num/25)*20/60, 2), "minutes."))
  do.call("rbind", lapply(range, function(i){
    search_4chan_snippet(start_date = start_date, end_date = end_date,
                         boards, text = text, subject = subject,
                         filename = filename, ghost = ghost, user_id = user_id,
                         tripcode = tripcode, type = type,
                         username = username, country = country,
                         results = results, show_only = show_only,
                         deleted = deleted, capcode = capcode, order = order,
                         cool = cool, result_type = "snippet", page = i)})
  )
}
