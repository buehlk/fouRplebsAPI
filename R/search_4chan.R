#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param start_date PARAM_DESCRIPTION, Default: ''
#' @param end_date PARAM_DESCRIPTION, Default: ''
#' @param boards PARAM_DESCRIPTION
#' @param text PARAM_DESCRIPTION, Default: ''
#' @param subject PARAM_DESCRIPTION, Default: ''
#' @param filename PARAM_DESCRIPTION, Default: ''
#' @param ghost PARAM_DESCRIPTION, Default: 'none'
#' @param user_id PARAM_DESCRIPTION, Default: ''
#' @param tripcode PARAM_DESCRIPTION, Default: ''
#' @param type PARAM_DESCRIPTION, Default: 'posts'
#' @param username PARAM_DESCRIPTION, Default: ''
#' @param country PARAM_DESCRIPTION, Default: ''
#' @param results PARAM_DESCRIPTION, Default: ''
#' @param show_only PARAM_DESCRIPTION, Default: 'text'
#' @param deleted PARAM_DESCRIPTION, Default: 'not-deleted'
#' @param capcode PARAM_DESCRIPTION, Default: 'user'
#' @param order PARAM_DESCRIPTION, Default: 'asc'
#' @param cool PARAM_DESCRIPTION, Default: 20
#' @param daily PARAM_DESCRIPTION, Default: 'FALSE'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname search_4chan
#' @export

search_4chan <- function(start_date = "", end_date = "", boards, text = "", subject = "", filename = "", ghost = "none", user_id = "", tripcode = "", type = "posts",
                         username = "", country = "", results = "", show_only = "text", deleted = "not-deleted", capcode = "user", order = "asc", cool = 20, daily = "FALSE"){ #API limit of max. 5 requests per minute

  total_results_num <- search_4chan_snippet(start_date = start_date, end_date = end_date, boards, text = text, subject = subject, filename = filename, ghost = ghost, user_id = user_id, tripcode = tripcode, type = type, username = username, country = country, results = results, show_only = show_only, deleted = deleted, capcode = capcode, order = order, cool = 20, result_type = "results_num", page = 1)[1]
  search_results_num <- ifelse(total_results_num > 100000, 100000, total_results_num)

  range <- 1:ceiling(search_results_num/25)
  print(paste("Approximate time:", ceiling(search_results_num/25)*20/60), "minutes.")
  do.call("rbind", lapply(range, function(i){
    search_4chan_snippet(start_date = start_date, end_date = end_date, boards, text = text, subject = subject,
                         filename = filename, ghost = ghost, user_id = user_id, tripcode = tripcode, type = type,
                         username = username, country = country, results = results, show_only = show_only,
                         deleted = deleted, capcode = capcode, order = order, cool = cool, result_type = "snippet", page = i)})
  )
}
