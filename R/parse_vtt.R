#' Utility to approximate one distribution with another
#'
#' @param file vtt file
#' @param smry whether to return raw data (FALSE, default) or a summary of the data (TRUE)
#'
#' @export
#'
parse_vtt <- function(file, smry=FALSE) {
    x <- suppressWarnings(readLines(file))

    stopifnot(x[1]=='WEBVTT',
              x[2]=='')

    data <- tibble::tibble(line=x[3:length(x)]) %>%
        dplyr::mutate(order = dplyr::row_number())

    #format seems to be groups of three lines then a blank
    stopifnot(all(data$line[data$order %% 4==0]== ''))

    data <- data %>%
        dplyr::mutate(comment = ceiling(order/4),
                      type = dplyr::case_when(order %% 4==1 ~ 'id',
                                       order %% 4==2 ~ 'timestamps',
                                       order %% 4==3 ~ 'content')) %>%
        dplyr::filter(!is.na(type)) %>%
        tidyr::pivot_wider(id_cols=comment, names_from=type, values_from=line) %>%
        dplyr::mutate(person = stringr::str_match(id,'["]([^"]+)["]')[,2],
                      start = hms::as_hms(stringr::str_match(timestamps, '([0-9:.]+) --> ([0-9:.]+)')[,2]),
                      end   = hms::as_hms(stringr::str_match(timestamps, '([0-9:.]+) --> ([0-9:.]+)')[,3]),
                      duration = end-start,
                      chars = stringr::str_length(content),
                      words = stringr::str_count(content,'\\w+')) %>%
        dplyr::select(comment, person, start, end, duration, content, words, chars)

    if(smry) {
        data %>%
            dplyr::filter(!is.na(person)) %>%
            dplyr::mutate(curses = stringr::str_count(stringr::str_to_lower(content),'(\\W|^)(fuck|shit|damn|hell)(ing|ed|s)?(\\W|$)'),
                   filler = stringr::str_count(stringr::str_to_lower(content),'(\\W|^)(um+|like|yeah|well|uh|sort of|you know)(\\W|$)')) %>%
            dplyr::group_by(person) %>%
            dplyr::summarize(time = sum(duration),
                      words = sum(words),
                      filler = sum(filler),
                      curses = sum(curses)) %>%
            dplyr::mutate(pace = 60*words/as.numeric(time),
                   fillerpace = 60*filler/as.numeric(time)) %>%
            dplyr::arrange(-time) %>%
            return()
    } else {
        return(data)
    }
}
