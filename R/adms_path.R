#' ADMS path
#'
#' Finds path to ADMS file (.nc) based on datetime and version
#' @param datetime (character) Basefolder of ADMS results. Default is "/home/dataop/data/nmodel/adms"
#' @param version  (character) e.g. "hkv20b"
#' @return An object of class \code{character}
#' @keywords adms
#' @export
# ' @examples 
# ' example_point = sf::st_point(c(1,2))
# ' example_point = sf::st_sfc(example_point)
# ' example_point = sf::st_sf(example_point)
# ' 
# ' buffer_circle(example_point, 200) -> circular_buffer
# ' plot(circular_buffer)
# ' @export

adms_path <- function(datetime, version, basedir = "/home/dataop/data/nmodel/adms"){

    yyyy = lubridate::year(datetime)
    yyyymm = paste0(yyyy, stringr::str_pad(lubridate::month(datetime), 2, pad = "0"))
    yyyymmdd = paste0(yyyymm, stringr::str_pad(lubridate::day(datetime), 2, pad = "0"))
    yyyymmddhh = paste0(yyyymmdd, stringr::str_pad(lubridate::hour(datetime), 2, pad = "0"))

    path = paste0(basedir, '/', version, "/fcstout.archive/",
        yyyy, "/", yyyymm, "/", yyyymmdd, "/", yyyymmddhh, ".nc")

    if(!file.exists(path)) stop("ADMS file does not exist: {path}" |> glue::glue())
    path
}
