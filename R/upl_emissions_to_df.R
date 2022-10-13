#' Reads the emissions of an upl file as a dataframe
#'
#' Reads the emissions of an upl file and outputs a dataframe
#' @param UPL_PATH (character) Full path to the UPL file
#' @param ROUTE_IDs (character vector) Which route ids should be read in?
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @return An object of class \code{data.frame}
#' @keywords adms 
#' @export
# ' @examples 
# ' upl_emissions_to_df("/disk/v092.user/tlh/adms/meta_model/local.disk/hkv20/test/t3.UPL", ROUTE_IDs = "all")
# ' @export
upl_emissions_to_df <- function(UPL_PATH, ROUTE_IDs, cores = 10){

    # debug lines
    # UPL_PATH <- "data/ADMS/ADMS_input/0p_shanghai_street.UPL"
    # ROUTE_ID <- 105651

    #read lines
    #pre-read upl for speedup
    if(!file.exists(UPL_PATH)) "File does not exist: {UPL_PATH}" |> glue::glue() |> stop()
    readLines(UPL_PATH) -> upl

    if(identical(ROUTE_IDs, "all")){
        ROUTE_IDs <- sources_in_upl(UPL_PATH)
    }

    #loop to support multiple ROUTE_IDs
    parallel::mclapply(ROUTE_IDs, mc.cores = cores, FUN = function(ROUTE_ID){
        message(ROUTE_ID)

        #search line with route id
        paste0('SrcName = "', ROUTE_ID, '"') -> matching_pattern
        grep(matching_pattern, upl) -> start_line

        #make bounding box
        grep("ADMS_SOURCE_DETAILS", upl) -> adms_source_lines

        #find start
        adms_source_line_start = which(adms_source_lines < start_line) |> tail(1)
        box_start <- adms_source_lines[adms_source_line_start]

        #find end of box
        if(length(adms_source_lines) == adms_source_line_start){
            box_end = length(upl) #if happens to be the last source in the file
        } else {
            box_end = adms_source_lines[adms_source_line_start + 1] - 1
        }

        #cut the box
        upl_box = upl[box_start:box_end]

        #extract pollution names
        poll_name_start = grep("SrcPollutants", upl_box) + 1
        poll_name_end = grep("SrcPolEmissionRate", upl_box) - 1

        poll_names = upl_box[poll_name_start:poll_name_end]
        poll_names = strsplit(poll_names, split = " ") |>  unlist() |> stringr::str_remove_all('"')
        poll_names = poll_names[nchar(poll_names) > 1]

        #extract emission values, which start 29 lines after SrcName
        emm_start = grep("SrcPolEmissionRate", upl_box) + 1
        emm_end = grep("SrcPolTotalemission", upl_box) - 1

        emm_values = upl_box[emm_start:emm_end]

        #transform into numbers
        emm_values = strsplit(emm_values, split = " ") |> unlist() |> as.numeric()
        emm_values = emm_values[!is.na(emm_values)]

        if(length(poll_names) != length(emm_values)) {
            "Poll names and poll values not the same length, for: {ROUTE_ID}" |> glue::glue() |> stop()
        }

        data.frame(emissions = emm_values) |> t() |> as.data.frame() -> results_df
        results_df$route_id <- ROUTE_ID
        colnames(results_df) <- c(poll_names, "route_id")

        return(results_df)

    }) |> dplyr::bind_rows() -> upl_df
    as.data.frame(upl_df) -> upl_df
    rownames(upl_df) <- NULL

    upl_df
}

#' Lists all sources in an .upl file
#'
#' Lists all sources in an .upl file and gives back a character vector
#' @param UPL_PATH (character) Full path to the UPL file
#' @return An object of class \code{character}
#' @keywords adms 
#' @export
# ' @examples 
# ' sources_in_upl(upl_path)
# ' @export
sources_in_upl <- function(UPL_PATH){
    if(!file.exists(UPL_PATH)) "File does not exist: {UPL_PATH}" |> glue::glue() |> stop()
    readLines(UPL_PATH) -> upl

    all_route_id_positions <- grep("SrcName =", upl)
    all_route_ids <- upl[all_route_id_positions]

    lapply(all_route_ids, function(x){
        gregexpr('\"', x) |> unlist() -> numeric_box
        substr(x, start = numeric_box[1], stop = numeric_box[2]) -> numeric_char
        stringr::str_remove_all(numeric_char, '"')
    }) |> unlist()

}

# upl_emissions_to_df("/disk/v092.user/tlh/adms/meta_model/local.disk/hkv20/test/t3.UPL",
#     ROUTE_IDs = "all")
    
# sources_in_upl("/disk/v092.user/tlh/adms/meta_model/local.disk/hkv20/test/t3.UPL") -> all_sources
# upl_emissions_to_df("/disk/v092.user/tlh/adms/meta_model/local.disk/hkv20/test/t3.UPL",
#     ROUTE_IDs = all_sources, cores = 20) -> tt
