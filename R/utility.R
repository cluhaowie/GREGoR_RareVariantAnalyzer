require(urltools)
require(httr)
require(jsonlite)
# validate url, if true return data.frame, or a vector of FALSE
validate_url <- function(url){
    tryCatch({
        result <- url_parse(url)
        return(result)
    },error=function(e){
        return(FALSE)
    }
    )
}

get_spliceai_result <- function(variant_position) {
    # Define the base URL
    base_url <- "https://spliceailookup-api.broadinstitute.org/spliceai/"

    # Define the query parameters
    query_params <- list(hg = "38", distance = "500", mask = "0", variant = variant_position)

    # Send the GET request
    response <- GET(base_url, query = query_params)
    # Define the proxy settings
    #proxy_url <- "https://64.225.8.82:9987"
    #proxy_username <- "your_proxy_username"
    #proxy_password <- "your_proxy_password"
    # Configure the proxy settings
    #httr::set_config(use_proxy(url = proxy_url,), override = TRUE)
    # Check if the request was successful
    if (status_code(response) != 200) {
        stop("Error: Request failed")
    }

    # Parse the response content as JSON
    content <- content(response, "text",encoding = "UTF-8")
    result <- fromJSON(content)

    # Return the result
    return(result)
}

get_pangolin_result <- function(variant_position) {
    # Define the base URL
    base_url <- "https://spliceailookup-api.broadinstitute.org/pangolin/"

    # Define the query parameters
    query_params <- list(hg = "38", distance = "500", mask = "0", variant = variant_position)

    # Send the GET request
    response <- GET(base_url, query = query_params)

    # Check if the request was successful
    if (status_code(response) != 200) {
        stop("Error: Request failed")
    }

    # Parse the response content as JSON
    content <- content(response, "text")
    result <- fromJSON(content)

    # Return the result
    return(result)
}

parse_spliceai_score <- function(input_string) {
    # Split the string by '---'
    split_by_dash <- strsplit(input_string, '---', fixed = TRUE)[[1]]

    # Split the last element of split_by_dash by '|'
    split_by_pipe <- strsplit(split_by_dash[length(split_by_dash)], '\\|', fixed = F)[[1]]

    # Combine the results
    result <- c(split_by_dash[1:(length(split_by_dash)-1)], split_by_pipe)

    return(result)
}
input_string <- "TRAPPC9---ENSG00000167632.18---ENST00000438773.4---yes---protein_coding---NM_001160372.4,NM_001374682.1,NM_001374683.1,NM_001374684.1|0.04|0.83|0.00|0.62|-32|-2|66|-147|0.01|0.06|0.83|0.00|0.00|0.00|0.91|0.30"
parsed_string <- parse_spliceai_result (input_string)
