#' Two digits FIPS codes for locations excluded from Hubs'
#' target data.
#'
#' Excludes Virgin Islands (78), Northern Mariana
#' Islands (69), Guam (66), American Samoa (60), and Minor
#' Outlying Islands (74).
#'
#' @export
excluded_locations <- c("78", "74", "69", "66", "60")

#' Two digits FIPS codes for locations included in Hubs'
#' target data.
#'
#' Includes 50 states, US national, DC, and Puerto Rico
#' (PR). Excludes Virgin Islands (78), Northern Mariana
#' Islands (69), Guam (66), American Samoa (60), and Minor
#' Outlying Islands (74).
#'
#' @export
included_locations <- c(
  "01",
  "02",
  "04",
  "05",
  "06",
  "08",
  "09",
  "10",
  "11",
  "12",
  "13",
  "15",
  "16",
  "17",
  "18",
  "19",
  "20",
  "21",
  "22",
  "23",
  "24",
  "25",
  "26",
  "27",
  "28",
  "29",
  "30",
  "31",
  "32",
  "33",
  "34",
  "35",
  "36",
  "37",
  "38",
  "39",
  "40",
  "41",
  "42",
  "44",
  "45",
  "46",
  "47",
  "48",
  "49",
  "50",
  "51",
  "53",
  "54",
  "55",
  "56",
  "72",
  "US"
)
