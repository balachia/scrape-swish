#!/usr/bin/env Rscript

library(httr)
library(rvest)
library(jsonlite)
library(data.table)

output_path <- "./swish-sessions.csv"

check_response <- function(response, label) {
    if (http_error(response)) {
        stop(sprintf("API request failed for %s: HTTP %d", label, status_code(response)))
    }
}

parse_api_list <- function(data) {
    data |>
        purrr::transpose() |>
        lapply(unlist) |>
        as.data.table()
}

# AJAX requests
# all classes
# https://app.iclasspro.com/api/open/v1/swishswimming/classes?locationId=1&limit=24&page=1
# https://app.iclasspro.com/api/open/v1/swishswimming/classes?page=1

# locations (aka programId)
# https://app.iclasspro.com/api/open/v1/swishswimming/class-programs/1
locations_url <- "https://app.iclasspro.com/api/open/v1/swishswimming/class-programs/1"

# levels (aka levelId)
# https://app.iclasspro.com/api/open/v1/swishswimming/levels/active/1
levels_url <- "https://app.iclasspro.com/api/open/v1/swishswimming/levels/active/1"

# instructors
# https://app.iclasspro.com/api/open/v1/swishswimming/instructors/1/classes

# AJAX request for all classes (paginated, and full)
# https://app.iclasspro.com/api/open/v1/swishswimming/classes?locationId=1&limit=24&page=1
# https://app.iclasspro.com/api/open/v1/swishswimming/classes?page=1
classes_url <- "https://app.iclasspro.com/api/open/v1/swishswimming/classes?page=1"

# all classes returns JSON object
#[1] "totalRecords"       "excludeTotal"       "forceStartDate"
#[4] "showFutureOpenings" "data"               "message"
#[7] "errors"
# data object contains each individual class (total = `totalRecords`), with fields:
# [1] "id"                    "name"                  "minAge"
# [4] "maxAge"                "schedule"              "programId"
# [7] "levelId"               "instructors"           "allowWebRegistration"
#[10] "isOnedayEnabled"       "onedayEnrollmentLabel" "showOpenings"
#[13] "openings"              "futureOpenings"        "futureOpeningDate"
#[16] "allowWaitlist"         "autoApprove"           "dates"
#[19] "availableDates"        "limitMinDate"          "limitMaxDate"
#[22] "limitStartDate"        "image"                 "sessions"
#[25] "startDate"             "endDate"               "limitMin"
#[28] "limitMax"              "availableDays"
# `schedule` appears to be numbered 1-7, 1 = Sunday

fetch_classes_paginated <- function(requestsLimit = 50) {
    classes_url_paginated <- "https://app.iclasspro.com/api/open/v1/swishswimming/classes?locationId=1&limit=%d&page=%d"
    # pull and parse first page
    cat("Page 1\n")
    system.time(p0 <- session(sprintf(classes_url_paginated, requestsLimit, 1))) |>
        print()
    check_response(p0$response, "classes page 1")
    totalRecords <- content(p0$response, as = "parsed")$totalRecords
    cat(sprintf("Expecting %d records\n", totalRecords))
    nPages <- ceiling(totalRecords / requestsLimit)
    combinedData <- content(p0$response, as = "parsed")$data
    print(length(combinedData))
    if(nPages > 1) {
        for(i in 2:nPages) {
            cat(sprintf("Page %d\n", i))
            system.time(p0 <- session(sprintf(classes_url_paginated, requestsLimit, i))) |>
                print()
            check_response(p0$response, sprintf("classes page %d", i))
            parsedData <- content(p0$response, as = "parsed")$data
            combinedData <- c(combinedData, parsedData)
            print(length(combinedData))
        }
    }
    combinedData
}

# fetch class data
cat('Fetching session data...\n')
class_sessions_data <- fetch_classes_paginated()
# system.time(classes_session <- session(classes_url)) |>
#     print()
# print(classes_session$response$status)
# fetch location data
system.time(locs_session <- session(locations_url)) |>
    print()
check_response(locs_session$response, "locations")
# fetch levels data
system.time(levels_session <- session(levels_url)) |>
    print()
check_response(levels_session$response, "levels")

# parse class data
# everything is unlistable *EXCEPT* schedule
# class_data <- content(classes_session$response, as = "parsed")
# class.dat <- class_data$data |>
#     purrr::transpose()
class.transposed <- class_sessions_data |>
    purrr::transpose()
schedule.dat <- class.transposed[["schedule"]] |>
    lapply(\(.) .[[1]]) |>  # extract first (only) element
    parse_api_list() |>
    (`[`)(, c("dayNumber", "startTime", "endTime", "dayName"))
class.dat <- class.transposed |>
    (`[`)(c("id", "name", "programId", "levelId", "openings", "futureOpenings")) |>
    lapply(unlist) |>
    as.data.table()
class.dat <- cbind(class.dat, schedule.dat)

# parse locations data
locs_data <- content(locs_session$response, as = "parsed")
locs.dat <- parse_api_list(locs_data$data)
# reprocess location name
locs.dat[, name := substring(name, 8)]
# recombine locations
loc.remap <- c('Aqua Studio - Outdoor Pool' = 'Aqua Studio',
               'Hub - Indoor Pool' = 'Hub',
               'Hub - Sheltered Pool' = 'Hub',
               'Laguna - 20m Outdoor Pool' = 'Laguna')
locs.dat[, name.short := loc.remap[name]]

# parse levels data
levels_data <- content(levels_session$response, as = "parsed")
levels.dat <- parse_api_list(levels_data$data)
# rewrite levels as ordered factor
levels.dat <- levels.dat[order(sortOrder)]
levels.dat[, name := factor(name, levels = name)]

# merge levels and locations data
class.dat[locs.dat, location := i.name, on = c(programId = "id")]
class.dat[locs.dat, location.short := i.name.short, on = c(programId = "id")]
class.dat[levels.dat, level := i.name, on = c(levelId = "id")]

# build helper columns for google sheets
class.dat[, startTime := paste(substring(startTime, 1, nchar(startTime) - 2),
                               substring(startTime, nchar(startTime) - 1))]
class.dat[, helper := sprintf('%s|%s|%s|%s', dayName, location, level, startTime)]
class.dat[, helper2 := sprintf('%s|%s|%s|%s', dayName, location.short, level, startTime)]

# sort descending by openings
class.dat <- class.dat[order(level, -openings, dayNumber, location, startTime)]

# export this shit
fwrite(class.dat, output_path)

