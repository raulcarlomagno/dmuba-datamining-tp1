#options(encoding="UTF-8")
require("jsonlite")
library("stringi")
library("dplyr")
library("DataExplorer")


usersLines <- readLines(file("Gripe-dump_users.json", encoding = "latin1"), warn = F)
fileContent <-  do.call(stri_c, list(usersLines, sep='!', collapse=','))
fileContent <-  do.call(stri_c, list("[", fileContent, "]", collapse=','))
users <- fromJSON(fileContent)
users$`_id` <- NULL

tweetsLines <- readLines(file("Gripe-dump_tweets.json", encoding = "latin1"), warn = F)
fileContent <-  do.call(stri_c, list(tweetsLines, sep='!', collapse=','))
fileContent <-  do.call(stri_c, list("[", fileContent, "]", collapse=','))
tweets <- fromJSON(fileContent)
tweets$`_id` <- NULL

merged <- merge(tweets, users, by = "user_id", all.x = TRUE)

merged$screen_name.x <- NULL
merged$screen_name <- merged$screen_name.y
merged$screen_name.y <- NULL
merged$symbols <- NULL

merged$hashtags_cant <- 0
merged[merged$hashtags != "NA",]$hashtags_cant <- unlist(lapply(merged[merged$hashtags != "NA",]$hashtags, length))
merged$hashtags <- NULL

merged$urls_url_cant <- 0
merged[merged$urls_url != "NA",]$urls_url_cant <- unlist(lapply(merged[merged$urls_url != "NA",]$urls_url, length))
merged$urls_url <- NULL
merged$urls_t_co <- NULL
merged$urls_expanded_url <- NULL

merged$media_url_cant <- 0
merged[merged$media_url != "NA",]$media_url_cant <- unlist(lapply(merged[merged$media_url != "NA",]$media_url, length))
merged$media_url <- NULL
merged$media_t_co <- NULL
merged$media_expanded_url <- NULL
merged$ext_media_url <- NULL
merged$ext_media_t_co <- NULL
merged$ext_media_expanded_url <- NULL

merged$media_type <- NULL

merged$mentions_cant <- 0
merged[merged$mentions_user_id != "NA",]$mentions_cant <- unlist(lapply(merged[merged$mentions_user_id != "NA",]$mentions_user_id, length))
merged$mentions_user_id <- NULL
merged$mentions_screen_name <- NULL

merged$lat <- NULL
merged$lng <- NULL
merged$geo_coords <- NULL
merged$coords_coords <- NULL
merged$bbox_coords <- NULL

merged$reply_to_user <- ifelse(is.na(merged$reply_to_user_id), 0, 1)
merged$reply_to_user_id <- NULL
merged$reply_to_screen_name <- NULL

merged$reply_to_status <- ifelse(is.na(merged$reply_to_status_id), 0, 1)
merged$reply_to_status_id <- NULL

merged$place_full_name <- NULL
merged$place_name <- NULL
merged$place_type <- NULL
merged$place_url <- NULL

merged$country <- NULL
merged$country_code <- NULL
merged$quoted_status_id <- NULL #ya tengo el is_quote
merged$quoted_text <- NULL
merged$name <- NULL
merged$url <- NULL
merged$profile_url <- NULL
merged$profile_expanded_url <- NULL
merged$profile_banner_url <- NULL
merged$profile_background_url <- NULL
merged$profile_image_url <- NULL
merged$screen_name <- NULL
merged$protected <- NULL
merged$is_retweet <- NULL
merged$lang <- NULL

merged$verified <- ifelse(merged$verified == T, 1, 0)
merged$is_quote <- ifelse(merged$is_quote == T, 1, 0)


glimpse(merged)
str(merged)
View(merged)
summary(merged)
write.csv(merged, "tweets.csv", row.names = FALSE)

plot_str(merged)
plot_missing(merged)
plot_histogram(merged)
numericalFeatures <- c("favorite_count","retweet_count", "followers_count", "friends_count", "listed_count", "statuses_count", "favourites_count")
plot_histogram(merged[, numericalFeatures])
plot_density(merged[, numericalFeatures])
plot_correlation(merged[, numericalFeatures], type = "continuous")
plot_correlation(merged)
plot_bar(merged)
plot_prcomp(merged)