#options(encoding="UTF-8")
require("jsonlite")
require("stringi")
require("dplyr")


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

merged <- merge(tweets, users, by = "user_id")

merged$screen_name.x <- NULL
merged$screen_name <- merged$screen_name.y
merged$screen_name.y <- NULL
merged$symbols <- NULL

merged$hashtags_cant <- 0
merged[merged$hashtags != "NA",]$hashtags_cant <- lapply(merged[merged$hashtags != "NA",]$hashtags, length)
merged$hashtags <- NULL

merged$urls_url_cant <- 0
merged[merged$urls_url != "NA",]$urls_url_cant <- lapply(merged[merged$urls_url != "NA",]$urls_url, length)
merged$urls_url <- NULL
merged$urls_t_co <- NULL
merged$urls_expanded_url <- NULL

merged$media_url_cant <- 0
merged[merged$media_url != "NA",]$media_url_cant <- lapply(merged[merged$media_url != "NA",]$media_url, length)
merged$media_url <- NULL
merged$media_t_co <- NULL
merged$media_expanded_url <- NULL
merged$ext_media_url <- NULL
merged$ext_media_t_co <- NULL
merged$ext_media_expanded_url <- NULL

merged$media_type <- NULL

merged$mentions_cant <- 0
merged[merged$mentions_user_id != "NA",]$mentions_cant <- lapply(merged[merged$mentions_user_id != "NA",]$mentions_user_id, length)
merged$mentions_user_id <- NULL
merged$mentions_screen_name <- NULL



glimpse(merged)
str(merged)
View(merged)
summary(merged)
write.csv(merged, "tweets.csv")
