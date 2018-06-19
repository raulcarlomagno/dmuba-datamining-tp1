#options(encoding="UTF-8")
library("jsonlite")
library("stringi")
library("dplyr")
library("DataExplorer")
library("stringr")


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



tweets$screen_name <- NULL
tweets$symbols <- NULL

tweets$hashtags_cant <- 0
tweets[tweets$hashtags != "NA",]$hashtags_cant <- unlist(lapply(tweets[tweets$hashtags != "NA",]$hashtags, length))
tweets$hashtags_cant <- as.integer(tweets$hashtags_cant)
tweets$hashtags <- NULL

tweets$urls_url_cant <- 0
tweets[tweets$urls_url != "NA",]$urls_url_cant <- unlist(lapply(tweets[tweets$urls_url != "NA",]$urls_url, length))
tweets$urls_url_cant <- as.integer(tweets$urls_url_cant)
tweets$urls_url <- NULL
tweets$urls_t_co <- NULL
tweets$urls_expanded_url <- NULL

tweets$media_url_cant <- 0
tweets[tweets$media_url != "NA",]$media_url_cant <- unlist(lapply(tweets[tweets$media_url != "NA",]$media_url, length))
tweets$media_url_cant <- as.integer(tweets$media_url_cant)
tweets$media_url <- NULL
tweets$media_t_co <- NULL
tweets$media_expanded_url <- NULL
tweets$ext_media_url <- NULL
tweets$ext_media_t_co <- NULL
tweets$ext_media_expanded_url <- NULL
tweets$media_type <- NULL

tweets$mentions_cant <- 0
tweets[tweets$mentions_user_id != "NA",]$mentions_cant <- unlist(lapply(tweets[tweets$mentions_user_id != "NA",]$mentions_user_id, length))
tweets$mentions_cant <- as.integer(tweets$mentions_cant)
tweets$mentions_user_id <- NULL
tweets$mentions_screen_name <- NULL

tweets$lat <- NULL
tweets$lng <- NULL
tweets$geo_coords <- NULL
tweets$coords_coords <- NULL
tweets$bbox_coords <- NULL

tweets$reply_to_user <- as.integer(ifelse(is.na(tweets$reply_to_user_id), 0, 1))
tweets$reply_to_user_id <- NULL
tweets$reply_to_screen_name <- NULL

tweets$reply_to_status <- as.integer(ifelse(is.na(tweets$reply_to_status_id), 0, 1))
tweets$reply_to_status_id <- NULL

tweets$place_full_name <- NULL
tweets$place_name <- NULL
tweets$place_type <- NULL
tweets$place_url <- NULL

tweets$country <- NULL
tweets$country_code <- NULL
tweets$quoted_status_id <- NULL #ya tengo el is_quote
tweets$quoted_text <- NULL

tweets$is_quote <- as.integer(ifelse(tweets$is_quote == T, 1, 0))
tweets$is_retweet <- NULL
tweets$lang <- NULL

tweets$created_at <- NULL
tweets$status_id <- NULL

users$url <- NULL
users$profile_url <- NULL
users$profile_expanded_url <- NULL
users$profile_banner_url <- NULL
users$profile_background_url <- NULL
users$profile_image_url <- NULL
users$screen_name <- NULL
users$protected <- NULL
users$verified <- as.integer(ifelse(users$verified == T, 1, 0))
users$name <- NULL
users$location <- NULL
users$description <- NULL
users$account_created_at <- NULL

plot_correlation(users, type = "continuous")
glimpse(users)

plot_correlation(tweets, type = "continuous")
glimpse(tweets)

merged <- merge(tweets, users, by = "user_id", all.x = TRUE)
merged$user_id <- NULL
merged$text <- str_replace_all(merged$text, "[\r\n]" , " ") #le saco los saltos de linea
columnas <- colnames(merged)
merged <- merged[, c(columnas[columnas != "text" & columnas != "source"], "source","text")] #pongo source y text al final


glimpse(merged)
str(merged)
View(merged)
summary(merged)
#write.csv(merged, "tweets.csv", row.names = FALSE, fileEncoding = "UTF-8")

plot_str(merged)
plot_missing(merged)
plot_histogram(merged)
numericalFeatures <- c("favorite_count","retweet_count", "followers_count", "friends_count", "listed_count", "statuses_count", "favourites_count")
plot_histogram(merged[, numericalFeatures])
plot_density(merged[, numericalFeatures])
plot_correlation(merged[, numericalFeatures], type = "continuous")

mergedWithoutCustoms <- merged[, setdiff(colnames(merged), c("mentions_cant", "media_url_cant", "urls_url_cant", "hashtags_cant", "reply_to_status", "reply_to_user"))]
plot_correlation(mergedWithoutCustoms)

plot_correlation(merged)

plot_bar(merged)
plot_prcomp(merged)


###########################################
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
#http://www.sthda.com/english/wiki/print.php?id=204
#http://www.sthda.com/english/wiki/print.php?id=202
library("FactoMineR")
library("FactoInvestigate")
library("factoextra")
merged$popular <- ifelse(merged$favorite_count > 0 & merged$retweet_count > 0, 'si', 'no')

mergedWithoutQuant <- merged[, setdiff(colnames(merged), c("account_lang", "source", "text", "popular"))]
pcaresult <- PCA(mergedWithoutQuant)
#Investigate(pcaresult)
summary(pcaresult, nbelements=Inf)  ## to print all the elements
fviz_pca_var(pcaresult)
get_pca_var(pcaresult)
## Principal Component Analysis Results for variables
##  ===================================================
##   Name       Description                                    
## 1 "$coord"   "Coordinates for the variables"                
## 2 "$cor"     "Correlations between variables and dimensions"
## 3 "$cos2"    "Cos2 for the variables"                       
## 4 "$contrib" "contributions of the variables"

fviz_contrib(pcaresult, choice = "var", axes = 1:2)


coords <- abs(pcaresult$var$coord[,1])
sort.int(coords)

dimdesc(pcaresult)

table(merged$popular)/sum(table(merged$popular))

#se agregaron features, pasando las listas a cantidad de elemntos
#reply_to_user y reply_to_status, marca para saber si fueron respuesta, elimino una ya que estan super correlacionadas
#followers_count y listed_count estan correlacionadas, elimino listed_count que tiene mas correlacion q followers_count con las otras variables
#se elimina verified por tener alta corrleacion con followers_count
#se elimina urls_url_cant y se deja statuses_count, por
#se elimina is_quote, ya que aporta poca variabilidad

mergedfinal <- merged
mergedfinal$popular <- NULL
mergedfinal$reply_to_user <- NULL
mergedfinal$listed_count <- NULL
mergedfinal$verified <- NULL
mergedfinal$urls_url_cant <- NULL
mergedfinal$is_quote <- NULL
mergedfinal$text <- NULL

plot_correlation(mergedfinal)

pcaresultfinal <- PCA(mergedfinal[, setdiff(colnames(mergedfinal), c("account_lang", "source"))])
#Investigate(pcaresult)
summary(pcaresultfinal, nbelements=Inf)  ## to print all the elements
fviz_pca_var(pcaresultfinal)

fviz_pca_var(pcaresultfinal, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pcaresultfinal, choice = "var", axes = 1:2)
fviz_cos2(pcaresultfinal, choice = "var", axes = 1:2)
#me da las variables mas correlacionadas para cada componente
#las que tienen el menor pvalor son las mas significativas para ese componente
dimdesc(pcaresultfinal, axes = 1:2)


summary(mergedfinal$statuses_count)
summary(mergedfinal$followers_count)
# > 3er quartil como umbral
mergedfinal$popular <- ifelse(mergedfinal$statuses_count >= mean(mergedfinal$statuses_count)  & mergedfinal$followers_count >= mean(mergedfinal$followers_count), 'si', 'no')
table(merged$popular)/sum(table(merged$popular))
table(mergedfinal$popular)/sum(table(mergedfinal$popular))
mean(merged$popular == mergedfinal$popular)


write.csv(merged, "tweets.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(mergedfinal, "tweets-processed.csv", row.names = FALSE, fileEncoding = "UTF-8")

