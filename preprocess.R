#options(encoding="UTF-8")
require("jsonlite")
require("stringi")


usersLines <- readLines(file("Gripe-dump_users.json", encoding = "latin1"), warn = F)
fileContent <-  do.call(stri_c, list(usersLines, sep='!', collapse=','))
fileContent <-  do.call(stri_c, list("[", fileContent, "]", collapse=','))
users <- fromJSON(fileContent)                      

tweetsLines <- readLines(file("Gripe-dump_tweets.json", encoding = "latin1"), warn = F)
fileContent <-  do.call(stri_c, list(tweetsLines, sep='!', collapse=','))
fileContent <-  do.call(stri_c, list("[", fileContent, "]", collapse=','))
tweets <- fromJSON(fileContent)                      
