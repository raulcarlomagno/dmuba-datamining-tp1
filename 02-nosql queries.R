library("mongolite")

db <- "dmuba"
url <- "mongodb://localhost"
collUsers <- mongo(db = db, collection = "Gripe-dump_users", url = url)
collTweets <- mongo(db = db, collection = "Gripe-dump_tweets", url = url)

#2.1
query <- '{$or:[
  {favorite_count: {$gt: 0 }},
  {retweet_count: {$gt: 0 }}
]}'
cantRtFav <- collTweets$count(query)
cantTotal <- collTweets$count('{}')

cantRtFav / cantTotal

#2.2
query <- '[
  {$group : {_id: $user_id, cantweets:{$sum: 1}}},
  {$sort:{cantweets:-1}},
  {$limit: 5}
]'

maxTweeters <- collTweets$aggregate(query)
maxTweeters

#2.3
query <- '{ source: "Twitter Web Client" }'
cantTwitterWeb <- collTweets$count(query)
cantTwitterWeb

#2.4
query <- '[ {$group: {_id: $account_lang, cant:{$sum:1}}} ]'
lenguajes <- collUsers$aggregate(query)
lenguajes

#2.5
userMaxFollowers <- collUsers$find('{}', sort = '{"followers_count": -1}', limit = 1)
tweetsUserMaxFollowers <- collTweets$find(paste0('{"user_id": "', userMaxFollowers$user_id, '"}'))
tweetsUserMaxFollowers$favorite_count
tweetsUserMaxFollowers$retweet_count
