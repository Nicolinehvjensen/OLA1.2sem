
# Indlæs pakken
library(mongolite)

# Forbindelse til MongoDB (test database, games collection)
mongo_con <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")

# Hent alle kampe via indeks (brug kun relevante felter)
query <- '{}'  # Hent alt
fields <- '{"matchId": 1, "team.name": 1, "type.primary": 1, "pass.accurate": 1, "_id": 0}'

# Mål tid for forespørgsel via indeks
system.time({
  indexed_data <- mongo_con$find(query, fields)
})

# Vis de første 5 rækker for at tjekke data
head(indexed_data)




# Forbindelse til MongoDB (test database, games collection)
mongo_games <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
mongo_matches <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

# Aggregation pipeline for at join'e games og matches på matchId
query <- '[ 
    { "$lookup": {
        "from": "matches",
        "localField": "matchId",
        "foreignField": "matchId",
        "as": "match_info"
    }},
    { "$unwind": "$match_info" },  
    { "$project": {
        "matchId": 1,
        "team.name": 1,
        "type.primary": 1,
        "pass.accurate": 1,
        "match_info.label": 1,
        "match_info._id": 1,
        "_id": 0
    }}
]'

# Mål tid for forespørgsel via indeks og join
system.time({
  joined_data <- mongo_games$aggregate(query)
})

# Vis de første 5 rækker for at tjekke data
head(joined_data)



system.time({
  games_data <- mongo_games$find('{}', '{"matchId": 1, "team.name": 1, "type.primary": 1, "pass.accurate": 1, "_id": 0}')
})
head(games_data)






# Installér og indlæs nødvendige pakker
install.packages("mongolite")
library(mongolite)

# Forbindelse til MongoDB
mongo_games <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")

# MongoDB-aggregation pipeline for at join'e `games` med `matches` via `_id`
query <- '[ 
    { "$lookup": {
        "from": "matches",
        "localField": "matchId",
        "foreignField": "_id",
        "as": "match_info"
    }},
    { "$unwind": "$match_info" },
    { "$project": {
        "matchId": 1,
        "team.name": 1,
        "type.primary": 1,
        "pass.accurate": 1,
        "match_info.label": 1, 
        "_id": 0
    }}
]'

# Hent data med join
system.time({
  joined_data <- mongo_games$aggregate(query)
})

# Vis de første rækker
head(joined_data)

