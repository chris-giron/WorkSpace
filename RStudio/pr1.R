load('movies_merged')
df = movies_merged

sum(df$Rated == "G" | df$Rated == "PG" | df$Rated == "PG-13" | df$Rated == "R")
Rate_ind = df$Rated == "G" | df$Rated == "PG" | df$Rated == "PG-13" | df$Rated == "R"
Rate = df[Rate_ind,]
