dehumids <- c(1,2,4,3)
slope <- c(0,0.2,0.5,0.1)

test <- data.frame(test=dehumids)
cbind(test, slope)
test2 <- as.matrix(test)
as_tibble(test2)

plot(slope ~ dehumids)
help(lm)
data()
help(trees)
data(trees)

fitted_model <- lm(Girth ~ Height, data=trees)
plot(Girth ~ Height, data=trees)
abline(fitted_model, col=4)
abline(v=70, col=2)
abline(h=16, col=3)


as_tibble(a=1)
library(dplyr)

num_vec <- c(3,6,3,8)
spp_vec <- c("spp1","spp3","spp2","spp3")
dataframe <- data.frame(num_vec, spp_vec)
data(trees)
tree_data <- trees
tree_data$light <- c(rep(c("shade","sun"),each=15),"sun")
my_matrix <- as.matrix(dataframe)
plot(Girth ~ as.factor(light), data=tree_data)

summarize(tree_data, 
          mean_girth = mean(Girth), 
          max_height = max(Height))
mutate(tree_data,
       Height_meters = Height * 0.3048, 
       Girth = mean(Girth))

test <- group_by(tree_data, light)
summarize(test, 
          mean_girth = mean(Girth), 
          max_height = max(Height))

Ungroup(data=dataframe)
