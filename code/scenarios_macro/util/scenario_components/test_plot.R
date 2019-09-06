# Test plot
df <- LifeYearLost %>% group_by(age_group, year) %>% summarise(sumLYL = sum(LYL))
test_graph <- ggplot(data = df, aes(x=year, y=sumLYL, fill=age_group)) + geom_area()
test_graph