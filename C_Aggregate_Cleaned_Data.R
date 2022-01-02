#first standardize all test score info
agg_cleaned_data <- cleaned_data %>% mutate_at(vars(matches("student_scores")), ~(.-grade))

#check if it worked, it did!
agg_cleaned_data %>% ggplot(aes(x=student_scores_all)) + 
  geom_density()

#now lets aggregate everything down to county and year
agg_cleaned_data <- agg_cleaned_data %>% group_by(key,CountyName,stateabb, year) %>% 
  summarise_if(is.numeric, mean) %>% 
  rename(fips = key)
  select(!grade)

#Lets see
agg_cleaned_data %>% ggplot(aes(x=student_scores_all, y=val, color=factor(year))) + 
  geom_point() + facet_wrap(vars(year))


#plot on US map!
#We have to be careful because some test data is gone for whole states in 2018,
#some housing data is gone for certain years too!!
install.packages("usmap")
library(usmap)
just_2018 <- agg_cleaned_data %>% 
  filter(year == 2018)

plot_usmap(regions="counties", data=just_2018, value="val")+
  scale_fill_continuous( type = "viridis",
  ) + theme(legend.position = "right")

plot_usmap(regions="counties", data=agg_cleaned_data, value="student_scores_all") +
  scale_fill_continuous( type = "viridis", 
) + theme(legend.position = "right")


grades %>% filter(stateabb == "AZ" | stateabb == "NY") %>% distinct(year)
zillow %>% filter(year == 2018 & StateName == "Arizona")

write_csv(agg_cleaned_data, file = "cleaned_collapsedinfo.csv") 
