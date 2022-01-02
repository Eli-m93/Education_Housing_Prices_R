#load in agg_cleaned_data (Note path for now)
agg_cleaned_data <- read_csv("C:/Users/emogel/Desktop/Spring 2021/Big Data/assignments/Data/cleaned_collapsedinfo.csv")

#Extra Stuff
agg_cleaned_data <- agg_cleaned_data %>% 
  mutate(logval = log(val))
cleaned_data <- cleaned_data %>% 
  mutate(logval = log(val))

agg_cleaned_data_long <- agg_cleaned_data %>% 
  pivot_longer(
  cols = starts_with("student_scores"),
  names_to = "student",
  names_prefix = "student_scores_",
  values_to = "score",
  values_drop_na = TRUE
)

cleaned_data_long <- cleaned_data %>% 
  pivot_longer(
    cols = starts_with("student_scores"),
    names_to = "student",
    names_prefix = "student_scores_",
    values_to = "score",
    values_drop_na = TRUE
  )

######About Data

#Table 1 of data
table1 <- cleaned_data %>% group_by(CountyName, grade) %>% 
  filter(CountyName == "Los Angeles" | CountyName == "Fairfax") %>% 
  summarise(score =mean(student_scores_all))

write.table(table1, file = "table1.csv", sep = ",", quote = FALSE, row.names = F)


# Per group achievement by grade, pooled
cleaned_data_long %>% 
  group_by(key, grade) %>% 
  select(score, logval) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  ggplot() + 
  geom_point(aes(x=score, y=logval)) +
  facet_wrap(vars(student))





#####Results
#Basic regressions in total and grade
corr_table <- cleaned_data %>% 
  mutate_at(vars(matches("student_scores")), ~(.-grade)) %>% 
  mutate(student_scores_all = 100*students_scores_all) %>% 
  group_by(grade) %>% 
  select(student_scores_all, val)
cor(corr_table$val,corr_table$student_scores_all, use="p")


func <- function(corr_table){
  return(data.frame(COR = cor(corr_table$student_scores_all, corr_table$val, use="p")))
}

ddply(corr_table, .(grade), func)


lm(student_scores_all~val, data=corr_table)
lm(student_scores_all~logval*factor(grade), data=corr_table)


#Graph that maps relationship by race
#Per group achievement, pooled
agg_cleaned_data_long %>% 
  group_by(student, fips) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  filter(student %in% c("all", "asn", "blk", "hsp", "wht", "nam", "ecd")) %>% 
  mutate(student = recode(student, all = "All Students", asn = "Asian", blk = "Black", hsp = "Hispanic", nam = "Native American", wht = "White", ecd = "Econ. Dist.")) %>% 
  ggplot(aes(x=score, y=val)) + 
  geom_point(aes(size = 0.5, alpha = 0.5)) +
  geom_smooth(method = "lm", se = F) +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(x = "Learning Rate (Years)", y= "Home Value ($)", title= "Figure 1:", subtitle = "Relationship Between Learning Rates & Home Prices by Demographic") +
  theme(legend.position = "none") +
  facet_wrap(vars(student))

## Test scores and Community
lm(student_scores_all~logval*rural + logval*suburb + logval*town , data = agg_cleaned_data)

#graph for majority type of town
town_info <- agg_cleaned_data %>% ungroup()%>% select(rural, suburb, town, urban) 
town_info <- colnames(town_info)[max.col(town_info, ties.method="first")]

#graph town info
agg_cleaned_data %>% 
  cbind(town_info) %>% 
  ggplot(aes(y=student_scores_all, x=val)) +
  geom_point(aes(size = 0.5, alpha = 0.5)) +
  labs(y = "Learning Rate (Years)", 
       x = "Home Value ($)", 
       title= "Figure 2:", 
       subtitle = "Learning Rate and Home Prices by Locale") +
  theme(legend.position = "none") +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_smooth(method="lm") +
  facet_wrap(vars(...84))


#Results for Geography
library(usmap)
plot_usmap(regions="counties", data=agg_cleaned_data, value="student_scores_all")+
  labs(title = "Figure 3: Test Scores by US counties 2008 - 2018") +
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "white", high ="darkblue", 
                        name = "Learning Rate (Years)", label = scales::comma) + 
  theme(legend.position = "right")
  
tempdf <- agg_cleaned_data %>% 
  group_by(fips, CountyName, stateabb) %>% 
  filter(!is.na(student_scores_all)) %>% 
  summarize_if(is.numeric, mean) %>% 
  arrange(desc(student_scores_all)) %>% 
  select(fips, CountyName ,stateabb, student_scores_all, val) %>% 
  head(100) 

richest_and_poorest <- agg_cleaned_data %>% 
  group_by(fips, CountyName, stateabb) %>% 
  filter(!is.na(student_scores_all)) %>% 
  summarize_if(is.numeric, mean) %>% 
  arrange(desc(student_scores_all)) %>% 
  select(fips, CountyName, stateabb , student_scores_all, val) %>% 
  tail(100) %>% 
  rbind(tempdf) %>% 
  mutate(poor = case_when(
    student_scores_all > 0 ~ "Best County",
    student_scores_all < 0 ~ "Worst County"
  ))




ggplot(agg_cleaned_data, aes(x=val, y=student_scores_all)) +geom_point(alpha=.1, color="grey") + 
  geom_point(data=richest_and_poorest, aes(y=student_scores_all, x=val, color=factor(poor))) +
  labs(y = "Learning Rate (Years)", 
       x = "Home Value ($)", 
       title= "Figure 3:", 
       subtitle = "Learning Rate and Home Prices of 100 Best and Worst Counties")+
  guides(color=guide_legend(title="Type"))+
  scale_color_manual(values = c( "Best County" = "dark green",
                                 "Worst County"="red",
                                 "Other" = "grey")) +
  scale_x_continuous(labels=scales::dollar_format())
  

