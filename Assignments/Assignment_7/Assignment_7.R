library(tidyverse)
dat <- read_csv('Utah_Religions_by_County.csv')

dat_clean <- dat %>%
  pivot_longer(cols = -c(County, Pop_2010, Religious),
               names_to = 'Religion',
               values_to = 'Proportion')


#this graph is confusing and isn't very clear
dat_clean %>%
  ggplot(aes(x = Pop_2010,
             y = Proportion,
             color = Religion)) +
geom_point() +
  facet_wrap(~County)


#This shows which religions are the most common throughout all of Utah 
#LDS is by far the most common, Non-Religious is second most common, with 
#Catholic and Evangelical next. The other religions have very small proportions.
dat_clean %>%
  ggplot(aes(x = Religion,
             y = Proportion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) 

  
#This shows which religions are most common in each County 
#based off of this graph it seems like whenever the proportion of people who
#are LDS is lower, the proportion who are non-religious is higher. Catholicism 
#also seems to be higher when the proportion of LDS is lower. 
#For Example: Wasatch County, Summit County, Carbon County, etc. 
dat_clean %>%
  ggplot(aes(x = Religion,
             y = Proportion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~County) 


#Shows the relationship between Population of a County and the Proportion of
#people who are religious, difficult to see a correlation because of outliers
dat_clean %>%
  ggplot(aes(x = Pop_2010,
             y = Religious)) +
  geom_point()


#this is the same graph as above but only including counties that have a 
#population below 200000, there is maybe a slight positive correlation but 
#it's too hard to really tell from just the graph
dat_no_outliers <- dat_clean %>%
  filter(Pop_2010 <= 200000)
  
dat_no_outliers %>%
  ggplot(aes(x = Pop_2010,
             y = Religious)) +
  geom_point()


#This graph shows the Proportion of each religion against the population of 
#each County, but it excludes any proportion that is below .005, as well as 
#excludes any population size over 200000 to make it easier to look at. 
#it looks like maybe there are more non-religious people in Counties with 
#lower populations but it's really hard to tell, this could also maybe be
#true of The Southern Baptist Church?
dat_no_outliers %>%
  filter(Proportion > .005) %>%
  ggplot(aes(x = Pop_2010,
             y = Proportion,
             color = Religion)) +
  geom_point()

#This is the same graph as above but it includes all counties and all 
#population sizes
dat_clean %>%
  filter(Proportion > .005) %>%
  ggplot(aes(x = Pop_2010,
             y = Proportion,
             color = Religion)) +
  geom_point()


#This graph shows population against proportion but it separates out each of 
#the religions to make it easier to see the relationships between population 
#and proportion of a specific religion. It seems like the proportion of
#Non-religious people slightly decreases as population size gets bigger
dat_clean %>%
  filter(Proportion > .005) %>%
  ggplot(aes(x = Pop_2010,
             y = Proportion,
             color = Religion)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Religion) 
  

#This graph is the same as above but makes it easier to see correlations in 
#less common religions by getting rid of the filtered proportion as well as 
#making the scales free. It seems like Catholicism may be higher in lower to
#intermediate populations. Southern Baptist Convention also seems to be higher 
#in more intermediate populations. The Pentecostal Church of God seems to get
#slightly bigger as the population increases, but the scale very very small. 
dat_no_outliers %>%
  ggplot(aes(x = Pop_2010,
             y = Proportion,
             color = Religion)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Religion, scales = 'free_y')




