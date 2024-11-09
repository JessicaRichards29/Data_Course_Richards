library(tidyverse)
library(gganimate)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv") 

#Task 1: clean data into tidy (long) form
dat_clean <- dat %>%
  pivot_longer(cols = starts_with('Hr_'),
               names_to = 'Time',
               names_prefix = 'Hr_',
               values_to = 'Absorbance')

dat_clean$Time <- as.numeric(dat_clean$Time)

#Task 2 is to create a new column specifying whether a sample is from soil or water
dat_clean_2 <- dat_clean %>%
  mutate(Type = case_when(
    `Sample ID` %in% c("Clear_Creek", "Waste_Water") ~ 'Water',
    TRUE ~ 'Soil'
  ))

#Task 3 is to generate a plot that matches the one from the assignment (plotting dilution == 0.1)

dat_plot <- dat_clean_2 %>%
  filter(Dilution == 0.1)

ggplot(data = dat_plot, aes(x = Time,
                            y = Absorbance,
                            color = Type)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Substrate) +
  theme_minimal() +
  ylim(0,2)
labs(x = 'Time',
     y = 'Absorbance') 

#Task 4 create animated plot
dat_plot_2 <- dat_clean_2 %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Dilution, Time) %>%
  summarise(Mean_absorbance = mean(Absorbance, na.rm = TRUE), .groups = 'drop') 


p <- ggplot(data = dat_plot_2, aes(x = Time, 
                                   y = Mean_absorbance, 
                                   color = `Sample ID`, 
                                   group = `Sample ID`)) +
  geom_line() + 
  facet_wrap(~ Dilution) +
  theme_minimal() +
  gganimate::transition_reveal(Time) +
  gganimate::ease_aes('linear')  

gganimate::animate(p, nframes = 100, fps = 10)

