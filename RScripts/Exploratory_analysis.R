library(tidyverse)
library(NHSRdatasets)
library(here)
library(knitr)
library(scales)
library(lubridate)
library(caret)
library(ggpubr)

data(ae_attendances)
ae<-ae_attendances

ae <- rowid_to_column(ae, "index")

ae %>%
  # Set the period column to show in month-year format
  mutate_at(vars(period), format, "%b-%y") %>% 
  # Set the numeric columns to have a comma at the 1000's place
  mutate_at(vars(attendances, breaches, admissions), comma) %>%
  # Show the first 10 rows
  head(10) %>%
  # Format as a table
  kable()

### Let's save provisional subsetted ae_attendances data to the 'RawData' folder
#Of note, when naming folders and files, it is important that you do so in a consistent, logical and predictable way means that information may be located, identified and retrieved by your and your colleagues, as quickly and easily as possible. With this in mind let's name this file "ae_attendances_ENG_4hr_perfom", and write it to the raw data folder.
glimpse(ae)
write_csv(ae, here("RawData", "ae_attendances_ENG_4hr_perfom.csv"))

#split to test and training data
#Work out the proportion (`prop`) of the raw data to assign to the training data:
prop<-(1-(15/nrow(ae)))
#'set.seed()' function is a random number generator, which is useful for creating random objects that can be reproduced.
set.seed(333)
#Partitioning the raw data into the test and training data.
trainIndex <- createDataPartition(ae$index, p = prop, 
                                  list = FALSE, 
                                  times = 1)
# All records that are in the trainIndex are assigned to the training data.
aeTrain <- ae[ trainIndex,]
# Our next task, it to save ae_attendances_ENG_4hr_perfom training data to your working data folder 'Data'
write_csv(aeTrain, here("Data", "ae_attendances_ENG_4hr_perfom_train.csv"))

### Let's extract the ae_attendances_ENG_4hr_perfom test data
aeTest  <- ae[-trainIndex,]
#You now need to set aside the first record from the ae_attendances_ENG_4hr_perfom test data so that your markers 
#can test and evaluate your data-capture tool.
aeTestMarker  <- aeTest[1,]
write_csv(aeTestMarker, here("Data", "ae_attendances_ENG_4hr_perfom_test_marker.csv"))
# We then need to set aside the remaining records for you to test (or collect with your) your data-capture tool.
aeTest  <- aeTest[2:nrow(aeTest),]
write_csv(aeTest, here("Data", "ae_attendances_test.csv"))


###Manipluate raw data
#group by org and filter only type 1
hosp_performance <- ae %>%
  filter(type == 1)%>%
  group_by(org_code) %>%
  summarise_at(vars(attendances, breaches), sum) %>%
  mutate(performance = 1 - breaches / attendances)
glimpse(hosp_performance)

#total performance vs time
ENG_performance <- ae %>%
  group_by(period) %>%
  summarise_at(vars(attendances, breaches), sum) %>%
  mutate(performance = 1 - breaches / attendances)
glimpse(ENG_performance)

###correlation
#p>0.05 indicates that distribution of the data are not significantly different from normal distribution
# Shapiro-Wilk normality test for attendances
shapiro.test(hosp_performance_rank$attendances) # => p = 0.03
# Shapiro-Wilk normality test for performance
shapiro.test(hosp_performance_rank$performance) # => p = 0.62

ggscatter(hosp_performance_rank, x = "attendances", y = "performance", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Attendances", ylab = "Performance")


ENG_performance <- ae_attendances %>%
  group_by(org_code, period) %>%
  # make sure that this trust has a type 1 department
  filter(any(type == 1)) %>%
  summarise_at(vars(attendances, breaches), sum) %>%
  mutate(performance = 1 - breaches / attendances)

# format for display
ENG_performance %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches), comma) %>%
  mutate_at(vars(performance), percent) %>%
  head(10) %>%
  kable()

#Create tibble of ranking hospitals 
hosp_performance_rank <- ae %>%
  filter(type == 1)%>%
  group_by(org_code) %>%
  summarise_at(vars(attendances, breaches), sum) %>%
  filter(attendances >= 500000) %>%
  mutate(performance = 1 - breaches / attendances) %>%
  arrange(performance) 

#ranking list of ranking hospitals by abbreviation
hosp_performance_rank_abbr <- hosp_performance_rank %>%
  pull(org_code) %>%
  as.character()


#graph hospitals against each other
ENG_performance %>%
  ungroup() %>%
  mutate_at(vars(org_code), fct_relevel, hosp_performance_rank_abbr) %>%
  filter(org_code %in% c(tail(hosp_performance_rank_abbr, 1),
                         head(hosp_performance_rank_abbr, 1))) %>%
  ggplot(aes(period, performance)) +
  geom_line(color = "darkcyan") +
  geom_point(color = "darkcyan") +
  scale_y_continuous(labels = percent) +
  facet_wrap(vars(org_code), nrow = 2) +
  theme(legend.position = "bottom") +
  labs(x = "Month of attendance",
       y = "% of attendances that met the 4-hour standard",
       title = "NHS England accident and emergency (A&E) four hour performance",
       subtitle = " Worst vs Best (Hospitals with >500,000 attendances)",
       caption = "Source: NHSRdatasets")

#plot performance vs org
ggplot(busy_hosp_performance, aes(x=org_code, y=performance)) + 
  geom_bar(stat = "identity")

#plot total attendance vs org
ggplot(hosp_performance, aes(x=org_code, y=attendances)) + 
  geom_bar(stat = "identity")
