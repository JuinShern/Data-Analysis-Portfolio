## Load package
library(tidyverse)
library(GGally)

## Import data
data = read.csv("StudentsPerformance.csv") %>% 
  mutate(gender = as.factor(gender),
         race.ethnicity = as.factor(race.ethnicity),
         parental.level.of.education = as.factor(parental.level.of.education),
         lunch = as.factor(lunch),
         test.preparation.course = as.factor(test.preparation.course),
         avg_score = (math.score + reading.score + writing.score)/3)

## Data exploratory
summary(data)

data %>% 
  select(gender, math.score, reading.score, writing.score) %>% 
  group_by(gender) %>% 
  summarise_all(mean)

data %>% 
  select(race.ethnicity, math.score, reading.score, writing.score) %>% 
  group_by(race.ethnicity) %>% 
  summarise_all(mean)

data %>% 
  select(parental.level.of.education, math.score, reading.score, writing.score) %>% 
  group_by(parental.level.of.education) %>% 
  summarise_all(mean)

data %>% 
  select(lunch, math.score, reading.score, writing.score) %>% 
  group_by(lunch) %>% 
  summarise_all(mean)

data %>% 
  select(test.preparation.course, math.score, reading.score, writing.score) %>% 
  group_by(test.preparation.course) %>% 
  summarise_all(mean)

### Findings
- Overall, all students are performing well in reading test and 50% of the students
have archieved 70% and above with average score of 69.17%.
- On average, male students perform slightly better than female students in mathematics,
while female students are doing better than male students in reading and writing.
- On average, students who classified as group E perform better than other race groups.
- On average, if the student's parents have master's degrees, their performance 
on the test is higher than other students.
- Students who take standard lunch on average are perform better than those who take
free or reduced lunch.
- Students who completed test prepration course on average are perform well in 
tests.

## Data visualisation
data %>% 
  select(math.score, reading.score, writing.score) %>% 
  ggcorr(label = TRUE)

ggplot(data = data) +
  geom_histogram(aes(x = math.score), bins = 20) +
  labs(title = "Math score distribution", x = "Math score", y = "Frequency")

ggplot(data = data) +
  geom_histogram(aes(x = reading.score), bins = 20) +
  labs(title = "Reading score distribution", x = "Reading score", y = "Frequency")

ggplot(data = data) +
  geom_histogram(aes(x = writing.score), bins = 20) +
  labs(title = "Writing score distribution", x = "Writing score", y = "Frequency")

### Findings
- Reading test and writing test has strong positive correlation.

## Test score prediction
set.seed(1997)
random_sample = sample(nrow(data)*0.5, replace = FALSE)
test_data = data[random_sample,]
train_data = data[-random_sample,]

model_1 = lm(data = train_data, math.score~gender+race.ethnicity +parental.level.of.education+
               lunch+test.preparation.course)
summary(model_1)

model_2 = lm(data = train_data, reading.score~gender+race.ethnicity +parental.level.of.education+
               lunch+test.preparation.course)
summary(model_2)

model_3 = lm(data = train_data, writing.score~gender+race.ethnicity +parental.level.of.education+
               lunch+test.preparation.course)
summary(model_3)

test_data <- test_data %>% 
  mutate(pre_math = round(predict(model_1, newdata = test_data),0), 
         pre_reading = round(predict(model_2, newdata = test_data),0),
         pre_writing = round(predict(model_3, newdata = test_data),0),
         pre_avg_score = round((pre_math+pre_reading+pre_writing)/3,0))

### Error rate
(mean(test_data$pre_math) - mean(test_data$math.score))/ mean(test_data$math.score)*100
(mean(test_data$pre_reading) - mean(test_data$reading.score))/ mean(test_data$reading.score)*100
(mean(test_data$pre_writing) - mean(test_data$writing.score))/ mean(test_data$writing.score)*100