# Data@ANZ Virtual Experience Program

## Data quality issues and recommendation
- Some highly confidential personal information (client first name) should be removed from the dataset.
- The dataset should consists each group of individual (gender, age) equally to avoid bias analysis.
- Empty cells should filled with some relevant value, like NA and the empty cells in merchant_state may filled with "online" to represent
the transaction is an online transfer.
- The format of the date and extraction should be consistent to avoid misunderstanding.
- The amount of data is not sufficient because it conly consists data from August to October and it may not fully reflect the actual cardusage 
pattern.

## Load library
library(tidyverse)

## Import datasets
ANZ_card = read.csv("ANZ synthesised transaction dataset.csv")

with_merchant = ANZ_card %>% 
  filter(status == "authorized")

with_bank = ANZ_card %>% 
  filter(status == "posted")

## With merchant
### Data munging
with_merchant <- with_merchant %>% 
  separate(merchant_long_lat, c("long", "lat"), sep = " ") %>% 
  mutate(status = as.factor(status),
         card_present_flag = as.factor(ifelse(card_present_flag == "0", "No", "Yes")),
         txn_description = as.factor(txn_description),
         date = as.Date(date, "%y-%m-%d"),
         gender = as.factor(ifelse(gender == "M", "Male", "Female")),
         merchant_state = as.factor(merchant_state),
         movement = as.factor(movement),
         long = as.numeric(long),
         lat = as.numeric(lat))

### Data analysis
summary(with_merchant)

with_merchant %>% 
  select(card_present_flag, merchant_state) %>% 
  table() %>% 
  addmargins()

with_merchant %>% 
  select(date, gender, amount) %>% 
  mutate(year = strftime(date, "%Y"),
         month = strftime(date, "%m")) %>% 
  group_by(year, month, gender) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  pivot_wider(id_cols = c(year, month),
              names_from = gender,
              values_from = avg_spent)

with_merchant %>% 
  select(gender, amount, merchant_state) %>% 
  group_by(gender, merchant_state) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  pivot_wider(id_cols = merchant_state,
              names_from = gender,
              values_from = avg_spent)

with_merchant %>% 
  select(age, gender, amount) %>% 
  mutate(age_group = cut(age, c(17, 21, 30, 40, 50, 60, 65, Inf))) %>% 
  group_by(age_group, gender) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  pivot_wider(id_cols = age_group,
              names_from = gender,
              values_from = avg_spent)

### Findings
- Nearly 80% of the total merchant transactions were card-present transactions.
- The number of transactions in NSW, VIC, QLD and WA are greater than other states
and territory.
- By comparing both both gender, the average spending in Auguest and September for 
female was lower than male.
- In QLD, VIC and WA, the average spending for male and female are nearly identical.
- Male have the highest average spending in ACT, while female have the highest 
average spending in SA.
- Client who age between 40 to 50 spent the most, while retired client spent the
least.
- All ANZ Bank card users use debit card for merchant transactions.

## With bank
### Data munging
with_bank <- with_bank %>% 
  separate(long_lat, c("long", "lat"), sep = " ") %>% 
  mutate(status = as.factor(status),
         txn_description = as.factor(txn_description),
         gender = as.factor(ifelse(gender == "M", "Male", "Female")),
         movement = as.factor(movement),
         long = as.numeric(long),
         lat = as.numeric(lat))

### Data analysis
summary(with_bank)

with_bank %>% 
  select(txn_description, movement) %>% 
  table()

with_bank %>% 
  group_by(txn_description) %>% 
  summarise(avg_amount = mean(amount))

with_bank %>% 
  select(date, txn_description, amount) %>% 
  mutate(year = strftime(date, "%Y"),
         month = strftime(date, "%m")) %>% 
  group_by(year, month, txn_description) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  pivot_wider(id_cols = c(year, month),
              names_from = txn_description,
              values_from = avg_spent)

with_bank %>% 
  select(age, gender, amount) %>% 
  mutate(age_group = cut(age, c(17, 21, 30, 40, 50, 60, 65, Inf))) %>% 
  group_by(age_group, gender) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  pivot_wider(id_cols = age_group,
              names_from = gender,
              values_from = avg_spent)

### Findings
- Nearly 60% of the posted transactions are from payment type transactions.
- All pay and salary type of transactions are made through credit card, while
inter bank, payment and phone bank type of transactions are completed via debit
cards.
- Pay and salary type of transactions have the highest transaction amount, while
the average transaction amount for payment type are the least.
- From August to October, the average transferred amount for each transaction
does not have significant changes.

## ANZ card transactions (with merchant and with bank)
### Data analysis
ANZ_card %>% 
  select(age, balance) %>% 
  mutate(age_group = cut(age, c(17, 20, 30, 40, 50, 60, 65, Inf))) %>% 
  group_by(age_group) %>% 
  summarise(avg_balance = mean(balance))

ANZ_card %>% 
  mutate(age_group = cut(age, c(17, 20, 30, 40, 50, 60, 65, Inf))) %>% 
  select(age_group, movement) %>% 
  group_by(age_group, movement) %>%
  tally() %>% 
  pivot_wider(id_cols = age_group,
              names_from = movement,
              values_from = n)

### Findings
- Individual who age 40 to 50 has the highest bank balance compared to other age
group, followed by group age 30 to 40 and retirees.
- Individual who age 60 to 65 has the least bank balance.
- Majority of the individuals does not own a credit card.

## Predict customer annual salary
ann_salary <- ANZ_card %>% 
  select(txn_description, gender, age, amount) %>% 
  mutate(txn_description = as.factor(txn_description),
         gender = as.factor(gender)) %>% 
  filter(txn_description == "PAY/SALARY")

set.seed(8869)
random_rows <- sample(nrow(ann_salary)*0.5, replace = FALSE)

ann_salary1 <- ann_salary[random_rows,]
ann_salary2 <- ann_salary[-random_rows,]

model_1 = lm(amount~gender+age, data = ann_salary)
summary(model_1)

ann_salary1 <- ann_salary1 %>% 
  mutate(model_1 = predict(model_1, newdata = ann_salary1))

mean((ann_salary1$model_1 - ann_salary1$amount)/ann_salary1$amount*100)

ann_salary2 <- ann_salary2 %>% 
  mutate(model_1 = predict(model_1, newdata = ann_salary2))

mean((ann_salary2$model_1 - ann_salary2$amount)/ann_salary2$amount*100)

### Findings
- Holding all other variables constant, the minimum wages that ANZ Bank's clients transferred are $2115.57, on average.
- Holding all other variables constant, the monthly salary transferred for male client is $427.94 higher than female client, on average.
- Holding all other variables constant, for every 1 unit increase in age, the monthly salary trnasferred is expected to decrease by $13.54.
- The error rate is more than 30%. The main reason is because the data points for salary payment is insufficient and more other variables need to be added to increase accuracy of prediction.
