#install.packages("babynames")

library(babynames)
library(ggplot2)    # the king of plotting 
library(magrittr)   # chain operators, e.g. to "pipe" a value forward
library(dplyr)      # for data manipulation 

# Check data
str(babynames)
head(babynames)
tail(babynames)

# Unique baby names
length(unique(babynames$name))

# How many kids for n>5 per name and year
sum(babynames$n)/10^6

# Plot single name
ggplot(babynames, aes(year, n)) +
  geom_line(data = filter(babynames, name=="Daron"))

head(filter(babynames, name=="Daron"))

# Plot single name over time
ggplot(babynames, aes(year, n)) +
  geom_line(data = filter(babynames, name=="Darren"), aes(color=sex))

# Try to follow this code chunk at home, using dplyr() and magrittr()
top10 <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  group_by(sex) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10)  %>%
  arrange(sex, rank)

top10f <- top10 %>% filter(sex=="F")
top10m <- top10 %>% filter(sex=="M")


top10_2013 <- babynames %>%
  filter(year == 2013) %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  group_by(sex) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  arrange(sex, rank)

top10f_2013 <- top10 %>% filter(sex=="F")
top10m_2013 <- top10 %>% filter(sex=="M")

babynames %>%
  filter(sex=="F") %>%
  filter(name %in% top10f_2013$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))

babynames %>%
  filter(sex=="M") %>%
  filter(name %in% top10m_2013$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))
