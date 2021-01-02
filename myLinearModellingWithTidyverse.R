#https://b-rodrigues.github.io/modern_R/statistical-models.html#fitting-a-model-to-data
#tidverse model building
library(Ecdat)
data(Housing)
model3 <- lm(price ~ ., data = Housing)
summary(model3)
#You can access different elements of model3 with $, because the result of lm() is a list:
print(model3$coefficients)
#use the {broom} package, and more specifically the tidy() function, which converts model3 into a neat data.frame
results3 <- broom::tidy(model3) #a dataframe
glimpse(results3)
# keep results that are significant at the 5% level
results3 %>%
    filter(p.value < 0.05)
#add new columns for confidence intervals
results3 <- broom::tidy(model3, conf.int = TRUE, conf.level = 0.95)
print(results3)


#use lm() in a pipe workflow
Housing %>%
    select(-driveway, -stories) %>%
    lm(price ~ ., data = .) %>%
    broom::tidy()

library(ggfortify)

autoplot(model3, which = 1:6) + theme_minimal()
# get residual information
housing_aug <- broom::augment(model3)
glimpse(housing_aug)

ggplot(housing_aug) +
    geom_density(aes(.resid))

#housing units where the model underestimates the price
(total_pos <- housing_aug %>%
        filter(.resid > 0) %>%
        summarise(total = n()) %>%
        pull(total))

#extract outliers
(housing_aug %>%
        mutate(prank = cume_dist(.cooksd)) %>%
        filter(prank > 0.99) %>%
        glimpse())
#cume_dist() which is a dplyr function that returns the proportion of all values less than or equal to the current rank
example <- c(0.0, 3.0, 9.0, 5, 4.6, 2, 1, 0.8, 0, -1)
cume_dist(example)


######################################################################
a_to_f <- head(letters)
a_to_f
tail(letters)
seq_along(a_to_f)

xs <- seq_len(10)
xs
ifelse(xs %% 2 == 0, xs, NA)


library(tidyverse)
library("broom")
mtcars %>% lm(mpg ~ cyl * wt, data = .) %>%
    tidy %>% print(digits = 4)