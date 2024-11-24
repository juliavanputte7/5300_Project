install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("stringr")
# Load Packages
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(broom)
library(tidyr)

# Read in the data
data = read_csv("/Users/albertomacia/downloads/COMBINED_With_Ground_Speed_CLEANED.csv") %>%
  setNames(nm = stringr::str_replace_all(tolower(names(.)), " ", ""))


# Group the data by every second
data = data %>%
  mutate(
    #floor to group by whole seconds
    second = round(time,1)
  )  %>%
  group_by(logdate, second) %>%
  summarize(
    across(where(is.numeric), mean, na.rm = TRUE),         # Mean for numeric columns
    across(where(is.factor), ~ first(.), .names = "{.col}") # Use the first value for categorical columns
  ) %>%
  ungroup()
select(-second)  # Drop the helper 'second' column if not needed

write.csv(data,"/Users/albertomacia/downloads/COMBINED_With_Ground_Speed_CLEANED_1Decimal.csv")