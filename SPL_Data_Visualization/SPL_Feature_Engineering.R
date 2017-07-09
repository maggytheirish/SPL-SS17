# This script creates new features and splits the dataset for training and testing
train = readRDS("train")  # Load the clean dataset

train$NewDate <- train$Date  #
```

```{r}
# Creating a starting date for Promo2
train$Promo2SinceDate = as.Date(with(train, paste(Promo2SinceYear,Promo2SinceWeek,1, sep="-")),"%Y-%U-%u")
train$Promo2SinceWeek = NULL  # deleting redundant columns
train$Promo2SinceYear = NULL
```

```{r}
# Creating a starting date for Competition (assuming it's from the first day of the month)
train$CompetitionSinceDate = as.Date(paste(train$CompetitionOpenSinceYear, train$CompetitionOpenSinceMonth, 1, sep="-"),format="%Y-%m-%d")
train$CompetitionOpenSinceYear = NULL  # deleting redundant columns
train$CompetitionOpenSinceMonth = NULL
```

```{r}
# Replacing NAs in CompetitionSinceDate
CompetitionDateMissing = train[is.na(train$CompetitionSinceDate), ]
CompetitionDateMissing = CompetitionDateMissing[!is.na(CompetitionDateMissing$CompetitionDistance), ]
CompetitionDateMissing$CompetitionSinceDate = CompetitionDateMissing$Date  # replacing NAs with the current date
train[is.na(train$CompetitionSinceDate) & !is.na(train$CompetitionDistance),"CompetitionSinceDate"]=CompetitionDateMissing$CompetitionSinceDate
train$CompetitionSinceDate = as.Date(train$CompetitionSinceDate)
```
```{r}
# Create a new binary variable StoreAssortmentMatch
train$Assortment = as.character(train$Assortment)  # StoreType and Assortment have different levels
train$StoreAssortmentMatch = ifelse(train$StoreType==train$Assortment,1,0)
train$Assortment = as.factor(train$Assortment)
train$StoreAssortmentMatch = as.factor(train$StoreAssortmentMatch)
```

```{r}
# Create a dummy variable for whether competition is present on a given day
train$Date = as.Date(train$Date)
train$CompetitionPresent = ifelse(train$Date<train$CompetitionSinceDate,0,1)
train$CompetitionPresent = as.factor(train$CompetitionPresent)
train$CompetitionPresent = factor(train$CompetitionPresent, levels=c(levels(train$CompetitionPresent), "missing"))
form_na = is.na(train$CompetitionPresent)
train[form_na,"CompetitionPresent"] = "missing"
```

```{r}
# Create a new binary variable indicating if both types of holidays take place on a given day
train$StateHoliday = as.character(train$StateHoliday)
train$BothHolidays = ifelse(train$StateHoliday==train$SchoolHoliday,1,0)
train$StateHoliday = as.factor(train$StateHoliday)
train$BothHolidays = as.factor(train$BothHolidays)
```

```{r}
# Replace NAs in newly created CompetitionDistance, CompetitionSinceDate, Promo2SinceDate
train$CompetitionDistance[is.na(train$CompetitionDistance)] = mean(train$CompetitionDistance, na.rm = TRUE)
train$CompetitionSinceDate[is.na(train$CompetitionSinceDate)] = mean(train$CompetitionSinceDate, na.rm = TRUE)
train$Promo2SinceDate[is.na(train$Promo2SinceDate)] = mean(train$Promo2SinceDate, na.rm = TRUE)
summary(train)   
```
