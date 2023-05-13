library(dplyr)
library(stringr)


# import
df_1 <- read.csv("tech_mental_health_2016.csv")
df_2 <- read.csv("Tech_mental_health_2017.csv")
df_global <-
  read.csv("Mental health Depression disorder Data 2.csv")

# read
us_raw_data <-
  df_global[(df_global$Entity == "United States") &
              (df_global$Year == "2016" |
                 df_global$Year == "2017"), ]

# clean us_raw_data
us_data <- us_raw_data %>%
  filter(index == 6103 | index == 6102)

head(us_data)


# create a new variable or a new column

df_1 <-
  df_1 %>% filter(Is.your.employer.primarily.a.tech.company.organization. == "1")
tech_2016_filtered_mh <-
  sum(df_1$Do.you.currently.have.a.mental.health.disorder. == 'Yes')
df_2 <-
  df_2 %>% filter(Is.your.employer.primarily.a.tech.company.organization. == "1")
tech_2017_filtered_mh <-
  sum(df_2$Do.you.currently.have.a.mental.health.disorder. == 'Yes')

# delete column of us_data

us_data <- us_data[, c(-1, -2,-9, -11)]


#calculate the number of people having each type of mental health disorder
num_anxiety_disorder_2016 <-
  df_1[grepl("Anxiety",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()
num_bipolar_disorder_2016 <-
  df_1[grepl("Bipolar",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()
num_ADHD_disorder_2016 <-
  df_1[grepl(
    "Attention Deficit",
    df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.
  ),] %>% nrow()
num_depression_disorder_2016 <-
  df_1[grepl("Depression",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()
num_schizophrenia_disorder_2016 <-
  df_1[grepl("Schizophrenia",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()
num_eating_disorder_2016 <-
  df_1[grepl("Eating",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()
num_PTSD_disorder_2016 <-
  df_1[grepl("Post-traumatic",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()
num_OCD_disorder_2016 <-
  df_1[grepl("Obsessive",
             df_1$If.yes..what.condition.s..have.you.been.diagnosed.with.),] %>% nrow()


bipolar_tech_2016_per <-
  (num_bipolar_disorder_2016 / nrow(df_1) * 100)
anxiety_tech_2016_per <-
  (num_anxiety_disorder_2016 / nrow(df_1) * 100)
ADHD_tech_2016_per <- (num_ADHD_disorder_2016 / nrow(df_1) * 100)
depression_tech_2016_per <-
  (num_depression_disorder_2016 / nrow(df_1) * 100)
schizophrenia_tech_2016_per <-
  (num_schizophrenia_disorder_2016 / nrow(df_1) * 100)
eating_tech_2016_per <-
  (num_eating_disorder_2016 / nrow(df_1) * 100)
PTSD_tech_2016_per <- (num_PTSD_disorder_2016 / nrow(df_1) * 100)
OCD_tech_2016_per <- (num_OCD_disorder_2016 / nrow(df_1) * 100)


us_data <-
  rbind(
    us_data,
    c(
      "Tech Company",
      "2016",
      schizophrenia_tech_2016_per,
      bipolar_tech_2016_per,
      eating_tech_2016_per,
      anxiety_tech_2016_per,
      depression_tech_2016_per
    )
  )


# add data from tech_2017


num_anxiety_disorder_2017 <-
  df_2[grepl("Anxiety",
             df_2$Anxiety.Disorder..Generalized..Social..Phobia..etc..1),] %>% nrow() + df_2[grepl("Anxiety",
                                                                                                   df_2$Anxiety.Disorder..Generalized..Social..Phobia..etc..2),] %>% nrow()
num_bipolar_disorder_2017 <-
  df_2[grepl("Bipolar",
             df_2$Mood.Disorder..Depression..Bipolar.Disorder..etc..1),] %>% nrow() + df_2[grepl("Bipolar",
                                                                                                 df_2$Mood.Disorder..Depression..Bipolar.Disorder..etc..2),] %>% nrow()
num_ADHD_disorder_2017 <-
  df_2[grepl("Attention",
             df_2$Attention.Deficit.Hyperactivity.Disorder.1),] %>% nrow() + df_2[grepl("Attention",
                                                                                        df_2$Attention.Deficit.Hyperactivity.Disorder.2),] %>% nrow()
num_depression_disorder_2017 <-
  df_2[grepl("Depression",
             df_2$Mood.Disorder..Depression..Bipolar.Disorder..etc..1),] %>% nrow() + df_2[grepl("Depression",
                                                                                                 df_2$Mood.Disorder..Depression..Bipolar.Disorder..etc..2),] %>% nrow()

num_schizophrenia_disorder_2017 <-
  df_2[grepl(
    "Schizophrenia",
    df_2$Psychotic.Disorder..Schizophrenia..Schizoaffective..etc..1
  ),] %>% nrow() + df_2[grepl(
    "Schizophrenia",
    df_2$Psychotic.Disorder..Schizophrenia..Schizoaffective..etc..2
  ),] %>% nrow()

num_eating_disorder_2017 <-
  df_2[grepl("Eating",
             df_2$Eating.Disorder..Anorexia..Bulimia..etc..1),] %>% nrow() + df_2[grepl("Attention",
                                                                                        df_2$Eating.Disorder..Anorexia..Bulimia..etc..2),] %>% nrow()


bipolar_tech_2017_per <-
  (num_bipolar_disorder_2017 / nrow(df_2) * 100)
anxiety_tech_2017_per <-
  (num_anxiety_disorder_2017 / nrow(df_2) * 100)
depression_tech_2017_per <-
  (num_depression_disorder_2017 / nrow(df_2) * 100)
schizophrenia_tech_2017_per <-
  (num_schizophrenia_disorder_2017 / nrow(df_2) * 100)
eating_tech_2017_per <-
  (num_eating_disorder_2017 / nrow(df_2) * 100)


us_data <-
  rbind(
    us_data,
    c(
      "Tech Company",
      "2017",
      schizophrenia_tech_2017_per,
      bipolar_tech_2017_per,
      eating_tech_2017_per,
      anxiety_tech_2017_per,
      depression_tech_2017_per
    )
  )



