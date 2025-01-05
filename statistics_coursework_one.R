library("tidyverse")
library("afex")
library("emmeans")
theme_set(theme_bw(base_size = 15) + theme(legend.position="bottom"))

###load and view data
recommend <- read_csv("recommendations.csv")
glimpse(recommend)

###transform categorical variables into factors
recommend <- recommend %>%
  mutate(
    ID = factor(ResponseID),
    Mode = factor(
      Mode,
      levels = c("PermanentVisual", "DisappearingVisual", "Auditory")),
    gender = factor(gender, levels = c(1, 2, 3), labels = c("male", "female", "other")),
    Stimulus = factor(Stimulus, levels = c("brownies", "hotel", "shampoo", "toaster"))
  )


###overview of the data
recommend %>%
  group_by(ID, Mode, Stimulus) %>%
  count()

###check everyone is on one mode and did all four conditions
recommend %>%
  group_by(ID) %>%
  summarise(
    n_Mode = n_distinct(Mode),
    n_Stimulus = n_distinct(Stimulus)
  ) %>%
  summarise(
    Mode = all(n_Mode == 1),
    Stimulus = all(n_Stimulus == 4)
  )


###check the participants numbers in each group
recommend %>%
  group_by (Mode, Stimulus) %>%
  summarise(n = n_distinct(ID))

###check age in each group
recommend %>%
  group_by (Mode, Stimulus) %>%
  summarise(
    m_age = mean(age),
    sd_age = sd(age),
    min_age = min(age),
    max_age = max(age)
  )

###check if the groups are counterbalanced
recommend %>%
  group_by (Mode, gender) %>%
  count()

###ANOVA analysis
res1 <- aov_ez("ID", "RecommendationFollowed", recommend,
               between = "Mode",
               within = "Stimulus")
res1

###plot the results
afex_plot(res1, "Stimulus", "Mode",
          data_geom = ggbeeswarm::geom_quasirandom,
          data_arg = list(
            dodge.width = 0.5,
            color = "darkgrey"), lengend_title = "recommendation")+
  labs(x = "Stimulus Object", y = "prop. following recommendation")

###follow-up contrasts
em1 <- emmeans(res1, "Mode")
em1
conlist1 <- list(
  PV_DV = c(1, -1, 0),
  DV_AU = c(0, 1, -1),
  PV_AU = c(1, 0, -1),
  Visual_Audi = c(0.5, 0.5, -1)
)

contrast(em1, conlist1, adjust = "holm")

emmeans(res1, "Stimulus") %>%
  pairs(adjust = "holm")
