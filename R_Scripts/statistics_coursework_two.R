library("tidyverse")
library("afex")
library("emmeans")
theme_set(theme_bw(base_size = 15) + theme(legend.position="bottom"))

###load and view data
stereotype <- read_csv("stereotype-threat.csv")
glimpse(stereotype)

###transform categorical variables into factors
stereotype <- stereotype %>%
  mutate(
    ID = factor(subject_id),
    treatment = factor(treatment,
                       levels = c("control", "stereotype")),
    gender = factor(gender,
                    levels = c("male","female")),
    )
glimpse(stereotype)


###overview of the data
stereotype %>%
  group_by(treatment, gender) %>%
  count()

###check age in each group
stereotype %>%
  group_by(treatment, gender) %>%
  summarise(
    m_age = mean(age),
    sd_age = sd(age),
    min_age = min(age),
    max_age = max(age)
  )


##ANOVA analysis for math test
a1 <- aov_car(math_score~treatment*gender + Error(ID),
              stereotype)


p1 <- afex_plot(a1, "gender", "treatment",
          data_geom = ggbeeswarm::geom_quasirandom,
          data_arg = list(
            dodge.width = 0.5,
            color = "darkgrey"),
          legend_title = "Condition") +
  labs (x = "Gender", y = "Math Score")

###Follow-up analysis
em2 <- emmeans(a1, c("treatment", "gender"))
em2

conlist2 <- list(
  eff_male = c(1, -1, 0, 0),
  eff_female = c(0, 0, 1, -1)
)

contrast(em2, conlist2, adjust = "holm")

###ANOVA analysis for verbal test
a2 <- aov_car(verbal_score~treatment*gender + Error(ID),
              stereotype)
a2
p2 <- afex_plot(a2, "gender", "treatment",
          data_geom = ggbeeswarm::geom_quasirandom,
          data_arg = list(
            dodge.width = 0.5,
            color = "darkgrey"),
          legend_title = "Condition") +
  labs (x = "Gender", y = "Verbal Score")

###plot two panels together
cowplot::plot_grid(p1, p2)
