#### Load packages ####

library(readr)
library(tidyverse)
library(brms)
library(tidybayes)
library(extrafont)
library(hrbrthemes)

#### Load data ####
data_drm_long <- read_csv("data/data_drm_long.csv")

#### Model for plots (categorical IV) ####

fit_drm <- brm (
  formula = resp ~ condition * item_type + (1 | ppt) + (1 | item),
  data = data_drm_long,
  warmup = 2000,
  iter = 4000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.8, max_treedepth = 10),
  inits = 0,
  family = cumulative (link = "probit", threshold = "flexible")
)

summary(fit_drm)

#### Model with contrast of interest (numerical IV) ####

fit_drm_rc_1 <- brm (
  formula = resp ~ cond_rc * item_type_rc_1 + (1 | ppt) + (1 | item),
  data = data_drm_long,
  warmup = 2000,
  iter = 4000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.8, max_treedepth = 10),
  inits = 0,
  family = cumulative (link = "probit", threshold = "flexible")
)

summary(fit_drm_rc_1)

#### Model with orthogonal contrast ####

fit_drm_rc_2 <- brm (
  formula = resp ~ cond_rc * item_type_rc_2 + (1 | ppt) + (1 | item),
  data = data_drm_long,
  warmup = 2000,
  iter = 4000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.8, max_treedepth = 10),
  inits = 0,
  family = cumulative (link = "probit", threshold = "flexible")
)

summary(fit_drm_rc_2)

##### Get estimates from the (categorical) model #####

# set conditions for the "condition" predictor
cond_Control <- data.frame(condition = "Control",
                                cond__ = "Control")

cond_Imagery <- data.frame(condition = "Imagery",
                               cond__ = "Imagery")

# get estimates of the probability of each response in each combination of the two predictors

conditional_Control <- conditional_effects(
  effects = "item_type",
  conditions = cond_Control,
  fit_drm,
  re_formula = NULL,
  robust = TRUE,
  categorical = TRUE,
  probs = c(0.025, 0.975),
  plot = FALSE
)[[1]]

conditional_Imagery <- conditional_effects(
  effects = "item_type",
  conditions = cond_Imagery,
  fit_drm,
  re_formula = NULL,
  robust = TRUE,
  categorical = TRUE,
  probs = c(0.025, 0.975),
  plot = FALSE
)[[1]]

conditional_merged <-
  rbind(conditional_Control, conditional_Imagery)

# Compute overall response estimates (for the dashed lines on the plot)

overall_list <- conditional_merged %>%
  mutate(overall_resp = as.numeric(effect2__) * estimate__)

overall_sum <-
  aggregate(data = overall_list, overall_resp ~ condition * item_type, sum)

#### Starting plot ####

# Get font
hrbrthemes::import_roboto_condensed()
loadfonts()

# Order levels
level_order <- c('Unstudied', 'Lure', 'Studied')

#### Plot raw data ####
plot_drm <-
  ggplot(data_drm_long, aes(
    x = factor(item_type, level = level_order),
    y = resp,
    fill = condition
  )) +
  geom_point(
    data = data_drm_long,
    aes(
      x = factor(item_type, level = level_order),
      y = resp,
      colour = condition,
      fill = condition
    ),
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .2,
    shape = 20,
    inherit.aes = FALSE
  )

#### Prepare secondary axes ####
plot_drm <- plot_drm + geom_segment(
  data = conditional_merged,
  aes(
    x = item_type,
    xend = c(
      1.95,
      2.95,
      0.95,
      1.95,
      2.95,
      0.95,
      1.95,
      2.95,
      0.95,
      1.95,
      2.95,
      0.95,
      2.05,
      3.05,
      1.05,
      2.05,
      3.05,
      1.05,
      2.05,
      3.05,
      1.05,
      2.05,
      3.05,
      1.05
    ),
    y = as.numeric(effect2__) + 0.2,
    yend = as.numeric(effect2__) + 0.5,
    linetype = "1"
  ),
  color = "gray20",
  size = 0.3,
  position = position_dodge(width = .2),
  lineend = "round",
  show.legend = FALSE
)

#### Plot estimates and CI ####
plot_drm <- plot_drm + geom_segment(
  data = conditional_merged,
  aes(
    x = item_type,
    xend = c(
      1.95,
      2.95,
      0.95,
      1.95,
      2.95,
      0.95,
      1.95,
      2.95,
      0.95,
      1.95,
      2.95,
      0.95,
      2.05,
      3.05,
      1.05,
      2.05,
      3.05,
      1.05,
      2.05,
      3.05,
      1.05,
      2.05,
      3.05,
      1.05
    ),
    colour = condition,
    y = as.numeric(effect2__) + 0.2 + lower__ * 0.3,
    yend = as.numeric(effect2__) + 0.2 + upper__ * 0.3,
    linetype = "1"
  ),
  size = 2,
  position = position_dodge(width = .2),
  lineend = "round",
  show.legend = FALSE
) + geom_point(
  data = conditional_merged,
  aes(
    x = item_type,
    y =  as.numeric(effect2__) + 0.2 + estimate__ * 0.3,
    colour = condition,
  ),
  size = 4,
  position = position_dodge(width = .2),
  show.legend = FALSE
)

#### Dashed lines ####
plot_drm <- plot_drm + geom_line(
  data = overall_sum,
  aes(
    x = factor(item_type, level = level_order),
    y = overall_resp,
    group = condition,
    colour = condition
  ),
  linetype = 3,
  size = 0.5
)

#### Labels ####
plot_drm <- plot_drm + annotate(
  "text",
  x = 0.4,
  y = 1.07,
  hjust = 0,
  color = "gray40",
  label = "Sure it is new",
  size = 7
) +
  annotate(
    "text",
    x = 0.4,
    y = 2.07,
    hjust = 0,
    color = "gray40",
    label = "Probably new",
    size = 7
  ) +
  annotate(
    "text",
    x = 0.4,
    y = 3.07,
    hjust = 0,
    color = "gray40",
    label = "Probably studied",
    size = 7
  ) +
  annotate(
    "text",
    x = 0.4,
    y = 4.17,
    hjust = 0,
    color = "gray40",
    label = "Sure that the \nitem was studied",
    size = 7
  )

#### Custom ####
plot_drm <- plot_drm +
  scale_colour_brewer(palette = "Dark2",
                      name = "Experimental \ncondition") +
  scale_fill_brewer(palette = "Dark2",
                    name = "Experimental \ncondition") +
  theme_minimal() +
  scale_x_discrete(name = "Item type") +
  scale_y_continuous(name = "Responses", ) +
  theme_ipsum_rc(
    base_size = 25,
    subtitle_size = 25,
    axis_title_size = 30
  ) +
  guides(alpha = FALSE,
         colour = guide_legend(override.aes = list(shape = 15, size = 7))) +
  scale_alpha_continuous(range = c(0.2, 1)) +
  theme(text = element_text(size = 30))

plot_drm

