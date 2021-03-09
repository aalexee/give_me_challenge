# Packages ----------------------------------------------------------------


library(tidyverse)
library(magrittr)
library(broom)

library(ggpubr)
library(ggsci)

library(plm)
library(lmtest)
library(sandwich)
library(margins)
library(ggeffects)
library(fixest)

library(lubridate)

library(stargazer)


# Functions ---------------------------------------------------------------


# rounding of p-values
round_p <- function(x) {
  res <- round(x, 3)
  if (res < 0.001) {
    res <- "$< 0.001$"
  } else
    res <- paste("=", res)
  return(res)
}


# Globals -----------------------------------------------------------------


# load graph settings
source("./graph_settings.R")

# substitutions for variable names
var_names <-
  tibble(
    Var = c("z", "w", "k", "theta", "I(theta^2)")
    , Variable = c("Bonus", "Wage", "Cost", "Difficulty", "Difficulty2")
  )


# Load Data ---------------------------------------------------------------


# read filenames of data files
filenames <-
  list.files(path = "../data",
             pattern = "\\.csv$",
             full.names = T)
filenames.short <-
  list.files(path = "../data",
             pattern = "\\.csv$",
             full.names = F)
filenames.short <- gsub(".csv", "", filenames.short)

for (i in 1:length(filenames)) {
  assign(paste0(filenames.short[i]), read_csv(filenames[i]))
}

# remove redundant objects
rm(filenames, filenames.short, i)



# Table A.1 -------------------------------------------------

treatments <- data_effort %>%
  select(treatment_id, theta, w, z, k) %>%
  group_by(treatment_id) %>%
  summarise_at(vars(-group_cols()), mean)

data_effort %>%
  group_by(treatment_id, session) %>%
  summarize(mean_effort = mean(effort)) %>%
  spread(session, mean_effort) %>%
  ungroup() %>%
  mutate(mean_effort = rowMeans(select(., -treatment_id), na.rm = T)) %>%
  mutate(mean_effort = round(mean_effort, 2)) %>%
  mutate_at(vars(-treatment_id, -mean_effort), function(x) (!is.na(x))*1) %>%
  left_join(treatments) %>%
  select(treatment_id, theta, w, z, k, everything()) %>%
  rename("id" = "treatment_id") %>%
  rename_at(vars(contains("pm")), function(x) str_replace(x, "_.{0,3}pm", "")) %>%
  rename_at(vars(contains("_")), function(x) str_replace_all(x, "_", "/"))


# Figures 2, D.1, D.2 -----------------------------------------------------


# > Bonus --------------------------------


data_effort %>%
  mutate_at(
    vars(matches("theta|z|w|k")),
    as.factor
  ) %>%
  group_by(id, z) %>%
  summarise(effort = mean(effort)) %>%
  ungroup() %>%
  mutate(sd = sd(effort))  %T>%
  {{ggplot(., aes(x = effort, color = z)) +
      stat_ecdf(size = graphs$linesize) +
      geom_segment(
        aes(x = median(effort[z == 2]), xend = median(effort[z == 4]), y = 0.5, yend = 0.5)
        , arrow = arrow(length = unit(0.03, "npc"), type = "open")
        , color = "black"
      ) +
      labs(
        x = "Effort",
        y = NULL
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 0),
        legend.position = c(0.1, 0.5),
        legend.direction = "vertical",
        legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print(.)
  } %T>% # plot means
  {{group_by(., z) %>%
      do( # test that choice proportions are different from 50%
        tidy(
          t.test(x = .data$effort)
        )
      ) %>%
      ggplot(., aes(x = z, y = estimate, fill = z)) +
      geom_bar(stat = "identity", width = 0.25) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , width = 0.05
        , color = "grey"
        , size = graphs$linesize
      ) +
      labs(
        x = NULL,
        y = NULL
      ) +
      ylim(c(0,1)) +
      theme(legend.position = "none")
  } %>%
      print(.)
  } %T>% # plot histograms
  {{ggplot(., aes(x = effort, y = 2*..count../sum(..count..), fill = z, group = z)) +
      geom_histogram(binwidth = 0.1, position = position_dodge(), color = "white") +
      labs(
        x = "Effort",
        y = NULL
      ) +
      ylim(c(0,0.3)) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 1),
        legend.position = c(-0.05, 1.1),
        legend.direction = "vertical"
        , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print(.)
  }

# > Wage --------------------------------------------------------------------


data_effort %>%
  mutate_at(
    vars(matches("theta|z|w|k")),
    as.factor
  ) %>%
  filter(k == 1) %>%
  group_by(id, w) %>%
  summarise(effort = mean(effort)) %T>%
  {{ggplot(., aes(x = effort, color = w)) +
      stat_ecdf(size = graphs$linesize) +
      labs(
        x = "Effort",
        y = NULL
      ) +
      geom_segment(
        aes(x = median(effort[w == 1]), xend = median(effort[w == 2]), y = 0.5, yend = 0.5)
        , arrow = arrow(length = unit(0.03, "npc"), type = "open")
        , color = "black"
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 0),
        legend.position = c(0.1, 0.5),
        legend.direction = "vertical",
        legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print(.)
  } %T>% # plot means
  {{group_by(., w) %>%
      do(
        tidy(
          t.test(x = .data$effort)
        )
      ) %>%
      ggplot(., aes(x = w, y = estimate, fill = w)) +
      geom_bar(stat = "identity", width = 0.25) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , width = 0.05
        , color = "grey"
        , size = graphs$linesize
      ) +
      labs(
        x = NULL,
        y = NULL
      ) +
      ylim(c(0,1)) +
      theme(legend.position = "none")
  } %>%
      print(.)
  } %>% # plot histograms
  {{ggplot(., aes(x = effort, y = 2*..count../sum(..count..), fill = w, group = w)) +
      geom_histogram(binwidth = 0.1, position = position_dodge(), color = "white") +
      labs(
        x = "Effort",
        y = NULL
      ) +
      ylim(c(0,0.3)) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 1),
        legend.position = c(-0.05, 1.1),
        legend.direction = "vertical"
        , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print(.)
  }


# > Cost --------------------------------------------------------------------


data_effort %>%
  mutate_at(
    vars(matches("theta|z|w|k")),
    as.factor
  ) %>%
  filter(w == 2) %>%
  group_by(id, k) %>%
  summarise(effort = mean(effort)) %T>%
  {{ggplot(., aes(x = effort, color = k)) +
      stat_ecdf(size = graphs$linesize) +
      labs(
        x = "Effort",
        y = NULL
      ) +
      geom_segment(
        aes(x = median(effort[k == 1]), xend = median(effort[k == 2]), y = 0.5, yend = 0.5)
        , arrow = arrow(length = unit(0.03, "npc"), type = "open")
        , color = "black"
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 0),
        legend.position = c(0.1, 0.5),
        legend.direction = "vertical",
        legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print()
  } %T>% # plot means
  {{group_by(., k) %>%
      do(
        tidy(
          t.test(x = .data$effort)
        )
      )  %>%
      ggplot(., aes(x = k, y = estimate, fill = k)) +
      geom_bar(stat = "identity", width = 0.25) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , width = 0.05
        , color = "grey"
        , size = graphs$linesize
      ) +
      labs(
        x = NULL,
        y = NULL
      ) +
      ylim(c(0,1)) +
      theme(legend.position = "none")
  } %>%
      print(.)
  } %>% # plot histograms
  {{ggplot(., aes(x = effort, y = 2*..count../sum(..count..), fill = k, group = k)) +
      geom_histogram(binwidth = 0.1, position = position_dodge(), color = "white") +
      labs(
        x = "Effort",
        y = NULL
      ) +
      ylim(c(0,0.3)) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 1),
        legend.position = c(-0.05, 1.1),
        legend.direction = "vertical"
        , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print(.)
  }


# > Difficulty --------------------------------------------------------------------


data_effort %>%
  mutate_at(
    vars(matches("theta|z|w|k")),
    as.factor
  ) %>%
  group_by(id, theta) %>%
  summarise(effort = mean(effort)) %T>%
  {{filter(., theta %in% c(0, 0.5, 1)) %>%
      ggplot(., aes(x = effort, color = theta)) +
      stat_ecdf(size = graphs$linesize) +
      geom_segment(
        aes(x = median(effort[theta == 0]), xend = median(effort[theta == 0.5]), y = 0.5, yend = 0.5)
        , arrow = arrow(length = unit(0.03, "npc"), type = "open")
        , color = "black"
      ) +
      geom_segment(
        aes(x = quantile(effort[theta == 0.5], 0.45), xend = quantile(effort[theta == 1], 0.45), y = 0.45, yend = 0.45)
        , arrow = arrow(length = unit(0.03, "npc"), type = "open")
        , color = "black"
      ) +
      labs(
        x = "Effort",
        y = NULL
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
        legend.justification = c(0, 0),
        legend.position = c(0.1, 0.5),
        legend.direction = "vertical",
        legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
      print(.)
  } %T>% # plot means
  {{group_by(., theta) %>%
      do(
        tidy(
          t.test(x = .data$effort)
        )
      ) %>%
      ggplot(., aes(x = theta, y = estimate, fill = theta)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , width = 0.1
        , color = "grey"
        , size = graphs$linesize
      ) +
      labs(
        x = NULL,
        y = NULL
      ) +
      ylim(c(0,1)) +
      theme(legend.position = "none")
  } %>%
      print(.)
  } %T>% # plot histograms
  {filter(., theta %in% c(0,0.5,1)) %>%
      {ggplot(., aes(x = effort, y = 3*..count../sum(..count..), fill = theta, group = theta)) +
          geom_histogram(binwidth = 0.1, position = position_dodge(), color = "white") +
          labs(
            x = "Effort",
            y = NULL
          ) +
          ylim(c(0,0.3)) +
          theme(
            legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize),
            legend.justification = c(0, 1),
            legend.position = c(-0.05, 1.1),
            legend.direction = "vertical"
            , legend.box.margin = margin(c(10,10,10,10))
          )
      } %>%
      print(.)
  }



# Figure 3 --------------------------------------------------------------

rbind(
  {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      group_by(id, z) %>%
      summarise(effort = mean(effort)) %>%
      t.test(effort ~ z, data = ., paired = T) %>%
      tidy(.) %>%
      mutate(var = "Bonus")
  },
  {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(k == 1) %>%
      group_by(id, w) %>%
      summarise(effort = mean(effort)) %>%
      t.test(effort ~ w, data = ., paired = T) %>%
      tidy(.) %>%
      mutate(var = "Wage")
  },
  {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(w == 2) %>%
      group_by(id, k) %>%
      summarise(effort = mean(effort)) %>%
      t.test(effort ~ k, data = ., paired = T) %>%
      tidy(.) %>%
      mutate(var = "Cost")
  },
  {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(theta %in% c(0, 0.5)) %>%
      group_by(id, theta) %>%
      summarise(effort = mean(effort)) %>%
      t.test(effort ~ theta, data = ., paired = T) %>%
      tidy(.) %>%
      mutate(var = "Difficulty 1")
  },
  {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(theta %in% c(1, 0.5)) %>%
      group_by(id, theta) %>%
      summarise(effort = mean(effort)) %>%
      t.test(effort ~ theta, data = ., paired = T) %>%
      tidy(.) %>%
      mutate(var = "Difficulty 2")
  }
) %>%
  mutate_at(vars(c("estimate", "conf.low", "conf.high")), multiply_by, -1) %>%
  mutate(var = factor(var)) %>%
  {ggplot(., aes(x = estimate, y = fct_reorder(var, desc(estimate)))) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        size = graphs$linesize,
        height = 0.1,
        color = "grey"
      ) +
      geom_point(aes(color = (estimate >= 0)), size = 2*graphs$linesize)  +
      geom_text(
        stat = "identity",
        aes(label = paste0(round(estimate, 2))),
        # position = position_dodge2(width = 0.5),
        vjust = -1,
        size = 9*5/14
      ) +
      xlim(-0.2, 0.2) +
      labs(x = "Average Treatment Effect",
           y = NULL) +
      theme(legend.position = "none")
  } %>%
  print()



# Table 3 --------------------------------------------------------------

data_effort %>%
  as.data.frame() %>%
  plm(
    effort ~ z + w + k + theta + I(theta^2)
    , model = "within"
    , index = c("id", "round")
    , data = .
  ) %>%
  coeftest(., vcov = function(x) vcovHC(x, type = "sss", cluster = "group")) %>%
  tidy() %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rename(
    Var = term,
    Coefficient = estimate,
    SE = std.error,
    Statistic = statistic,
    `p-value` = p.value
  ) %>%
  left_join(var_names) %>%
  select(Variable, everything()) %>%
  select(-Var) %>%
  mutate(across(`p-value`, as.character)) %>%
  mutate(across(`p-value`, ~replace(., . == 0, "<0.001"))) %>%
  print()


# Figure 4 --------------------------------------------------------------------


rbind(
  {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(., theta %in% c(0, 1)) %>%
      group_by(., id, z, theta) %>%
      summarise(., effort = mean(effort)) %>%
      group_by(., theta) %>%
      do(
        tidy(
          t.test(effort ~ z, data = ., paired = T)
        )
      ) %>%
      mutate(var = "z")
  } # z, for diff = 0 or 1
  , {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(., theta %in% c(0.5)) %>%
      group_by(., id, z, theta) %>%
      summarise(., effort = mean(effort)) %>%
      group_by(., theta) %>%
      do(
        tidy(
          t.test(effort ~ z, data = ., paired = F)
        )
      ) %>%
      mutate(var = "z")
  } # z, for diff = 0.5
  , {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(k == 1) %>%
      filter(., theta %in% c(0, 1)) %>%
      group_by(., id, w, theta) %>%
      summarise(., effort = mean(effort)) %>%
      group_by(., theta) %>%
      do(
        tidy(
          t.test(effort ~ w, data = ., paired = T)
        )
      ) %>%
      mutate(var = "w")
  } # w, for diff = 0 or 1
  , {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(k == 1) %>%
      filter(., theta %in% c(0.5)) %>%
      group_by(., id, w, theta) %>%
      summarise(., effort = mean(effort)) %>%
      group_by(., theta) %>%
      do(
        tidy(
          t.test(effort ~ w, data = ., paired = F)
        )
      ) %>%
      mutate(var = "w")
  } # z, for diff = 0.5
  , {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(w == 2) %>%
      filter(., theta %in% c(0, 1)) %>%
      group_by(., id, k, theta) %>%
      summarise(., effort = mean(effort)) %>%
      group_by(., theta) %>%
      do(
        tidy(
          t.test(effort ~ k, data = ., paired = T)
        )
      ) %>%
      mutate(var = "k")
  } # k, for diff = 0 or 1
  , {data_effort %>%
      mutate_at(
        vars(matches("theta|z|w|k")),
        as.factor
      ) %>%
      filter(w == 2) %>%
      filter(., theta %in% c(0.5)) %>%
      group_by(., id, k, theta) %>%
      summarise(., effort = mean(effort)) %>%
      group_by(., theta) %>%
      do(
        tidy(
          t.test(effort ~ k, data = ., paired = F)
        )
      ) %>%
      mutate(var = "k")
  } # z, for diff = 0.5
) %>%
  mutate_at(vars(c("estimate", "conf.low", "conf.high")), multiply_by, -1) %>%
  group_by(var) %>%
  mutate(int_eff = estimate[2] - estimate[1]) %>%
  ungroup() %>%
  mutate(var = factor(var, labels = c("Cost", "Wage", "Bonus"))) %>%
  {ggplot(., aes(x = estimate, y = theta, color = theta)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        size = graphs$linesize,
        height = 0.1,
        color = "grey"
      ) +
      geom_point(size = 2*graphs$linesize)  +
      geom_text(
        stat = "identity",
        aes(label = paste0(round(estimate, 2))),
        # position = position_dodge2(width = 0.5),
        vjust = -1,
        size = 9*5/14,
        color = "black"
      ) +
      # xlim(-0.2, 0.2) +
      labs(x = "Average Treatment Effect",
           y = "Difficulty") +
      facet_wrap( ~ var) +
      # coord_flip() +
      theme(legend.position = "none")
  } %>%
  print()


# Figure D.3  ----------------------------------------------------------

data_effort %>%
  filter(theta %in% c(0, 0.5, 1)) %>%
  mutate(
    incentives = case_when(
      (w == 1 & z == 2) ~ "low"
      , (w == 2 & z == 4) ~ "high"
    )
  ) %>%
  filter(!is.na(incentives)) %>%
  mutate(across(c("w", "z", "k", "theta"), factor)) %>%
  select(incentives, theta, effort) %>%
  nest(data = effort) %>%
  rename(effort = data) %>%
  mutate(t.test = map(effort, t.test)) %>%
  mutate(t.test = map(t.test, tidy)) %>%
  unnest(t.test) %>%
  {ggplot(., aes(theta, estimate, fill = theta)) +
      geom_col(width = 0.5) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        size = graphs$linesize,
        width = 0.1,
        color = "grey") +
      facet_wrap(~ incentives, labeller = label_both) +
      ylim(0, 1) +
      labs(x = "Difficulty", y = "Mean Effort") +
      theme(legend.position = "none")
  } %>% # plot
  print()


# Table D.1 ---------------------------------------------------------------------


reg_fe_inter <-
  data_effort %>%
  mutate(across(c("z", "w", "k", "theta"), as_factor)) %>%
  filter(theta %in% c(0, 0.5, 1)) %>%
  as.data.frame() %>%
  plm(
    # effort ~ z*w*k*theta
    effort ~ z + w + k + theta + z:theta + w:theta + k:theta + z:w + z:k
    # effort ~ z + w + k + theta
    , model = "within"
    , index = c("id", "round")
    , data = .
  )

# export
reg_fe_inter %>%
  coeftest(., vcov = function(x) vcovHC(x, type = "sss", cluster = "group")) %>%
  print() %>%
  tidy() %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rename(
    Variable = term,
    Coefficient = estimate,
    SE = std.error,
    Statistic = statistic,
    `p-value` = p.value
  ) %>%
  mutate(
    Variable = str_replace_all(Variable, "z4", "Bonus = 4")
    , Variable = str_replace_all(Variable, "w2", "Wage = 2")
    , Variable = str_replace_all(Variable, "k2", "Cost = 2")
    , Variable = str_replace_all(Variable, "theta0.5", "Difficulty = 0.5")
    , Variable = str_replace_all(Variable, "theta1", "Difficulty = 1")
    , Variable = str_replace_all(Variable, ":", " x ")
  ) %>%
  mutate(across(`p-value`, as.character)) %>%
  mutate(across(`p-value`, ~replace(., . == 0, "<0.001"))) %>%
  print()


# Figure D.7 -----------------------------------------------------------

# > bonus ----
data_effort %>%
  select(id, effort, z) %>%
  group_by(id, z) %>%
  summarize(across(.cols = everything(), mean)) %>%
  arrange(id, z) %>%
  mutate(ate = diff(effort)) %>%
  select(id, ate) %>%
  distinct() %>%
  left_join(., data_demog %>% select(id, gender)) %>%
  filter(gender %in% c("Male", "Female")) %>%
  mutate(ate_positive = if_else(ate >= 0, "Increasing", "Decreasing")) %>%
  group_by(ate_positive, gender) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  group_by(gender) %>%
  mutate(sum_count = sum(count)) %>%
  group_by(gender, ate_positive) %>%
  do(
    tidy(
      binom.test(
        x = .data$count, n = .data$sum_count
      )
    )
  ) %>%
  group_by(ate_positive) %>%
  mutate(
    ate = diff(estimate)
    , phi = 2*asin(sqrt(estimate))
    , cohen_h = diff(phi)
  ) %>%
  {ggplot(., aes(factor(gender), estimate, fill = ate_positive)) +
      geom_hline(yintercept = 0.5, linetype = 2, color = "grey", size = 1) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(),
        width = 0.5
      ) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , position = position_dodge(width = 0.5)
        , width = 0.1
        , color = "grey"
        , size = 1
      ) +
      geom_text(
        stat = "identity",
        aes(
          # label = paste0(round(100 * prop), "% (", count, ")"))
          label = paste0(round(100 * estimate), "%"))
        , position = position_dodge2(width = 0.5)
        , vjust = -1
        , hjust = -0.5
        , size = 9*5/14
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = NULL,
        x = "Gender",
        y = NULL
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize)
        , legend.justification = c(0, 0)
        , legend.position = c(0.05, 0.9)
        , legend.direction = "horizontal"
        # , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
  print()


# > wage ----
data_effort %>%
  filter(k == 1) %>%
  select(id, effort, w) %>%
  group_by(id, w) %>%
  summarize(across(.cols = everything(), mean)) %>%
  arrange(id, w) %>%
  mutate(ate = diff(effort)) %>%
  select(id, ate) %>%
  distinct() %>%
  left_join(., data_demog %>% select(id, gender)) %>%
  filter(gender %in% c("Male", "Female")) %>%
  mutate(ate_positive = if_else(ate >= 0, "Increasing", "Decreasing")) %>%
  group_by(ate_positive, gender) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))  %>%
  group_by(gender) %>%
  mutate(sum_count = sum(count)) %>%
  group_by(gender, ate_positive) %>%
  do(
    tidy(
      binom.test(
        x = .data$count, n = .data$sum_count
      )
    )
  ) %>%
  group_by(ate_positive) %>%
  mutate(
    ate = diff(estimate)
    , phi = 2*asin(sqrt(estimate))
    , cohen_h = diff(phi)
  ) %>%
  {ggplot(., aes(factor(gender), estimate, fill = ate_positive)) +
      geom_hline(yintercept = 0.5, linetype = 2, color = "grey", size = 1) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(),
        width = 0.5
      ) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , position = position_dodge(width = 0.5)
        , width = 0.1
        , color = "grey"
        , size = 1
      ) +
      geom_text(
        stat = "identity",
        aes(
          # label = paste0(round(100 * prop), "% (", count, ")"))
          label = paste0(round(100 * estimate), "%"))
        , position = position_dodge2(width = 0.5)
        , vjust = -1
        , hjust = -0.5
        , size = 9*5/14
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = NULL,
        x = "Gender",
        y = NULL
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize)
        , legend.justification = c(0, 0)
        , legend.position = c(0.05, 0.9)
        , legend.direction = "horizontal"
        # , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
  print()

# > cost ----
data_effort %>%
  filter(w == 2) %>%
  select(id, effort, k) %>%
  group_by(id, k) %>%
  summarize(across(.cols = everything(), mean)) %>%
  arrange(id, k) %>%
  mutate(ate = diff(effort)) %>%
  select(id, ate) %>%
  distinct() %>%
  left_join(., data_demog %>% select(id, gender)) %>%
  filter(gender %in% c("Male", "Female")) %>%
  mutate(ate_positive = if_else(ate >= 0, "Increasing", "Decreasing")) %>%
  group_by(ate_positive, gender) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  group_by(gender) %>%
  mutate(sum_count = sum(count)) %>%
  group_by(gender, ate_positive) %>%
  do(
    tidy(
      binom.test(
        x = .data$count, n = .data$sum_count
      )
    )
  ) %>%
  group_by(ate_positive) %>%
  mutate(
    ate = diff(estimate)
    , phi = 2*asin(sqrt(estimate))
    , cohen_h = diff(phi)
  ) %>%
  {ggplot(., aes(factor(gender), estimate, fill = ate_positive)) +
      geom_hline(yintercept = 0.5, linetype = 2, color = "grey", size = 1) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(),
        width = 0.5
      ) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , position = position_dodge(width = 0.5)
        , width = 0.1
        , color = "grey"
        , size = 1
      ) +
      geom_text(
        stat = "identity",
        aes(
          # label = paste0(round(100 * prop), "% (", count, ")"))
          label = paste0(round(100 * estimate), "%"))
        , position = position_dodge2(width = 0.5)
        , vjust = -1
        , hjust = -0.5
        , size = 9*5/14
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = NULL,
        x = "Gender",
        y = NULL
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize)
        , legend.justification = c(0, 0)
        , legend.position = c(0.05, 0.9)
        , legend.direction = "horizontal"
        # , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
  print()

# > difficulty ----
data_effort %>%
  select(id, effort, theta) %>%
  filter(theta %in% c(0, 0.5)) %>%
  group_by(id, theta) %>%
  summarize(across(.cols = everything(), mean)) %>%
  arrange(id, theta) %>%
  mutate(ate1 = diff(effort)) %>%
  select(-c(theta, effort)) %>%
  distinct() %>%
  left_join(
    data_effort %>%
      select(id, effort, theta) %>%
      filter(theta %in% c(0.5, 1)) %>%
      group_by(id, theta) %>%
      summarize(across(.cols = everything(), mean)) %>%
      arrange(id, theta) %>%
      mutate(ate2 = diff(effort)) %>%
      select(-c(theta, effort)) %>%
      distinct()
  ) %>%
  mutate(ate_type = case_when(
    ate1 >= 0 & ate2 >= 0 ~ "Incr",
    ate1 <= 0 & ate2 <= 0 ~ "Decr",
    ate1 >= 0 & ate2 <= 0 ~ "Inv-U",
    ate1 <= 0 & ate2 >= 0 ~ "U"
  )
  ) %>%
  select(id, ate_type) %>%
  left_join(., data_demog %>% select(id, gender)) %>%
  filter(gender %in% c("Male", "Female")) %>%
  group_by(ate_type, gender) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  group_by(gender) %>%
  mutate(sum_count = sum(count)) %>%
  group_by(gender, ate_type) %>%
  do(
    tidy(
      binom.test(
        x = .data$count, n = .data$sum_count
      )
    )
  ) %>%
  group_by(ate_type) %>%
  mutate(
    ate = diff(estimate)
    , phi = 2*asin(sqrt(estimate))
    , cohen_h = diff(phi)
  ) %>%
  {ggplot(., aes(factor(gender), estimate, fill = ate_type)) +
      geom_hline(yintercept = 0.5, linetype = 2, color = "grey", size = 1) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(),
        width = 0.5
      ) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high)
        , position = position_dodge(width = 0.5)
        , width = 0.1
        , color = "grey"
        , size = 1
      ) +
      geom_text(
        stat = "identity",
        aes(
          # label = paste0(round(100 * prop), "% (", count, ")"))
          label = paste0(round(100 * estimate), "%"))
        , position = position_dodge2(width = 0.5)
        , vjust = -0.5
        , hjust = -0.1
        , size = 9*5/14*0.75
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = NULL,
        x = "Gender",
        y = NULL
      ) +
      theme(
        legend.background = element_rect(colour = 'white', size = 0.25*graphs$linesize)
        , legend.justification = c(0, 0)
        , legend.position = c(0.05, 0.9)
        , legend.direction = "horizontal"
        # , legend.box.margin = margin(c(10,10,10,10))
      )
  } %>%
  print()
