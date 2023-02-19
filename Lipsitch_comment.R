rm(list=ls())
load("Lipsitch_comment.Rdata")
#These are all indexed by week since time 0. For unvax that is week since study start (July 31). 
#For vax this is weeks since vaccination.
attach(foo)
len<-length(U_risk_calwk) #study length in weeks
new_vax_calwk<-diff(rev(c(V_risk_studywk,0))) # infer number vaxed in each week from the differences in the number followed up all, all but 1, ... weeks from vaccination. 
cum_vax_calwk<-cumsum(new_vax_calwk) #cumulative number in the vaccinated group
#vax_at and cum_vax are indexed by calendar time, not by time since vaccination as the other vaccinated variables.         


N_calc<-V_risk_studywk[1]+U_risk_calwk[1] #total study size calculated from the number ever vaccinated and the number always unvax
U_incinf_calwk<-c(diff(c(0,U_event_calwk)))# incident infections by calendar week in unvax
U_incrate_calwk<-U_incinf_calwk/(U_risk_calwk + V_risk_studywk[1] - cum_vax_calwk) #incidence rate in unvax
V_inc_null_calwk<-U_incrate_calwk*cum_vax_calwk #incident cases eqch week with placebo vaccine
V_cum_null_calwk<-cumsum(V_inc_null_calwk) #total incident cases by each week with placebo vax

RR_crude_recalc<-V_event_studywk[len]/V_cum_null_calwk[len]
VE_crude_recalc <- 1-RR_crude_recalc



# make plot ---------------------------------------------------------------

# load packages
library(ggplot2)
library(tidyverse)
library(patchwork)

# helper function
specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

# create data.frame
df <- data.frame(
  time = seq(as.Date('2022/07/31'), as.Date('2022/12/25'), by = "week"),
  null = V_cum_null_calwk,
  vaccinated = V_event_studywk,
  unvaccinated = U_event_calwk
)

# reshape to long
df_long <- 
  pivot_longer(df, -time) %>%
  mutate(
    name = factor(
      name, 
      levels = c("unvaccinated", "vaccinated", "null")
    )
  )

# plot of cumulative cases over time
p1 <- ggplot(
  df_long %>% filter(name != "vaccinated"), 
  aes(x = time, y = value, color = name)) +
  geom_point(
    size = 2,
    data = df_long %>% 
      filter(name == "vaccinated" & time == "2022-12-25")
  ) +
  geom_step(aes(linetype = name == "null"), size = 1.2) +
  geom_errorbar(
    aes(
      x = time + 5,
      # xend = time + 1,
      y = NULL,
      ymin = vaccinated,
      ymax = null
    ),
    data = df %>% filter(time == "2022-12-25"),
    color = "grey70"
  ) +
  geom_errorbar(
    aes(
      x = time + 10,
      # xend = time + 1,
      y = NULL,
      ymin = vaccinated,
      ymax = unvaccinated
    ),
    data = df %>% filter(time == "2022-12-25"),
    color = "grey70"
  ) +
  annotate(
    geom = "text",
    x = as.Date('2022/12/25') + 5,
    y = 3,
    label = bquote(VE[crude] %~~% 33*"%"),
    hjust = "center",
    color = "grey70"
      # parse = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date('2022/12/25') + 10,
    y = 18,
    label = bquote(VE[crude] == 69*"%"),
    hjust = "center",
    color = "grey70"
      # parse = TRUE
  ) +
  theme_classic(base_size = 14) +
  scale_color_manual(
    name = "",
    values = c("#FF0000", "#ADD8E6", "#333333"),
    labels = c("unvaccinated", "vaccinated", "null (unvaccinated rate x vaccinated enrollment)"),
    drop = FALSE
  ) +
  scale_linetype_discrete(guide = "none") +
  #scale_x_continuous(breaks = seq(0, 7 * nrow(foo) - 1, 7)) +
  scale_x_date(
    breaks = seq(as.Date('2022/07/31'), as.Date('2022/12/25'), by = "week"),
    labels = function(x) str_wrap(format(x, format = "%b %d"), width = 4)
  ) +
  labs(
    x = NULL,
    y = "Cumulative cases of mpox"
  ) +
  theme(legend.position = "top")

# numbers at risk
p3 <- ggplot(
  pivot_longer(df, -time) %>% filter(name != "vaccinated"), 
  aes(
    x = time, 
    y = factor(name, 
               levels = c("vaccinated", "null", "unvaccinated"),
               labels = c("vaccinated (calc.)", "null (calc.)", "unvaccinated")
    ),
    label = format(round(value, 1), big.mark = ",")
  )
) +
  geom_text(hjust = 'center', size = 3.5) +
  scale_y_discrete(labels = function(x) str_wrap(x, 30)) +
  scale_x_date(
    limits = c(as.Date('2022/07/31'), as.Date('2022/12/25') + 10),
    breaks = seq(as.Date('2022/07/31'), as.Date('2022/12/25'), by = "week"),
    labels = function(x) str_wrap(format(x, format = "%b %d"), width = 4)
  ) +
  # scale_x_continuous(breaks = seq(0, 7 * nrow(foo) - 1, 7)) +
  theme_classic(base_size = 14) +
  labs(
    x = NULL,
    y = NULL,
    title = "Number at risk"
  ) +
  theme(axis.text.x = element_blank())

# combine plots in single layout using patchwork
p <- p1 / p3 + plot_layout(heights = c(3, 1))
p[[1]] <- p[[1]] + theme(axis.title.y = element_text(vjust = -15))
p


# make table --------------------------------------------------------------

df2 <- data.frame(
  date = seq(as.Date('2022/07/31'), as.Date('2022/12/25'), by = "week"),
  unvaccinated = U_risk_calwk + V_risk_studywk[1] - cum_vax_calwk,
  events = U_event_calwk,
  ir = U_incrate_calwk,
  cumvax = cum_vax_calwk,
  vaccinc0 = V_inc_null_calwk
)

df2 %>%
  mutate(
    date = format(date, "%b %d"),
    ir = ir * 1000,
    across(-date, round, 1)
  ) %>%
  flextable() %>%
  save_as_docx(path = "df2.docx")



