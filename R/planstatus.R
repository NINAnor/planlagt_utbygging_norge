# --------------------------------------------------------------------------- #
# Name: planstatus.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

status_data <- data %>%
  group_by(planstatus) %>%
  summarize(observations = n())

# 1 = planlegging igangsatt
# 2 = planforslag
# 3 = endelig vedtatt arealplan
# 4 = opphevet
# 5 = utgått/erstattet
# 6 = vedtatt plan med utsatt rettsvirkning
# 7 = vedtatt plan underlagt tidsbegrensning
# 8 = overstyrt

status_data

# planstatus: antall områder
# 1 = planlegging igangsatt: 228
# 1 = planlegging igangsatt: 1386
# 3 = endelig vedtatt arealplan: 245557
# 4 = opphevet: 37
# 5 = utgått/erstattet: 21
# 6 = vedtatt plan med utsatt rettsvirkning: 239
# 7 = vedtatt plan underlagt tidsbegrensning: 179
# 8 = overstyrt: 3540

245557/(sum(status_data$observations))
