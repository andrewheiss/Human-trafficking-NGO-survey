permission.given <- responses.org %>%
  filter(Q1.3 == "No") %>%
  select(survey.id)

write.csv(permission.given, file="Data/permission_given.csv", row.names=FALSE)
