####################################################################
### Convenience Functions to interact with House of Reps' data #####
####################################################################


votes <- get_house_primary_vote("Wills",party_abb = c("ALP","GRN","LP"),aggregation = TRUE)
turnout <- get_house_turnout("Wills") %>% select(Year,Turnout)


a<- votes %>% left_join(turnout,by="Year") %>%
  mutate(Percentage=100*OrdinaryVotes/Turnout) %>%
  select(PartyAb,Year,Percentage,SittingMemberFl) %>%
  ggplot()+
  geom_line(aes(x=Year,y=Percentage,group=PartyAb))

a

df %>% select(PartyAb,Year,SittingMemberFl,) +


