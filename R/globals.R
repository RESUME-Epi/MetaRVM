utils::globalVariables(c(".", "time", "disease_state", "population_id", "day", "value", "instance", "date", "N", "S0", "I0", "V0", "R0"))
# Note: Category column names (previously age, race, zone) are now user-defined
# and handled dynamically through data.table's get() function or ..column notation
utils::globalVariables(c("initial<-", "user", "update<-", "step", "interpolate", "j", "rbinom", "output<-"))