source("functions_to_feature_selection.R")




model <- glm(formula = target~dtiWoe+annual_incWoe+revol_utilWoe+total_rev_hi_limWoe+home_ownershipWoe,
             data = dataTrainWithSelectionIvFeaturesWoe,
             family = binomial(link='logit'))

