
### Clear memory
rm(list = ls())

#dataset transformation

database <- read.csv2("Airline_db.csv", header=TRUE)


library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL",
  modelDescr      = "MNL model on mode choice data",
  indivID         = "SubjectId", 
  outputDirectory = "outputmodel1",
  panelData=FALSE
)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_NonStop        = 0,
              asc_1Stop          = 0,
              asc_1StopChange    = 0,
              b_StopPenaltyHours = 0,
              b_Fare             = 0,
              b_Legroom2         = 0,
              b_Legroom3         = 0,
              b_Legroom4         = 0,
              b_DiffTime         = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_NonStop")

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["NonStop"]]  = asc_NonStop + b_Fare * Fare_1 + b_Legroom2 * Legroom12 + b_Legroom3 * Legroom13 + b_Legroom4 * Legroom14 + b_DiffTime * DiffTime1
  V[["OneStop"]]  = asc_1Stop  + b_StopPenaltyHours * StopPenaltyHours_2  + b_Fare * Fare_2 + b_Legroom2 * Legroom22 + b_Legroom3 * Legroom23 + b_Legroom4 * Legroom24 + b_DiffTime * DiffTime2
  V[["OneStopChange"]]  = asc_1StopChange  + b_StopPenaltyHours * StopPenaltyHours_3  + b_Fare * Fare_3 + b_Legroom2 * Legroom32 + b_Legroom3 * Legroom33 + b_Legroom4 * Legroom34 + b_DiffTime * DiffTime3
  
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(NonStop =1, OneStop=2, OneStopChange=3), 
    avail         = 1,
    choiceVar     = Choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

Model1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(Model1, modelOutput_settings)
apollo_saveOutput(Model1, modelOutput_settings)

#WILLINGNESS TO PAY MODEL 1

#LEGROOM 2
0.37965766 / -0.0193137
#LEGROOM 3
0.42743215 / -0.0193137
#LEGROOM 4
0.65897922 / -0.0193137
#DIFFTIME
-0.1082203 / -0.0193137

####MODEL 2

### Set core controls
apollo_control = list(
  modelName       = "MNL_Covariates",
  modelDescr      = "MNL model with interactions",
  indivID         = "SubjectId", 
  outputDirectory = "outputmodel2",
  panelData=FALSE
)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(NonStop =1, OneStop=2, OneStopChange=3),
  avail        = 1,
  choiceVar    = database$Choice,
  explanators  = database[,c("BusinessTrip","PartySize","Income","IncomeDummy", "Female","Undergraduate","Posgraduate",
                             "Age25_44","Age45_54","Age55")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# choiceAnalysis_settings2 <- list(
#   alternatives = c(Direct=1, Stop=2),
#   choiceVar = (1*((database$Choice==1))
#                +2*((database$Choice==2)+(database$Choice==3))),
#   explanators  = database[,c("BusinessTrip","PartySize","Income","IncomeDummy", "Female","Undergraduate","Posgraduate",
#                              "Age25_44","Age45_54","Age55")]
# )
# 
# apollo_choiceAnalysis(choiceAnalysis_settings2, apollo_control, database)



# ################################################################# #
#### MODEL WITH COVARIATES                                      ####
# ################################################################# #


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_NonStop        = 0,
              asc_1Stop          = 0,
              asc_1StopChange    = 0,
              asc_1StopChange_Shift_Income = 0,
              b_StopPenaltyHours = 0,
              b_StopPenaltyHours_Shift_Business = 0,
              b_Fare             = 0,
              b_Legroom2         = 0,
              b_Legroom3         = 0,
              b_Legroom4         = 0,
              b_DiffTime         = 0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_NonStop")

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Create alternative specific constants and coefficients using interactions with socio-demographics
  asc_1StopChange_Income = asc_1StopChange  + asc_1StopChange_Shift_Income * IncomeDummy
  b_StopPenalty_Business = b_StopPenaltyHours + b_StopPenaltyHours_Shift_Business * BusinessTrip
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["NonStop"]]  = asc_NonStop + b_Fare * Fare_1 + b_Legroom2 * Legroom12 + b_Legroom3 * Legroom13 + b_Legroom4 * Legroom14 + b_DiffTime * DiffTime1
  V[["OneStop"]]  = asc_1Stop + b_StopPenalty_Business * StopPenaltyHours_2  + b_Fare * Fare_2 + b_Legroom2 * Legroom22 + b_Legroom3 * Legroom23 + b_Legroom4 * Legroom24 + b_DiffTime * DiffTime2
  V[["OneStopChange"]]  = asc_1StopChange_Income + b_StopPenalty_Business * StopPenaltyHours_3  + b_Fare * Fare_3 + b_Legroom2 * Legroom32 + b_Legroom3 * Legroom33 + b_Legroom4 * Legroom34 +  b_DiffTime * DiffTime3
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(NonStop =1, OneStop=2, OneStopChange=3), 
    avail         = 1,
    choiceVar     = Choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

Model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(Model2, modelOutput_settings)
apollo_saveOutput(Model2, modelOutput_settings)

###COMPARE MNL1 VS MNL2

apollo_lrTest(Model1,Model2)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)

### Now imagine the cost for option1 increases by 1%
database$Fare_1 = 1.01*database$Fare_1

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_new = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)
### Return to original data
database$Fare_1 = 1/1.01*database$Fare_1
apollo_inputs = apollo_validateInputs()

### Compute own elasticity for option1:
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.01)

### Compute cross-elasticities for other options
#option 2
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.01)
#option3
log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.01)


### Use the estimated model to make predictions
predictions_base = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)

### Now imagine the cost for option2 increases by 1%
database$Fare_2 = 1.01*database$Fare_2

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_new = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)
### Return to original data
database$Fare_2 = 1/1.01*database$Fare_2
apollo_inputs = apollo_validateInputs()

### Compute own elasticity for option2:
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.01)

### Compute cross-elasticities for other options
#option 1
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.01)
#option3
log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.01)

### Use the estimated model to make predictions
predictions_base = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)

### Now imagine the cost for option3 increases by 1%
database$Fare_3 = 1.01*database$Fare_3

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_new = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)
### Return to original data
database$Fare_3 = 1/1.01*database$Fare_3
apollo_inputs = apollo_validateInputs()

### Compute own elasticity for option3:
log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.01)

### Compute cross-elasticities for other options
#option 1
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.01)
#option3
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.01)


###MODEL 3

### Set core controls
apollo_control = list(
  modelName       = "MMNL",
  modelDescr      = "Mixed Multinomial Logit Model",
  indivID         = "SubjectId", 
  outputDirectory = "outputmodel3",
  mixing=TRUE,
  panelData=FALSE,
  nCores=4
)


#### DEFINE MODEL PARAMETERS                  
### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(asc_NonStop        = 0,
              asc_1Stop          = 0,
              asc_1StopChange    = 0,
              mu_log_StopPenaltyHours = -3,
              sigma_log_StopPenaltyHours = -0.01,
              b_Fare             = 0,
              b_Legroom2         =  0,
              b_Legroom3         =  0,
              b_Legroom4         =  0,
              mu_log_DiffTime    = -3,
              sigma_log_DiffTime = -0.01)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_NonStop")

#### DEFINE RANDOM COMPONENTS 
### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_DiffTime","draws_StopPenalty")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_StopPenalty"]] = -exp(mu_log_StopPenaltyHours + sigma_log_StopPenaltyHours * draws_StopPenalty) 
  randcoeff[["b_DiffTime"]] = -exp( mu_log_DiffTime + sigma_log_DiffTime * draws_DiffTime )
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["NonStop"]]  = asc_NonStop + b_Fare * Fare_1 + b_Legroom2 * Legroom12 + b_Legroom3 * Legroom13 + b_Legroom4 * Legroom14 + b_DiffTime * DiffTime1
  V[["OneStop"]]  = asc_1Stop + b_StopPenalty * StopPenaltyHours_2  + b_Fare * Fare_2 + b_Legroom2 * Legroom22 + b_Legroom3 * Legroom23 + b_Legroom4 * Legroom24 + b_DiffTime * DiffTime2
  V[["OneStopChange"]]  = asc_1StopChange + b_StopPenalty * StopPenaltyHours_3  + b_Fare * Fare_3 + b_Legroom2 * Legroom32 + b_Legroom3 * Legroom33 + b_Legroom4 * Legroom34 +  b_DiffTime * DiffTime3
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(NonStop =1, OneStop=2, OneStopChange=3), 
    avail         = 1,
    choiceVar     = Choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MMNL = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(MMNL, modelOutput_settings)
apollo_saveOutput(MMNL, modelOutput_settings)

#parameters of distribution of beta DiffTime
mean_DiffTime<- -exp(-3.0710841+((-2.2368974403498)^2/2))
mean_DiffTime





