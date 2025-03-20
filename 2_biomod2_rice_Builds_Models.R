## 3. Build the models ########################################################
## Set Output Ditectory #######################################################
setwd("D:/Biomod2_Rice/Climate_Data/Models")

# Formatting Data for current models
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)
myBiomodData
plot(myBiomodData)

#Plot Climate data > myExpl
plot(myExpl, nc=5, maxnl=20)
#plot(myExpl)

## Defining Models Options using default options. #############################
myBiomodOption <- BIOMOD_ModelingOptions()

# print the object
myBiomodOption

#  Set MAXENT model options
# myBiomodOption <- BIOMOD_ModelingOptions(
#   MAXENT.Phillips = list( path_to_maxent.jar = "D:/Biomod2_Rice/Climate_Data/Models",
#                  maximumiterations = 200,
#                  visible = FALSE,
#                  linear = TRUE,
#                  quadratic = TRUE,
#                  product = TRUE,
#                  threshold = TRUE,
#                  hinge = TRUE,
#                  lq2lqptthreshold = 80,
#                  l2lqthreshold = 10,
#                  hingethreshold = 15,
#                  beta_threshold = -1,
#                  beta_categorical = -1,
#                  beta_lqp = -1,
#                  beta_hinge = -1,
#                  defaultprevalence = 0.5))
# 
# print default parameters & return then follow the same
# Print_Default_ModelingOptions()

## Computing the models ########################################################
#   GBM : Generalized Boosting Model or usually called Boosted Regression Trees
#   ANN: Artificial Neural Network
#   SRE: Surface Range Envelop or usually called BIOCLIM
#   MARS: Multiple Adaptive Regression Splines
#   RF: Random Forest

myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('RF'),
  #models = c('MAXENT.Phillips', 'RF','GBM', 'GAM','CTA','ANN'),
  models.options = myBiomodOption,
  NbRunEval = 1,
  DataSplit = 80,
  Yweights = NULL,
  Prevalence = 0.5,
  VarImport = 3,
  models.eval.meth = c('ROC','TSS'),
  SaveObj = TRUE,
  rescal.all.models = FALSE,
  do.full.models = TRUE,
  modeling.id = paste(myRespName,"Modeling_RF",sep=""))

# modeling summary
myBiomodModelOut

# get all models evaluation
get_predictions(myBiomodModelOut)
get_calib_lines(myBiomodModelOut)
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
get_variables_importance(myBiomodModelOut)
get_built_models(myBiomodModelOut)


# print the dimnames of this object
dimnames(myBiomodModelEval)
print(myBiomodModelEval)

optmROC = getStatOptimValue('ROC')

# save models evaluation score & variable importance
capture.output(get_evaluations(myBiomodModelOut),
               file = file.path(myRespName,
                                paste(myRespName, "formal_ModelsEva.txt",
                                      sep = "")))
capture.output(get_variables_importance(myBiomodModelOut),
               file = file.path(myRespName,
                                paste(myRespName, "formal_ModelsEvaImp.txt",
                                      sep = "")))


# plot evaluation models score graph
# by models
gg1 <- models_scores_graph(myBiomodModelOut, 
                           by = 'models', 
                           metrics = c('ROC', 'TSS'))

gg1custom <- gg1 + ggtitle("Diff between Models evaluation scores")

# by cross validatioin run
gg2 <- models_scores_graph( myBiomodModelOut,
                            by = 'cv_run',
                            metrics = c('ROC','TSS') )

# Load the models for which we want to extract the predicted response curves
# myCTAs <- BIOMOD_LoadModels(myBiomodModelOut, models='CTA')
# myGBMs <- BIOMOD_LoadModels(myBiomodModelOut, models='GBM')
# myANNs <- BIOMOD_LoadModels(myBiomodModelOut, models='ANN')
# myGAMs <- BIOMOD_LoadModels(myBiomodModelOut, models='GAM')
myRFs <- BIOMOD_LoadModels(myBiomodModelOut, models='RF')
# myMaxent.P <- BIOMOD_LoadModels(myBiomodModelOut, models='MAXENT.Phillips')



# plot 2D response plots
# myRespPlot2D <- response.plot2(models  = myGAMs,
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = FALSE,
#                                fixed.var.metric = 'median',
#                                col = c("blue", "red"),
#                                legend = TRUE,
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))
# 
# 
# myRespPlot2D <- response.plot2(models  = myGBMs,
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = FALSE,
#                                fixed.var.metric = 'median',
#                                col = c("blue", "red"),
#                                legend = TRUE,
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))
# 
# 
# myRespPlot2D <- response.plot2(models  = myCTAs,
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = FALSE,
#                                fixed.var.metric = 'median',
#                                col = c("blue", "red"),
#                                legend = TRUE,
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))
# 
# 
# myRespPlot2D <- response.plot2(models  = myANNs,
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = FALSE,
#                                fixed.var.metric = 'median',
#                                col = c("blue", "red"),
#                                legend = TRUE,
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))
# 

myRespPlot2D <- response.plot2(models  = myRFs,
                               Data = get_formal_data(myBiomodModelOut,'expl.var'), 
                               show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
                               do.bivariate = FALSE,
                               fixed.var.metric = 'median',
                               col = c("blue", "red"),
                               legend = TRUE,
                               data_species = get_formal_data(myBiomodModelOut,'resp.var'))

# myRespPlot2D <- response.plot2(models  = myMaxent.P,
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = FALSE,
#                                fixed.var.metric = 'median',
#                                col = c("blue", "red"),
#                                legend = TRUE,
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))
# 
# # Plot 3D Response plots
# myRespPlot3D <- response.plot2(models = myRFs[1],
#                                Data = get_formal_data(myBiomodModelOut,
#                                                       'expl.var'),
#                                show.variables = get_formal_data(myBiomodModelOut,
#                                                                 'expl.var.names'),
#                                do.bivariate = TRUE,
#                                fixed.var.metric = 'median',
#                                data_species = get_formal_data(myBiomodModelOut,
#                                                               'resp.var'),
#                                display_title = FALSE)


### all the values used to produce this plot are stored into the returned object
### you can redo plots by yourself and customised them

# let's print the TSS scores of Random Forest
#myBiomodModelEval["TSS","Testing.data","RF",,]

# let's print the ROC scores of all selected models
#myBiomodModelEval["ROC","Testing.data",,,]

# let's print all scores of all selected models
#myBiomodModelEval[,"Testing.data",,,]

# Write "variable importances" to CSV
#write.csv(get_variables_importance(myBiomodModelOut), file = "variables_importance.csv", row.names=T)

# Write "Models Evaluation" to CSV
#write.csv(myBiomodModelEval, file = "models_evaluation.csv", row.names = T)

## 4. Ensemble modeling #######################################################
# 4.1 All models
myBiomodEM.algo <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = myRFs, # RF, ANN, GBM, GAM, CTA 
  em.by='algo', # all, PA_dataset, algo, PA_dataset+algo, PA_dataset+repet
  eval.metric = c('ROC'),
  eval.metric.quality.threshold = 0.7, #0.8
  models.eval.meth = c('ROC','TSS'),
  prob.mean = TRUE,
  prob.cv = FALSE,
  prob.ci = FALSE,
  prob.ci.alpha = 0.05,
  prob.median = FALSE,
  committee.averaging = FALSE,
  prob.mean.weight = TRUE,
  prob.mean.weight.decay = 'proportional' )

myBiomodEM.PA.Rp <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = myRFs, # RF, ANN, GBM, GAM, CTA 
  em.by='PA_dataset+repet', # all, PA_dataset, algo, PA_dataset+algo, PA_dataset+repet
  eval.metric = c('ROC'),
  eval.metric.quality.threshold = 0.7, #0.8
  prob.mean = TRUE,
  prob.cv = FALSE,
  prob.ci = FALSE,
  prob.ci.alpha = 0.05,
  prob.median = FALSE,
  committee.averaging = FALSE,
  prob.mean.weight = TRUE,
  prob.mean.weight.decay = 'proportional' )

myBiomodEM.algo
myBiomodEM.PA.Rp

# evaluate Biomod models with the Boyce index and MPA
pres.only.eval <- BIOMOD_presenceonly(myBiomodModelOut, myBiomodEM.PA.Rp)
pres.only.eval$eval

# 4.2 RF
# myBiomodEM.RF.algo <- BIOMOD_EnsembleModeling(
#   modeling.output = myBiomodModelOut,
#   chosen.models = grep('RF',get_built_models(myBiomodModelOut),
#                        value = TRUE), # RF, ANN, GBM, GAM, CTA 
#   em.by='algo',
#   eval.metric = c('ROC'),
#   eval.metric.quality.threshold = 0.8, #0.8
#   prob.mean = T,
#   prob.cv = F,
#   prob.ci = F,
#   prob.ci.alpha = 0.05,
#   prob.median = F,
#   committee.averaging = F,
#   prob.mean.weight = T,
#   prob.mean.weight.decay = 'proportional' )

# # 4.3 ANN
# myBiomodEM.ANN <- BIOMOD_EnsembleModeling(
#   modeling.output = myBiomodModelOut,
#   chosen.models = grep('ANN',get_built_models(myBiomodModelOut),
#                        value = TRUE), # RF, ANN, GBM, GAM, CTA 
#   em.by='PA_dataset',
#   eval.metric = c('ROC'),
#   eval.metric.quality.threshold = 0.8, #0.8
#   prob.mean = T,
#   prob.cv = T,
#   prob.ci = T,
#   prob.ci.alpha = 0.05,
#   prob.median = T,
#   committee.averaging = T,
#   prob.mean.weight = T,
#   prob.mean.weight.decay = 'proportional' )
# 
# # 4.4 GBM
# myBiomodEM.GMB <- BIOMOD_EnsembleModeling(
#   modeling.output = myBiomodModelOut,
#   chosen.models = grep('GBM',get_built_models(myBiomodModelOut),
#                        value = TRUE), # RF, ANN, GBM, GAM, CTA 
#   em.by='PA_dataset',
#   eval.metric = c('ROC'),
#   eval.metric.quality.threshold = 0.8, #0.8
#   prob.mean = T,
#   prob.cv = T,
#   prob.ci = T,
#   prob.ci.alpha = 0.05,
#   prob.median = T,
#   committee.averaging = T,
#   prob.mean.weight = T,
#   prob.mean.weight.decay = 'proportional' )
# 
# # 4.5 GAM
# myBiomodEM.GAM <- BIOMOD_EnsembleModeling(
#   modeling.output = myBiomodModelOut,
#   chosen.models = grep('GAM',get_built_models(myBiomodModelOut),
#                        value = TRUE), # RF, ANN, GBM, GAM, CTA 
#   em.by='PA_dataset',
#   eval.metric = c('ROC'),
#   eval.metric.quality.threshold = 0.8, #0.8
#   prob.mean = T,
#   prob.cv = T,
#   prob.ci = T,
#   prob.ci.alpha = 0.05,
#   prob.median = T,
#   committee.averaging = T,
#   prob.mean.weight = T,
#   prob.mean.weight.decay = 'proportional' )
# 
# # 4.6 CTA
# myBiomodEM.CTA <- BIOMOD_EnsembleModeling(
#   modeling.output = myBiomodModelOut,
#   chosen.models = grep('CTA',get_built_models(myBiomodModelOut),
#                        value = TRUE), # RF, ANN, GBM, GAM, CTA 
#   em.by='PA_dataset',
#   eval.metric = c('ROC'),
#   eval.metric.quality.threshold = 0.8, #0.8
#   prob.mean = T,
#   prob.cv = T,
#   prob.ci = T,
#   prob.ci.alpha = 0.05,
#   prob.median = T,
#   committee.averaging = T,
#   prob.mean.weight = T,
#   prob.mean.weight.decay = 'proportional' )
# 
# # 4.7 MAXENT.Phillips
# myBiomodEM.MAXENT <- BIOMOD_EnsembleModeling(
#   modeling.output = myBiomodModelOut,
#   chosen.models = grep('MAXENT.Phillips',get_built_models(myBiomodModelOut),
#                        value = TRUE), # RF, ANN, GBM, GAM, CTA 
#   em.by='PA_dataset',
#   eval.metric = c('ROC'),
#   eval.metric.quality.threshold = 0.8, #0.8
#   prob.mean = T,
#   prob.cv = T,
#   prob.ci = T,
#   prob.ci.alpha = 0.05,
#   prob.median = T,
#   committee.averaging = T,
#   prob.mean.weight = T,
#   prob.mean.weight.decay = 'proportional' )


# print summary
myBiomodEM.algo
myBiomodEM.PA.Rp
#myBiomodEM.RF.algo
# myBiomodEM.CTA
# myBiomodEM.GMB
# myBiomodEM.ANN
# myBiomodEM.GAM
# myBiomodEM.MAXENT
# get evaluation scores
get_evaluations(myBiomodEM.algo)
get_evaluations(myBiomodEM.PA.Rp)
#get_evaluations(myBiomodEM.RF.algo)
# get_evaluations(myBiomodEM.CTA)
# get_evaluations(myBiomodEM.GMB)
# get_evaluations(myBiomodEM.ANN)
# get_evaluations(myBiomodEM.GAM)
# get_evaluations(myBiomodEM.MAXENT)

# 5. projection over the globe under current conditions
# All models
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'current',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

# # RF
# myBiomodProj.RF <- BIOMOD_Projection(
#   modeling.output = myBiomodModelOut,
#   new.env = myExpl,
#   proj.name = 'current',
#   selected.models = grep('RF',get_built_models(myBiomodModelOut),
#                          value = TRUE), # RF, CTA, GBM, ANN, GAM
#   binary.meth = 'ROC',
#   compress = 'xz',
#   clamping.mask = F,
#   output.format = '.grd')
# 
# # CTA
# myBiomodProj.CTA <- BIOMOD_Projection(
#   modeling.output = myBiomodModelOut,
#   new.env = myExpl,
#   proj.name = 'current',
#   selected.models = grep('CTA',get_built_models(myBiomodModelOut),
#                          value = TRUE), # RF, CTA, GBM, ANN, GAM
#   binary.meth = 'ROC',
#   compress = 'xz',
#   clamping.mask = F,
#   output.format = '.grd')
# 
# # GBM
# myBiomodProj.GBM <- BIOMOD_Projection(
#   modeling.output = myBiomodModelOut,
#   new.env = myExpl,
#   proj.name = 'current',
#   selected.models = grep('GBM',get_built_models(myBiomodModelOut),
#                          value = TRUE), # RF, CTA, GBM, ANN, GAM
#   binary.meth = 'ROC',
#   compress = 'xz',
#   clamping.mask = F,
#   output.format = '.grd')
# 
# # GAM
# myBiomodProj.GAM <- BIOMOD_Projection(
#   modeling.output = myBiomodModelOut,
#   new.env = myExpl,
#   proj.name = 'current',
#   selected.models = grep('GAM',get_built_models(myBiomodModelOut),
#                          value = TRUE), # RF, CTA, GBM, ANN, GAM
#   binary.meth = 'ROC',
#   compress = 'xz',
#   clamping.mask = F,
#   output.format = '.grd')
# 
# # ANN
# myBiomodProj.ANN <- BIOMOD_Projection(
#   modeling.output = myBiomodModelOut,
#   new.env = myExpl,
#   proj.name = 'current',
#   selected.models = grep('ANN',get_built_models(myBiomodModelOut),
#                          value = TRUE), # RF, CTA, GBM, ANN, GAM
#   binary.meth = 'ROC',
#   compress = 'xz',
#   clamping.mask = F,
#   output.format = '.grd')
# 
# # MAXENT
# myBiomodProj.MAXENT <- BIOMOD_Projection(
#   modeling.output = myBiomodModelOut,
#   new.env = myExpl,
#   proj.name = 'current',
#   selected.models = grep('MAXENT.Phillips',get_built_models(myBiomodModelOut),
#                          value = TRUE), # RF, CTA, GBM, ANN, GAM
#   binary.meth = 'ROC',
#   compress = 'xz',
#   clamping.mask = F,
#   output.format = '.grd')

# summary of crated oject
myBiomodProj
# myBiomodProj.RF
# myBiomodProj.CTA
# myBiomodProj.GBM
# myBiomodProj.GAM
# myBiomodProj.ANN
# myBiomodProj.MAXENT

plot(myBiomodProj)
# plot(myBiomodProj, str.grep = 'RF')
# plot(myBiomodProj, str.grep = 'CTA')
# plot(myBiomodProj, str.grep = 'GBM')
# plot(myBiomodProj, str.grep = 'GAM')
# plot(myBiomodProj, str.grep = 'ANN')
# plot(myBiomodProj, str.grep = 'MAXENT.Phillips')


# files created on hard drive
#list.files("GuloGulo/proj_current/")
# make some plots sub-selected by str.grep argument
#plot(myBiomodProj, str.grep = 'MARS')
# if you want to make custom plots, you can also get the projected map
myCurrentProj <- get_predictions(myBiomodProj)
plot(myCurrentProj)
plot(myCurrentProj$RICE_AllData_RUN1_RF)
plot(myCurrentProj$RICE_AllData_Full_RF)

# myCurrentProj.RF <- get_predictions(myBiomodProj.RF)
# myCurrentProj.RF
# 
# myCurrentProj.CTA <- get_predictions(myBiomodProj.CTA)
# myCurrentProj.CTA
# 
# myCurrentProj.GBM <- get_predictions(myBiomodProj.GBM)
# myCurrentProj.GBM
# 
# myCurrentProj.GAM <- get_predictions(myBiomodProj.GAM)
# myCurrentProj.GAM
# 
# myCurrentProj.ANN <- get_predictions(myBiomodProj.ANN)
# myCurrentProj.ANN
# 
# myCurrentProj.MAXENT <- get_predictions(myBiomodProj.MAXENT)
# myCurrentProj.MAXENT

# 6. make Ensemble Forcasting
myBiomodEF.current.algo <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.algo,
  #selected.models = 'all',
  projection.output = myBiomodProj)

myBiomodEF.current.PA.Rp <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  #selected.models = 'all',
  projection.output = myBiomodProj)

plot(myBiomodEF.current.algo)
plot(myBiomodEF.current.PA.Rp)

# Get predictions result
myCurrentEF <- get_predictions(myBiomodEF.current.PA.Rp)
plot(myCurrentEF$RICE_EMmeanByROC_mergedAlgo_Full_AllData)
plot(myCurrentEF$RICE_EMwmeanByROC_mergedAlgo_Full_AllData)

# =============End of Current Climate Modeling========================

## 5. Projection Models for Future ###############################
# 7. Run models under HadGEM2-CC
# 7.1 RCP45
setwd("D:/Biomod2_Rice/Climate_Data/Models")
myBiomodProj.hg45bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = hg45bi50,
  proj.name = 'hg45bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.hg45bi50)
# plot(myBiomodProj.hg45bi50, str.grep = 'RF')
# plot(myBiomodProj.hg45bi50, str.grep = 'GBM')
# plot(myBiomodProj.hg45bi50, str.grep = 'GAM')
# plot(myBiomodProj.hg45bi50, str.grep = 'CTA')
# plot(myBiomodProj.hg45bi50, str.grep = 'ANN')
# plot(myBiomodProj.hg45bi50, str.grep = 'MAXENT.Phillips')

# make Ensemble Forcasting
myBiomodEF.hg45bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.hg45bi50)

plot(myBiomodEF.hg45bi50)

# 7.2 RCP85
myBiomodProj.hg85bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = hg85bi50,
  proj.name = 'hg85bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.hg85bi50)
# plot(myBiomodProj.hg85bi50, str.grep = 'GBM')
# plot(myBiomodProj.hg85bi50, str.grep = 'GAM')
# plot(myBiomodProj.hg85bi50, str.grep = 'CTA')
# plot(myBiomodProj.hg85bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.hg85bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.hg85bi50)

plot(myBiomodEF.hg85bi50)


# reduce layer names for plotting convegences
plot(myBiomodEF.hg85bi50)
plot(myBiomodProj.hg85bi50)

# 8. Run models under IPSL-CM5A-LR
# 8.1 RCP45
myBiomodProj.ip45bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = ip45bi50,
  proj.name = 'ip45bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.ip45bi50)
# plot(myBiomodProj.ip45bi50, str.grep = 'GBM')
# plot(myBiomodProj.ip45bi50, str.grep = 'GAM')
# plot(myBiomodProj.ip45bi50, str.grep = 'CTA')
# plot(myBiomodProj.ip45bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.ip45bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.ip45bi50)

# reduce layer names for plotting convegences
plot(myBiomodEF.ip45bi50)

# 8.2 RCP85
myBiomodProj.ip85bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = ip85bi50,
  proj.name = 'ip85bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.ip85bi50, str.grep = 'RF')
# plot(myBiomodProj.ip85bi50, str.grep = 'GBM')
# plot(myBiomodProj.ip85bi50, str.grep = 'GAM')
# plot(myBiomodProj.ip85bi50, str.grep = 'CTA')
# plot(myBiomodProj.ip85bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.ip85bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.ip85bi50)

# reduce layer names for plotting convegences
plot(myBiomodEF.ip85bi50)

# 9. Run models under MIROC-ESM-CHEM
# 9.1 RCP45
myBiomodProj.mi45bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = mi45bi50,
  proj.name = 'mi45bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.mi45bi50, str.grep = 'RF')
# plot(myBiomodProj.mi45bi50, str.grep = 'GBM')
# plot(myBiomodProj.mi45bi50, str.grep = 'GAM')
# plot(myBiomodProj.mi45bi50, str.grep = 'CTA')
# plot(myBiomodProj.mi45bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.mi45bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.mi45bi50)

# reduce layer names for plotting convegences
plot(myBiomodEF.mi45bi50)

# 9.2 RCP85
myBiomodProj.mi85bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = mi85bi50,
  proj.name = 'mi85bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.mi85bi50, str.grep = 'RF')
# plot(myBiomodProj.mi85bi50, str.grep = 'GBM')
# plot(myBiomodProj.mi85bi50, str.grep = 'GAM')
# plot(myBiomodProj.mi85bi50, str.grep = 'CTA')
# plot(myBiomodProj.mi85bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.mi85bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.mi85bi50)

# reduce layer names for plotting convegences
plot(myBiomodEF.mi85bi50)

# 10. Run models under MPI-ESM-LR
# 10.1 RCP45
myBiomodProj.mp45bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = mp45bi50,
  proj.name = 'mp45bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.mp45bi50, str.grep = 'RF')
# plot(myBiomodProj.mp45bi50, str.grep = 'GBM')
# plot(myBiomodProj.mp45bi50, str.grep = 'GAM')
# plot(myBiomodProj.mp45bi50, str.grep = 'CTA')
# plot(myBiomodProj.mp45bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.mp45bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.mp45bi50)

# reduce layer names for plotting convegences
plot(myBiomodEF.mp45bi50)

# 10.2 RCP85
myBiomodProj.mp85bi50 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = mp85bi50,
  proj.name = 'mp85bi50',
  selected.models = myRFs,
  binary.meth = 'ROC',
  compress = FALSE,
  clamping.mask = TRUE,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProj.mp85bi50, str.grep = 'RF')
# plot(myBiomodProj.mp85bi50, str.grep = 'GBM')
# plot(myBiomodProj.mp85bi50, str.grep = 'GAM')
# plot(myBiomodProj.mp85bi50, str.grep = 'CTA')
# plot(myBiomodProj.mp85bi50, str.grep = 'ANN')

# make Ensemble Forcasting
myBiomodEF.mp85bi50 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM.PA.Rp,
  projection.output = myBiomodProj.mp85bi50)

# reduce layer names for plotting convegences
plot(myBiomodEF.mp85bi50)

plot(myExpl, nc=5)
plot(hg45bi50, nc = 5)
plot(hg85bi50, nc = 5)
plot(ip45bi50, nc = 5)
plot(ip85bi50, nc = 5)
plot(mi45bi50, nc = 5)
plot(mi85bi50, nc = 5)
plot(mp45bi50, nc = 5)
plot(mp85bi50, nc = 5)

# load binary projections
# here is rasters objects ('.grd')
#Detect where our species occurances state is forecasted to change
currentPred <- stack("RICE/proj_current/proj_current_RICE_ensemble.grd")
futurePred <- stack("RICE/proj_hg45bi50/proj_hg45bi50_RICE_ensemble.grd")

# call the Range size function
myBiomodRangeSize <- BIOMOD_RangeSize(CurrentPred = currentPred,
                                      FutureProj = futurePred)

# see the results
myBiomodRangeSize$Compt.By.Models
plot(myBiomodRangeSize$Diff.By.Pixel)
