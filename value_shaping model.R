#Simulate data
vsSimulator <- function(etaVec, nAgents = 4, trials = 25, rounds = 1, alpha = .9, beta = 1, Q0 = 0){
  vsDF <- data.frame() #initialize dataframe
  for (r in 1:rounds){ #loop through rounds
    Qmat <- matrix(Q0,  nrow = nAgents, ncol = k) #Initialize Q-values in a matrix with one row per agent
    socialObs <- rep(NA, nAgents) #social observations
    for (t in 1:trials){ #loop through trials
      for (agent in 1:nAgents){ #loop through agents
        socialFreq <- table(factor(socialObs[-agent], levels = 1:k)) + .0001 #Frequency of each socially observed action + a very small number
        Qmat[agent,] <- Qmat[agent,] + (etaVec[agent]*socialFreq) #add value bonus
        p <- softmax(beta, Qmat[agent,]) #compute policy
        action <- sample(1:k,size = 1, prob=p) #sample action
        reward <- banditGenerator(action) #generate reward
        Qmat[agent,action] <- Qmat[agent,action] + alpha*(reward - Qmat[agent,action]) #update q-values
        chosen <- rep(0, k) #create an index for the chosen option
        chosen[action]<- 1 #1 = chosen, 0 not
        socialObs[agent] <- action #update social observation vector
        #save data
        trialDF <- data.frame(trial = t, round = r,agent = agent,  reward = reward, action = action, eta = etaVec[agent])
        vsDF <- rbind(vsDF,trialDF)
      }
    } 
  }
  return(vsDF)
}

etaVec <-rexp(4) #value bonus parameter; let's sample this from a exponential distribution
vsDF <- vsSimulator(etaVec) #run simulation

#We can also run this for several rounds of the task, which will be necessary for model fitting. The commented out lines of code will be used to simulate data for Tutorial 3
#vsDF <- vsSimulator(etaVec, rounds = 10) #run simulation
#saveRDS(vsDF,'data/simChoicesVSagent.Rds') #save data

ggplot(vsDF, aes(x = trial, y = reward, color = factor(agent)))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"), name = 'Agent')+
  theme(legend.position=c(1,0.1), legend.justification = c(1,0))+
  labs(x = 'Trial', y = 'Reward', title = 'Value-shaping agents')