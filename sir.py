
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on Tue Aug 20 2020
@author: Juliane Oliveira julianlanzin@gmail.com 
@author: Moreno rodrigues rodriguesmsb@gmail.com

"""


from scipy.integrate import odeint
from scipy import optimize
from scipy.integrate import odeint
from scipy.optimize import least_squares
import numpy as np
import warnings
warnings.filterwarnings('ignore')




class start_model:
    def __init__(self, pop):
        self.pop = pop
        pass

    #defining a setp function
    def __h(self,t):
        h = 1.0/(1.0 + np.exp(-2.0 * 50 * t))
        return h
    
    #defing a function to compute one change in beta
    def __beta_t(self, t, t1, b, b1):
        beta = b * self.__h(t1 - t) + b1 * self.__h(t - t1) 
        return beta
    
    #defing a function to compute two changes in beta
    def __beta_t2(self, t, t1, t2, b, b1, b2):
        beta = b * self.__h(t1 - t) + b1 * self.__h(t2 - t) * self.__h(t - t1) + b2 * self.__h(t - t2)
        return beta

    #defing SIR model
    def __sir(self, f, t, parametros):

        n_betas = self.n_betas
        #variables
        S, I, R, Tt = f
    
    
        #create a block that leads with number of betas
        if n_betas == 3:
        
            #define paramters for model with 3 betas
            beta, beta1, beta2, gamma, t1, t2 = parametros
        
            #define derivatives for 3 betas
            dS_dt = - self.__beta_t2(t, t1, t2, beta, beta1, beta2) * S * I
            dTt_dt = self.__beta_t2(t, t1, t2, beta, beta1, beta2) * S * I
            dI_dt = self.__beta_t2(t, t1, t2, beta, beta1, beta2) * S * I - gamma * I
    
        #Change here to lead with more betas
        elif n_betas == 2:
        
            #define parameters for two betas
            beta, beta1, gamma, t1 = parametros
        
            #define derivatives for two betas
            dS_dt = - self.__beta_t(t, t1, beta, beta1) * S * I
            dTt_dt = self.__beta_t(t, t1, beta, beta1) * S * I
            dI_dt = self.__beta_t(t, t1, beta, beta1) * S * I - gamma * I
    
        
        elif n_betas == 1:
            #define parameters for one beta
            beta, gamma = parametros
        
            #define derivatives for single beta
            dS_dt = - beta * S * I
            dTt_dt = beta * S * I
            dI_dt = beta * S * I - gamma * I
    
    
    
        #return to derivatives that are common to all models
        dR_dt = gamma*I 
        
        return dS_dt, dI_dt, dR_dt, dTt_dt

    #Define the minimizer function
 
    def fit(self, x, y, n_tries, n_betas, fit_by = "cs", bounds = {"beta":  [0,2.0], 
                                                                                  "beta1": [0,2.0],
                                                                                  "beta2": [0,2.0],
                                                                                  "gamma": [1/14,1/7],
                                                                                  "t1": [0,45],
                                                                                  "t2": [50,100]}):

        self.n_betas = n_betas
        self.y = y
        self.x = x
        self.fit_by = fit_by
    
        def least_square_error(pars, ts0):
        
            #Define the number of parameters that will be used to pars according to the number of betas
            if self.n_betas == 3:
                beta, beta1, beta2, gamma, t1, t2,  i0 = pars
                parode = beta, beta1, beta2, gamma, t1, t2
            
            elif self.n_betas == 2:
                beta, beta1, gamma, t1,  i0 = pars
                parode = beta, beta1, gamma, t1
            
            else:
                beta, gamma,  i0 = pars
                parode = beta, gamma
            
            
            #define initial conditions
            q0 = [1-i0,i0,0,i0]
    
    
            #Integrating
            qs = odeint(self.__sir, q0, ts0, args = (parode, ), mxstep = 1000000)
        
            

            #define the standardized residuals
            if self.fit_by == "cs":
                #get the series of cummulative cases to minimize error
                sinf = qs[:,-1]

            elif self.fit_by == "ts":
                #get the series of daily cases to minimize error
                sinf = np.r_[qs[:,-1][0], np.diff(qs[:,-1])]

            erri = (self.pop * sinf - self.y) / np.sqrt(self.pop * sinf + 1.0)
    
            return np.r_[erri]
         
        ts0 = np.arange(1, len(self.x) + 1)
    
    
        #change the bounds according wtih number of betas
        if self.n_betas == 3:
            bounds = list(bounds.values())
        elif self.n_betas == 2:
            for parameter in ["beta2", "t2"]:
                del(bounds[parameter])
            bounds = list(bounds.values())
        else:
            #remove parameters that will note be used for the model
            for parameter in ["beta1", "beta2", "t1", "t2"]:
                del(bounds[parameter])
            bounds = list(bounds.values()) 
    
    
        #Add bounds that are common to all models
        bounds.append([0,50/self.pop]) #i0
        bounds = np.array(bounds)
    
        #start to variables to track best results during optmization process
        best_res = None
        best_cost = np.inf
    
        for i in range(n_tries):
        
            #create a set of ramdom parameters
            par0 = np.random.rand(len(bounds))
        
            #Limit those parameters to the interval defined
            par0 = bounds[:,0] + par0 * (bounds[:,1] - bounds[:,0])
            
            try:
                res = optimize.least_squares(lambda pars: least_square_error(pars, ts0), par0, bounds = (bounds[:,0],bounds[:,1]))
        
                if res.cost < best_cost:
                    best_cost = res.cost
                    best_res = res
            except:
                pass
            
        #Define the dict wtih the parameters that will be returned by model
        if self.n_betas == 3:
            self.beta = best_res.x[0]
            self.beta1 = best_res.x[1]
            self.beta2 = best_res.x[2] 
            self.gamma = best_res.x[3] 
            self.t1 = best_res.x[4]
            self.t2 = best_res.x[5] 
            self.i0  = best_res.x[6]
            

        elif self.n_betas == 2:
            self.beta = best_res.x[0] 
            self.beta1 = best_res.x[1] 
            self.gamma  = best_res.x[2]
            self.t1 = best_res.x[3]
            self.i0 = best_res.x[4]
           
        else:
            self.beta = best_res.x[0]
            self.gamma = best_res.x[1]
            self.i0 = best_res.x[2]

    def get_parameters(self):
        if self.n_betas == 3:
            return({"beta": self.beta, "beta1": self.beta1, "beta2": self.beta2,
                   "gamma": self.gamma, "t1": self.t1, "t2": self.t2, "i0": self.i0})

        elif self.n_betas == 2:
            return({"beta": self.beta, "beta1": self.beta1, "gamma": self.gamma, "t1": self.t1, "i0": self.i0})
           
        else:
            return({"beta": self.beta, "gamma": self.gamma, "i0": self.i0})
    
    def predict(self, time):
        q0 = [1 - self.i0, self.i0, 0, self.i0]
        if self.n_betas == 3:
            parode = self.beta, self.beta1, self.beta2, self.gamma, self.t1, self.t2
        elif self.n_betas == 2:
            parode = self.beta, self.beta1, self.gamma, self.t1
        else:
            parode = self.beta, self.gamma

        predicted = odeint(self.__sir, q0, np.arange(1, len(time) + 1), args = (parode,), mxstep = 1000000)
        self.S = predicted[:,0]
        self.I = predicted[:,1]
        self.R = predicted[:,2]

      
        if self.fit_by == "cs":

            #predict the series for cummulative cases
            self.Tt = predicted[:,3]

        elif self.fit_by == "ts":
            #predict the series for daily cases
            self.Tt = np.r_[predicted[:,3][0], np.diff(predicted[:,3])]
        
        return {"S": self.S, "I": self.I, "R": self.R, "Tt": self.Tt * self.pop}





   
    



    
