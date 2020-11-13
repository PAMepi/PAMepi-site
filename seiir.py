
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on Tue Oct 07 2020
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

    #defing SEIR model
    def __seiir(self, f, t, parametros):

        n_betas = self.n_betas
        #variables
        S, E, Ia, Is, R, Tt = f
    
    
        #create a block that leads with number of betas
        if n_betas == 3:
        
            #define paramters for model with 3 betas
            beta, beta1, beta2, delta, rho, kappa, gammaA, gammaS, t1, t2 = parametros
        
            #define derivatives for 3 betas
            dS_dt = - self.__beta_t2(t, t1, t2, beta, beta1, beta2) * S * (Is + delta * Ia)
            dE_dt = self.__beta_t2(t, t1, t2, beta, beta1, beta2) * S * (Is + delta * Ia) - kappa * E
    
        #Change here to lead with more betas
        elif n_betas == 2:
        
            #define parameters for two betas
            beta, beta1, delta, rho, kappa, gammaA, gammaS, t1 = parametros
        
            #define derivatives for two betas
            dS_dt = - self.__beta_t(t, t1, beta, beta1) * S * (Is + delta * Ia)
            dE_dt = self.__beta_t(t, t1, beta, beta1) * S * (Is + delta * Ia) - kappa * E
        
        else:
            #define parameters for one beta
            beta, delta, rho, kappa, gammaA, gammaS = parametros
        
            #define derivatives for single beta
            dS_dt = - beta * S * (Is + delta * Ia)
            dE_dt = beta * S * (Is + delta * Ia) - kappa * E
    
    
    
        #return to derivatives that are common to all models
        dTt_dt = rho * kappa * E
        dIa_dt = (1 - rho) * kappa * E - (gammaA * Ia)
        dIs_dt = rho * kappa * E - (gammaS * Is)
        dR_dt = gammaA * Ia + gammaS * Is
        
        return dS_dt, dE_dt, dIa_dt, dIs_dt, dR_dt, dTt_dt

    #Define the minimizer function
 
    def fit(self, x, y, n_tries, n_betas, fit_by = "cs", bounds = {"beta":  [0,2.0], 
                                                                    "beta1": [0,2.0],
                                                                    "beta2": [0,2.0],
                                                                    "delta": [0., 0.75],
                                                                    "rho": [0.13,0.5],
                                                                    "kappa": [1/6, 1/3],
                                                                    "gammaA": [1/3.7,1/3.24],
                                                                    "gammaS": [1/5,1/3],
                                                                    "t1": [0,45],
                                                                    "t2": [50,100]}):

        self.n_betas = n_betas
        self.y = y
        self.x = x
        self.fit_by = fit_by
    
        def least_square_error(pars, ts0):
        
            #Define the number of parameters that will be used to pars according to the number of betas
            if self.n_betas == 3:
                beta, beta1, beta2, delta, rho, kappa, gammaA, gammaS, t1, t2, e0, ia0, is0 = pars
                parode = beta, beta1, beta2, delta, rho, kappa, gammaA, gammaS, t1, t2
            
            elif self.n_betas == 2:
                beta, beta1, delta, rho, kappa, gammaA, gammaS, t1, e0, ia0, is0 = pars
                parode = beta, beta1, delta, rho, kappa, gammaA, gammaS, t1
            
            else:
                beta, delta, rho, kappa, gammaA, gammaS, e0, ia0, is0  = pars
                parode =  beta, delta, rho, kappa, gammaA, gammaS
            
            
            #define initial conditions
            q0 = [1-(is0 + ia0), e0, ia0, is0, 0, is0]
    
    
            #Integrating
            qs = odeint(self.__seiir, q0, ts0, args = (parode, ), mxstep = 1000000)
        
            #define the standardized residuals
            if self.fit_by == "cs":
                #get the series of cummulative cases to minimize error
                sinf = qs[:,-1]

            elif self.fit_by == "ts":
                #get the series of daily cases to minimize error
                sinf = np.r_[qs[:,-1][0], np.diff(qs[:,-1])]


            #define the standardized residuals
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
        bounds.append([0,50/self.pop])
        bounds.append([0,50/self.pop])
        bounds.append([0,50/self.pop])
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
            self.delta = best_res.x[3]  
            self.rho = best_res.x[4]  
            self.kappa = best_res.x[5]  
            self.gammaA = best_res.x[6]  
            self.gammaS = best_res.x[7]  
            self.t1 = best_res.x[8]  
            self.t2 = best_res.x[9]  
            self.e0 = best_res.x[10]  
            self.ia0 = best_res.x[12] 
            self.is0 = best_res.x[13]

        elif self.n_betas == 2:
            self.beta = best_res.x[0] 
            self.beta1 = best_res.x[1]  
            self.delta = best_res.x[2]  
            self.rho = best_res.x[3]  
            self.kappa = best_res.x[4]  
            self.gammaA = best_res.x[5]  
            self.gammaS = best_res.x[6]  
            self.t1 = best_res.x[7]  
            self.e0 = best_res.x[8]  
            self.ia0 = best_res.x[9]  
            self.is0 = best_res.x[10]
           
        else:
            self.beta = best_res.x[0]  
            self.delta = best_res.x[1]  
            self.rho = best_res.x[2]  
            self.kappa = best_res.x[3]  
            self.gammaA = best_res.x[4]  
            self.gammaS = best_res.x[5]  
            self.e0 = best_res.x[6]  
            self.ia0 = best_res.x[7]  
            self.is0 = best_res.x[8]

    def get_parameters(self):
        if self.n_betas == 3:
            return({"beta": self.beta, "beta1": self.beta1, "beta2": self.beta2, "delta": self.delta, "rho": self.rho, "kappa": self.kappa, 
                    "gammaA": self.gammaA, "gammaS": self.gammaS, "t1": self.t1, "t2": self.t2, "e0": self.e0, "ia0": self.ia0, "is0": self.is0})

        elif self.n_betas == 2:
            return({"beta": self.beta, "beta1": self.beta1, "delta": self.delta, "rho": self.rho, "kappa": self.kappa, 
                    "gammaA": self.gammaA, "gammaS": self.gammaS, "t1": self.t1, "e0": self.e0, "ia0": self.ia0, "is0": self.is0})
           
        else:
            return({"beta": self.beta, "delta": self.delta, "rho": self.rho, "kappa": self.kappa, 
                    "gammaA": self.gammaA, "gammaS": self.gammaS, "e0": self.e0, "ia0": self.ia0, "is0": self.is0})
    
    def predict(self, time):
        q0 = [1 - (self.is0 + self.ia0), self.e0, self.ia0, self.is0, 0, self.is0]
        if self.n_betas == 3:
            parode = self.beta, self.beta1, self.beta2, self.delta, self.rho, self.kappa, self.gammaA, self.gammaS, self.t1, self.t2
        elif self.n_betas == 2:
            parode = self.beta, self.beta1, self.delta, self.rho, self.kappa, self.gammaA, self.gammaS, self.t1
        else:
            parode = self.beta, self.delta, self.rho, self.kappa, self.gammaA, self.gammaS

        predicted = odeint(self.__seiir, q0, np.arange(1, len(time) + 1), args = (parode,), mxstep = 1000000)
        self.S = predicted[:,0]
        self.E = predicted[:,1]
        self.Ia = predicted[:,2]
        self.Is = predicted[:,3]
        self.R = predicted[:,4]
        
        if self.fit_by == "cs":

            #predict the series for cummulative cases
            self.Tt = predicted[:,5]

        elif self.fit_by == "ts":
            #predict the series for daily cases
            self.Tt = np.r_[predicted[:,5][0], np.diff(predicted[:,5])]

        return {"S": self.S, "E":self.E, "Ia": self.Ia, "Is": self.Is, "R": self.R, "Tt": self.Tt * self.pop}





   
    



    
