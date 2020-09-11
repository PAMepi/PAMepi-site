
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
# import warnings #reticulate#
# warnings.filterwarnings('ignore') #reticulate#




class compartimental_models:
    def __init__(self):
        pass

    def r0(self):
        return self.beta/self.gamma

class start_model(compartimental_models):
    def __init__(self, pop):
        self.pop = pop
    
    #Defining the Model
    def __sir(self, f ,t, parametros):
    
        #parameters
        b, gamma = parametros
    
        #variables
        S, I, R, Nw = f
        #S = f[0]
        #I = f[1]
        #R = f[2]
        #Nw = f[3]
    

        #equations
        dNw_dt = b * S * I
        dS_dt = - b * S * I
        dI_dt = b * S * I - gamma * I
        dR_dt = gamma * I 
    
        #Returning the derivatives
        return [dS_dt, dI_dt, dR_dt, dNw_dt]

    
    #Calculating the Error
    def __least_square_error(self,pars, ts0, y):

        y = self.y
    
        #assaing the Least square given parameters to the numerical integration for the error calculation
        b, gamma, i0 = pars
    
        #initial conditions
        q0 = [1-i0,i0,0,i0]
    
        #parameters to feed the E.D.Os
        parode = b, gamma
    
        #Integrating
        qs = odeint(self.__sir, q0, ts0, args = (parode,),mxstep = 1000000)

        #sinf the epidemic curve
        #sdth the death curve

        sinf = qs[:,-1]

        #define the standardized residuals
        erri = (self.pop * sinf - y) / np.sqrt(self.pop * sinf + 1.0)
    
        return np.r_[erri]

    
    def fit(self, x, y, n_tries, bounds = [[0., 2.0], [1/14, 1/5]]):
        self.x = x
        self.y = y
        #
        ts0 = np.arange(1, len(self.x) + 1)
    
        #Convert bounds to array
        bounds.append([0,50/self.pop])
        bounds = np.array(bounds)
      
       

        #Add infinity to try to avoid local
        self.best_res = None
        best_cost = np.inf
        for i in range(n_tries):
        
            #create a set of ramdom parameters
            par0 = np.random.rand(len(bounds))
        
            #Limit those parameters to the interval defined
            par0 = bounds[:,0] + par0 * (bounds[:,1] - bounds[:,0])
        
            res = optimize.least_squares(lambda pars: self.__least_square_error(pars, ts0, self.y), par0, bounds = (bounds[:,0],bounds[:,1]))
        
            if res.cost < best_cost:
                best_cost = res.cost
                self.best_res = res
        
        self.beta, self.gamma, self.i0 = self.best_res.x
    

    def predict(self,time):
        
        q0 = [1 - self.i0, self.i0,0, self.i0]

        #pick best parameters
        parode = self.beta, self.gamma
        predicted = odeint(self.__sir, q0, np.arange(1, len(time) + 1), args = (parode,), mxstep = 1000000)

        self.S = predicted[:,0]
        self.I = predicted[:,1]
        self.R = predicted[:,2]
        self.Nw = predicted[:,3]

        #Compute cases
        return self.Nw * self.pop


    
        #Add infinity to try to avoid local
        self.best_res = None
        best_cost = np.inf
        
        for i in range(n_tries):
        
            #create a set of ramdom parameters
            par0 = np.random.rand(len(bounds))
        
            #Limit those parameters to the interval defined
            par0 = bounds[:,0] + par0 * (bounds[:,1] - bounds[:,0])
            
        
            res = optimize.least_squares(lambda pars: self.__least_square_error_bv(pars, ts0, self.y), par0, bounds = (bounds[:,0],bounds[:,1]))
        
            if res.cost < best_cost:
                best_cost = res.cost
                self.best_res = res
        
        self.beta, self.beta1, self.gamma, self.t1, self.i0 = self.best_res.x
        #note to transform t into the correct date use int(t1 - 1)
    
    def predict(self, time):
        
        q0 = [1 - self.i0, self.i0,0, self.i0]

        #pick best parameters
        parode = self.beta, self.beta1, self.gamma, self.t1
        predicted = odeint(self.__sir, q0, np.arange(1, len(time) + 1), args = (parode,), mxstep = 1000000)

        self.S = predicted[:,0]
        self.I = predicted[:,1]
        self.R = predicted[:,2]
        self.Nw = predicted[:,3]

        #Compute cases
        return self.Nw * self.pop




class start_model_bv(compartimental_models):
    def __init__(self, pop):
        self.pop = pop
    
    #Defining the Steap Function 

    def __H(self,t):
        h = 1.0/(1.0+ np.exp(-2.0 * 50 * t))
        return h


    #Defining the Beta(t)
    def __beta(self, t, t1, b, b1):
        beta = b * self.__H(t1 - t) + b1 * self.__H(t - t1) 
        return beta
    

    #Defining the Model
    def __sir(self, f, t, parametros):
    
        #parameters
        b, b1, gamma, t1 = parametros
        
        
        #variables
        S, I, R, Nw = f
    
        #equations
        dNw_dt = self.__beta(t, t1, b, b1) * S * I
        
        dS_dt = - self.__beta(t, t1, b, b1) * S * I
        dI_dt = self.__beta(t, t1, b, b1) * S * I - gamma * I
        dR_dt = gamma * I 
    
        #Returning the derivatives
        return [dS_dt, dI_dt, dR_dt, dNw_dt]

    #Calculating the Error
    def __least_square_error_bv(self, pars, ts0, y):

        y = self.y
    
        #assaing the Least square given parameters to the numerical integration for the error calculation
        b, b1, gamma, t1, i0 = pars
    
        #initial conditions
        q0 = [1-i0, i0, 0, i0]
    
        #parameters to feed the E.D.Os
        parode = b, b1, gamma, t1
    
        #Integrating
        qs = odeint(self.__sir, q0, ts0, args = (parode,),mxstep=1000000)
        

        #sinf the epidemic curve
        #sdth the death curve

        sinf = qs[:,-1]

        #define the standardized residuals
        erri = (self.pop * sinf - y) / np.sqrt(self.pop * sinf + 1.0)
    
        return np.r_[erri]
    
    def fit(self, x, y, n_tries, bounds = [[0., 2.0], [0., 2.0], [1/14, 1/5], [15,45]]):
        self.x = x
        self.y = y
        #
        ts0 = np.arange(1, len(self.x) + 1)
    
        #Convert bounds to array
        bounds.append([0,50/self.pop])
        bounds = np.array(bounds)
        
      
    
        #Add infinity to try to avoid local
        self.best_res = None
        best_cost = np.inf
        
        for i in range(n_tries):
        
            #create a set of ramdom parameters
            par0 = np.random.rand(len(bounds))
        
            #Limit those parameters to the interval defined
            par0 = bounds[:,0] + par0 * (bounds[:,1] - bounds[:,0])
            
        
            res = optimize.least_squares(lambda pars: self.__least_square_error_bv(pars, ts0, self.y), par0, bounds = (bounds[:,0],bounds[:,1]))
        
            if res.cost < best_cost:
                best_cost = res.cost
                self.best_res = res
        
        self.beta, self.beta1, self.gamma, self.t1, self.i0 = self.best_res.x
        #note to transform t into the correct date use int(t1 - 1)
    
    def predict(self, time):
        
        q0 = [1 - self.i0, self.i0,0, self.i0]

        #pick best parameters
        parode = self.beta, self.beta1, self.gamma, self.t1
        predicted = odeint(self.__sir, q0, np.arange(1, len(time) + 1), args = (parode,), mxstep = 1000000)

        self.S = predicted[:,0]
        self.I = predicted[:,1]
        self.R = predicted[:,2]
        self.Nw = predicted[:,3]

        #Compute cases
        return self.Nw * self.pop
       
       
def run_sir(vector, pop):

    #Start model
    model = start_model_bv(pop = pop)
    time = np.arange(1, len(vector) + 1)
    #fit model
    model.fit(x = time,
              y = np.array(vector),
              n_tries = 20)
    model.predict(time)

    return pop * np.array(vector)
