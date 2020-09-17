
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on Tue Aug 20 2020
@author: Moreno rodrigues rodriguesmsb@gmail.com

"""
import numpy as np
from scipy.integrate import solve_ivp, odeint
from scipy import optimize
from scipy.optimize import least_squares


def H(t):
    h = 1.0/(1.0+ np.exp(-2.0 * 50 * t))
    return h


#Defining the Beta(t)
def beta(t, t1, b, b1):
    beta = b * H(t1 - t) + b1 * H(t - t1) 
    return beta
    

#Defining the Model
def sir(f, t, parametros):
    
    #parameters
    b, b1, gamma, t1 = parametros
        
        
    #variables
    S, I, R, Nw = f
    
    #equations
    dNw_dt = beta(t, t1, b, b1) * S * I
        
    dS_dt = - beta(t, t1, b, b1) * S * I
    dI_dt = beta(t, t1, b, b1) * S * I - gamma * I
    dR_dt = gamma * I 
    
    #Returning the derivatives
    return [dS_dt, dI_dt, dR_dt, dNw_dt]

#Calculating the Error
def fit(x, y, pop, n_tries, bounds = [[0., 2.0], [0., 2.0], [1/14, 1/5], [15,45]]):
    
    def least_square_error_bv(pars, ts0, y, pop):

        #assaing the Least square given parameters to the numerical integration for the error calculation
        
        b = pars[0] 
        b1 = pars[1]  
        gamma = pars[2] 
        t1 = pars[3] 
        i0 = pars[4]
    
        #initial conditions
        q0 = [1-i0, i0, 0, i0]
    
        #parameters to feed the E.D.Os
        parode = b, b1, gamma, t1
    
        #Integrating
        qs = odeint(sir, q0, ts0, args = (parode,),mxstep=1000000)
        

        #sinf the epidemic curve
        #sdth the death curve

        sinf = qs[:,-1]

        #define the standardized residuals
        erri = (pop * sinf - y) / np.sqrt(pop * sinf + 1.0)
    
        return np.r_[erri]
 
        
    ts0 = np.arange(1, len(x) + 1)
    
    #Convert bounds to array
    bounds.append([0,50/pop])
    bounds = np.array(bounds)
        
      
    
    #Add infinity to try to avoid local
    best_res = None
    best_cost = np.inf
        
    for i in range(n_tries):
        
        #create a set of ramdom parameters
        par0 = np.random.rand(len(bounds))
        
        #Limit those parameters to the interval defined
        par0 = bounds[:,0] + par0 * (bounds[:,1] - bounds[:,0])
            
        
        res = optimize.least_squares(lambda pars: least_square_error_bv(pars, ts0, y, pop), par0, bounds = (bounds[:,0],bounds[:,1]))
        
        if res.cost < best_cost:
            best_cost = res.cost
            best_res = res
        
    beta = best_res.x[0] 
    beta1 = best_res.x[1]  
    gamma = best_res.x[2]  
    t1 = best_res.x[3]  
    i0 = best_res.x[4]

    return {"beta": beta, "beta1": beta1, "gamma": gamma, "t1":t1, "i0": i0, "pop": pop}

def predict(time, pars):
    i0 = pars["i0"]
    beta = pars["beta"]
    beta1 = pars["beta1"]
    gamma = pars["gamma"]
    t1 = pars["t1"]
    i0 = pars["i0"]
    pop = pars["pop"]
    
    q0 = [1 - i0, i0,0, i0]

    #pick best parameters
    parode = beta, beta1, gamma, t1
    predicted = odeint(sir, q0, np.arange(1, len(time) + 1), args = (parode,), mxstep = 1000000)

    S = predicted[:,0]
    I = predicted[:,1]
    R = predicted[:,2]
    Nw = predicted[:,3]

    #Compute cases
    return {"S": S, "I": I, "R": R, "Nw": Nw * pop}




def run_sir(vector, pop):
    time = np.arange(1, len(vector) + 1)
    pars  = fit(x = time,
		y = vector,
                pop = pop,
             	n_tries = 20)
    series = predict(time = time, pars = pars)
    return series["Nw"]


#print(run_sir(vector = [10,20,30,40,50,60], pop = 17000))
#print(run_sir(vector = [10,20,30,40,50,70], pop = 17000))
