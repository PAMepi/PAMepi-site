from JAS.models.gradient_optmization import sir, seir, seiir
#import sir, seir, seiir
import numpy as np 

def run_sir(vector, pop, n_betas):
    model = sir.start_model(pop = pop)
    time = np.arange(1, len(vector) + 1)
    model.fit(x = time,
              y = np.array(vector), # OLHA AQUI
              n_tries = 10,
              n_betas = n_betas,
              fit_by = "cs",
              bounds = {"beta":  [0,2.0], "beta1": [0,2.0],"beta2": [0,2.0],
                        "gamma": [1/14,1/7],"t1": [0,45], "t2": [50,100]})
    results = model.predict(np.arange(1, len(vector) + 1))
    
    return  results["Tt"]



def run_seir(vector, pop, n_betas):
    #print(vector)
    model = seir.start_model(pop = pop)
    time = np.arange(1, len(vector) + 1)
    model.fit(x = time,
              y = np.array(vector),
              n_tries = 10,
              n_betas = n_betas,
              fit_by = "cs",
              bounds = {"beta":  [0,2.0], "beta1": [0,2.0], "beta2": [0,2.0],
                        "kappa": [1/6, 1/3], "gamma": [1/14,1/7], "t1": [0,45],
                        "t2": [50,100]})
    results = model.predict(np.arange(1, len(vector) + 1))
    
    return  results["Tt"]


def run_seiir(vector, pop, n_betas):
    model = seiir.start_model(pop = pop)
    time = np.arange(1, len(vector) + 1)
    model.fit(x = time,
              y = vector,
              n_tries = 10,
              n_betas = n_betas,
              fit_by = "cs",
              bounds = {"beta":  [0,2.0], "beta1": [0,2.0], "beta2": [0,2.0],
                        "delta": [0., 0.75], "rho": [0.13,0.5], "kappa": [1/6, 1/3],
                        "gammaA": [1/3.7,1/3.24], "gammaS": [1/5,1/3], "t1": [0,45],
                        "t2": [50,100]})
    results = model.predict(np.arange(1, len(vector) + 1))
    
    return  results["Tt"]




# vector = np.array([10,20,30,40,50,60,70,80])
# pop = 10000
# n_betas  = 2

# print(run_seiir(vector=vector, pop =pop, n_betas = n_betas))

