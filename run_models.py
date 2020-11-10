from JAS.models.gradient_optmization import sir, seir, seiir
import numpy as np 

def run_sir(vector, pop, n_betas):
    model = sir.start_model(pop = pop)
    time = np.arange(1, len(vector) + 1)
    model.fit(x = time,
              y = vector,
              n_tries = 10,
              n_betas = n_betas)
    results = model.predict(np.arange(1, len(vector) + 10))
    
    return  results["Tt"]



def run_seir(vector, pop, n_betas):
    model = seir.start_model(pop = pop)
    time = np.arange(1, len(vector) + 1)
    model.fit(x = time,
              y = vector,
              n_tries = 10,
              n_betas = n_betas)
    results = model.predict(np.arange(1, len(vector) + 10))
    
    return  results["Tt"]


def run_seiir(vector, pop, n_betas):
    model = seiir.start_model(pop = pop)
    time = np.arange(1, len(vector) + 1)
    model.fit(x = time,
              y = vector,
              n_tries = 10,
              n_betas = n_betas)
    results = model.predict(np.arange(1, len(vector) + 10))
    
    return  results["Tt"]




# vector = np.array([10,20,30,40,50,60,70,80])
# pop = 10000
# n_betas  = 2

# print(run_seiir(vector=vector, pop =pop, n_betas = n_betas))

