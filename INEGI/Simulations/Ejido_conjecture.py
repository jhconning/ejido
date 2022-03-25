## This file creates an ejido class and tries to find a nash belief equilibrium (not a technical term)
#%%
## imports
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from itertools import product

#%%


class Ejido:
    
    def __init__(self, 
                 c = 2,
                 N = 3,
                 mu = 1,
                 sigma = 3,
                 pe = 1,
                 pp =1):
        
        self.c = c
        self.N = N
        
        ## Prices
        self.pe = pe
        self.pp = pp
        

        ## Gamma parameters
        self.mu, self.sigma = mu, sigma
        
    def b(self):
        """
        Return skill distribution for farmers, 
        with lognormal distribution 
        and N farmers
        """
        np.random.seed(12324)
        
        return np.sort(np.random.lognormal(mean = self.mu, 
                                   sigma=self.sigma
                                   , size = self.N))
            
        
    def _profit(self, p, a, b):
        """
        Profit function which is the result of optimization problem
        from paper with:
        p: price
        c: marginal cost
        b: skill or average skill (depending on choice)
        a: share of profits (a=1 for private, and 1/N for ejido)
        """
        return a*(0.5)*(p - self.c + b)**2
    
    
    def profit_priv(self, b):
        """
        Profits with a=1
        """
        
        return self._profit(self.pp, 1, b)
    
    def profit_ejido(self, players, b):
        """
        Profits with a = 1/N
        """
        
        return self._profit(self.pe, 1/players, b)
    
    def belief_combinations(self, phat, number):
        
        """
        Given a belief p_hat about staying in the ejido, 
        calculate the matrix for every player of possible decisions
        So for 3 players this would look like this:
        
        Skill   P1       P2           P3          P4
        -----  -----  ---------     ---------   --------
        1      p_hat  p_hat        (1-p_hat)   (1-p_hat)
        2      p_hat  (1-p_hat)    p_hat       (1-p_hat)
        
        This leads to a matrix which should be N by 2^N+ 1 
        (the extra 1 is for the skill column)
        """
        
        ## cartesian product of p and 1-p repeated N times
        cart_prod = product((phat, 1-phat), 
                             repeat= number)
        
        ## Get joint probabilities of each results
        prob = np.product(list(cart_prod), axis=1)
        
        stayers = np.array(list(product((1, 0), 
                             repeat= number)))
        
        return prob, stayers
    
    def expected_payoff(self, phat, player_index):
        
        prob, stayers = self.belief_combinations(phat, self.N-1)
        
        ## Get skill of the player we care about
        
        our_farmer = self.b()[player_index]
        
        
        ## Get rid of the skill we're thinking about
        other_farmers  = np.setdiff1d(self.b(), our_farmer)
        
        ## number of farmers left in ejido for each possibility
        farmers_left = (1 + np.sum(stayers, axis=1)[:, np.newaxis])
        
        average_skill =((stayers @ other_farmers[:, np.newaxis]) + our_farmer)/ farmers_left
        
        ## Get expected payoff for every possibility and average skill
        
        poss_payoff = prob[:, np.newaxis]*self.profit_ejido(farmers_left,average_skill)
        
        ## get expected payoff
        
        expected_payoff = np.sum(poss_payoff)
        
        return expected_payoff
    
    def choice(self, phat, player_index):
        
        expected_payoff = self.expected_payoff(phat, player_index)
        
        ## Check if leaving is a better option
        if self.profit_priv(self.b()[player_index]) >= expected_payoff:
            choice = 0
        else:
            choice = 1
        
        return choice
    
    def nash(self):
        
        """
        Finds the realized proportion of ejidatarios
        staying in the ejido that is equal to belief
        
        Set up as finding distance smaller than some
        `eps`
        """
        
        ## Deviation
        eps = .001
        
        ## Set up belief array
        beliefs = np.arange(0,1, eps)
        
        ## Make array of realized proportion given belief
        prop = np.zeros(shape = len(beliefs))
        
            
        for j, belief in enumerate(beliefs):
            
            per_belief_prop = 0
            
            for i in range(self.N):

                choice = self.choice(belief, i)
                per_belief_prop += choice

            prop[j] = per_belief_prop/self.N
            
        nash = prop[abs(prop - beliefs)<eps]
            
        opt_choice = np.zeros(( len(nash), self.N))

        for j, eq in enumerate(nash):
        
            for i in range(self.N):
                
                opt_choice[j, i] = self.choice(eq, i)
               
        return nash, opt_choice, prop, beliefs
            
      
##### Landlord Class that optimizes over ejido decision

class Processor(Ejido):
    
    def __init__(self,
                 c = 2,
                 N = 3,
                 mu = 1,
                 sigma = 3,
                 pe = 300,
                 pp =100,
                 pr = 1000):
        
        super(Processor, self).__init__(c,
                                        N,
                                        mu,
                                        sigma,
                                        pe,
                                        pp)
                                
        self.pr = pr
        
    def profits_proc(self, b):
        
        """
        Define optimal pricing decision for ejido and private farmer
        for landlord
        
        Landlord chooses lowest price in market and buys from that member
        """
        opt_price = (0.5)*(self.pr + self.c - b)
        
        profits = (self.pr- opt_price)*(opt_price - self.c + b)
        
        return opt_price, profits
        
    
    def optimal_price_choice(self):  
        
        """
        Gets results of nash and gets lowest optimal price 
        and computes profits
        """
        
        nash , ejido_choice , _ , _ =self.nash()  
        
        max_profits = []
        
        for i, eq in enumerate(nash):
            
            ejido_skill_average = (self.b()@ejido_choice[i])/(self.N*eq)
            
            private_skill = self.b() @ np.diag(1- ejido_choice[i])
            
            private_skill = private_skill[private_skill>0]
            
            ### Get highest profit for each
            
            ejido_price, ejido_profit = self.profits_proc(ejido_skill_average)
            
        
            private_profits = []
        
            for skill in private_skill:
                
                private_price, private = self.profits_proc(skill)
                
                private_profits.append(private)
                
            all_profits = [ejido_profit] + private_profits

            max_profits.append(np.where(all_profits == max(all_profits),1,0))
            
        return max_profits, all_profits
    

            
        
        
        
        
              
    


#%%

e = Ejido()
e.expected_payoff(0,0)


