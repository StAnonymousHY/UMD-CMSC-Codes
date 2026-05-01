""" Bayesian networks """

from probability import BayesNet, enumeration_ask, elimination_ask, rejection_sampling, likelihood_weighting, gibbs_ask
from timeit import timeit, repeat
import pickle
import numpy as np

T, F = True, False

class DataPoint:
    """
    Represents a single datapoint gathered from one lap.
    Attributes are exactly the same as described in the project spec.
    """
    def __init__(self, muchfaster, early, overtake, crash, win):
        self.muchfaster = muchfaster
        self.early = early
        self.overtake = overtake
        self.crash = crash
        self.win = win

def get_prob_true(target_attr, data, parent_attrs=[], parent_values=()):
    matching_data = []
    for d in data:
        match = True
        for attr, value in zip(parent_attrs, parent_values):
            if getattr(d, attr) != value:
                match = False
                break
        if match:
            matching_data.append(d)
    true_count = sum(1 for d in matching_data if getattr(d, target_attr))
    return true_count / len(matching_data)

def generate_bayesnet():
    """
    Generates a BayesNet object representing the Bayesian network in Part 2
    returns the BayesNet object
    """
    bayes_net = BayesNet()
    # load the dataset, a list of DataPoint objects
    data = pickle.load(open("data/bn_data.p","rb"))
    # BEGIN_YOUR_CODE ######################################################
    bayes_net = BayesNet()
    # load the dataset, a list of DataPoint objects
    data = pickle.load(open("data/bn_data.p","rb"))
    p_muchfaster = get_prob_true("muchfaster", data)
    p_early = get_prob_true("early", data)
    overtake_cpt = {}
    for muchfaster in [T, F]:
        for early in [T, F]:
            overtake_cpt[(muchfaster, early)] = get_prob_true("overtake", data, ["muchfaster", "early"], (muchfaster, early))
    crash_cpt = {}
    for muchfaster in [T, F]:
        for early in [T, F]:
            crash_cpt[(muchfaster, early)] = get_prob_true("crash", data, ["muchfaster", "early"], (muchfaster, early))
    win_cpt = {}
    for overtake in [T, F]:
        for crash in [T, F]:
            win_cpt[(overtake, crash)] = get_prob_true("win", data, ["overtake", "crash"], (overtake, crash))
    bayes_net.add(("MuchFaster", "", p_muchfaster))
    bayes_net.add(("Early", "", p_early))
    bayes_net.add(("Overtake", "MuchFaster Early", overtake_cpt))
    bayes_net.add(("Crash", "MuchFaster Early", crash_cpt))
    bayes_net.add(("Win", "Overtake Crash", win_cpt))
    
    # END_YOUR_CODE ########################################################
    return bayes_net

def find_best_overtake_condition(bayes_net):
    """
    Finds the optimal condition for overtaking the car, as described in Part 3
    Returns the optimal values for (MuchFaster,Early)
    """
    # BEGIN_YOUR_CODE ######################################################
    best_condition = None
    best_prob = -1
    for muchfaster in [T, F]:
        for early in [T, F]:
            evidence = {"MuchFaster": muchfaster, "Early": early}
            p_no_crash = enumeration_ask("Crash", evidence, bayes_net)[F]
            p_win_given_no_crash = enumeration_ask("Win", {"MuchFaster": muchfaster, "Early": early, "Crash": F}, bayes_net)[T]
            prob = p_no_crash * p_win_given_no_crash
            if prob > best_prob:
                best_prob = prob
                best_condition = (muchfaster, early)

    return best_condition
    # END_YOUR_CODE ########################################################

def main():
    bayes_net = generate_bayesnet()
    cond = find_best_overtake_condition(bayes_net)
    print("Best overtaking condition: MuchFaster={}, Early={}".format(cond[0],cond[1]))

if __name__ == "__main__":
    main()

