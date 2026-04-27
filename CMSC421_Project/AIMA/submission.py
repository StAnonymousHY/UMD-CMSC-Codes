"""
CMSC 421 — Project #2: Planning Graphs and SAT Planning
--------------------------------

# ==============================================================
#  Preface to Students
# ==============================================================

You are expected to take some time to explore the repository in order
to understand the syntax and structure of the provided codebase. This
is an excellent opportunity to practice reading and reasoning about
existing code — a skill that is crucial in research and industry alike,
where you'll often need to extend or modify large, mature projects.

The most relevant files to review before beginning this part are:
    • planning.py
    • tests/test_planning.py
    • tests/test_graphplan.py

# ==============================================================
"""

import pytest
from planning import *
from logic import *
import matplotlib
from matplotlib import pyplot as plt

################################################################
# PART 1.1: Implement Actions & Domain Components
################################################################
"""
You must implement the necessary remaining three actions:
 - 'MoveLeft(c, frm, to)'
 - 'MoveUp(c, frm, to)'
 - 'MoveDown(c, frm, to)'
Be careful - this code is case sensitive.

Next, implement the 'AdjacentUp' and 'AdjacentDown' domain 
definitions. We have implemented 'AdjacentLeft' and
'AdjacentRight' programatically. You can define a similar
generator, or hardcode the relations manually.

At this point, the rush_hour_4x4 function should return a
PlanningProblem object successfully.

Ensure your solution to this is correct, as the following
parts build upon this function.
"""

def rush_hour_4x4(initial: str, goals: str, domain_fluents: str) -> PlanningProblem:
    """Return a planning problem representing a simplified 4x4 Rush-Hour 
    domain.

    This domain does not have 'cars' and 'trucks', etc. as in the
    original Rush Hour game. Instead, every vehicle is a 'Car' that
    occupies only one cell and may move one step along its orientation.
    The domain of cell names and adjacency relations is generated 
    programmatically.
        
    Start and goal states are passed in. The car doesn't 
    have an exit lane, rather we define a goal cell and consider
    the problem solved when it reaches it.
    """
    # Action schemas for cars.
    actions = [
        Action(
            'MoveRight(c, frm, to)',
            precond=expr('At(c, frm) & Clear(to) & Horizontal(c) & AdjacentRight(frm, to)'),
            effect=expr('At(c, to) & Clear(frm) & ~At(c, frm) & ~Clear(to)'),
            domain=expr('Car(c) & Cell(frm) & Cell(to) & AdjacentRight(frm, to)'),
        ),
        
        # BEGIN_YOUR_CODE

        Action(
            'MoveLeft(c, frm, to)',
            precond=expr('At(c, frm) & Clear(to) & Horizontal(c) & AdjacentLeft(frm, to)'),
            effect=expr('At(c, to) & Clear(frm) & ~At(c, frm) & ~Clear(to)'),
            domain=expr('Car(c) & Cell(frm) & Cell(to) & AdjacentLeft(frm, to)'),
        ),
        Action(
            'MoveUp(c, frm, to)',
            precond=expr('At(c, frm) & Clear(to) & Vertical(c) & AdjacentUp(frm, to)'),
            effect=expr('At(c, to) & Clear(frm) & ~At(c, frm) & ~Clear(to)'),
            domain=expr('Car(c) & Cell(frm) & Cell(to) & AdjacentUp(frm, to)'),
        ),
        Action(
            'MoveDown(c, frm, to)',
            precond=expr('At(c, frm) & Clear(to) & Vertical(c) & AdjacentDown(frm, to)'),
            effect=expr('At(c, to) & Clear(frm) & ~At(c, frm) & ~Clear(to)'),
            domain=expr('Car(c) & Cell(frm) & Cell(to) & AdjacentDown(frm, to)'),
        ),

        # END_YOUR_CODE
        
    ]

    # Build cell and adjacency facts.
    cell_names = [f'C{row}_{col}' for row in range(1, 5) for col in range(1, 5)]
    cell_defs = ' & '.join(f'Cell({name})' for name in cell_names)

    # Horizontal adjacency.
    adj_right = []
    adj_left = []
    for row in range(1, 5):
        for col in range(1, 4):
            frm = f'C{row}_{col}'
            to = f'C{row}_{col+1}'
            adj_right.append(f'AdjacentRight({frm}, {to})')
        for col in range(2, 5):
            frm = f'C{row}_{col}'
            to = f'C{row}_{col-1}'
            adj_left.append(f'AdjacentLeft({frm}, {to})')
    adj_right_defs = ' & '.join(adj_right)
    adj_left_defs = ' & '.join(adj_left)

    # Vertical adjacency.
    adj_down_defs = ''
    adj_up_defs = ''
    
    # BEGIN_YOUR_CODE

    adj_down = []
    adj_up = []

    for col in range(1, 5):
        for row in range(1, 4):
            frm = f'C{row}_{col}'
            to = f'C{row+1}_{col}'
            adj_down.append(f'AdjacentDown({frm}, {to})')

        for row in range(2, 5):
            frm = f'C{row}_{col}'
            to = f'C{row-1}_{col}'
            adj_up.append(f'AdjacentUp({frm}, {to})')

    adj_down_defs = ' & '.join(adj_down)
    adj_up_defs = ' & '.join(adj_up)

    # END_YOUR_CODE

    domain_expr = expr(
        domain_fluents
        + ' & '
        + cell_defs
        + ' & '
        + adj_right_defs
        + ' & '
        + adj_left_defs
        + ' & '
        + adj_down_defs
        + ' & '
        + adj_up_defs
    )

    return PlanningProblem(
        initial=expr(initial), goals=expr(goals), actions=actions, domain=domain_expr
    )


# ==============================================================
# The following functions initialize a Planning Problem with the
# associated task. No work is necessary here. Familiarize 
# yourself with the tasks specified. Consider using these to test
# Part 1.1.
# ==============================================================
def simple_rush_hour_task():

    initial = ('At(R, C3_2) & At(B1, C1_3) & At(B2, C1_1) & '
            'Clear(C1_2) & Clear(C1_4) & Clear(C2_1) & '
            'Clear(C2_2) & Clear(C2_3) & Clear(C2_4) & '
            'Clear(C3_1) & Clear(C3_3) & Clear(C3_4) & '
            'Clear(C4_1) & Clear(C4_2) & Clear(C4_3) & '
            'Clear(C4_4)'
            )
    goals = 'At(R, C3_4)'
    domain = ('Car(R) & Car(B1) & Car(B2) & '
            'Horizontal(R) & Horizontal(B2) & Vertical(B1)')
    
    return rush_hour_4x4(initial, goals, domain)

def complex_rush_hour_task():
   
    initial = (
        'At(R, C4_1) & At(A, C4_2) & At(B, C4_3) & '
        'At(C, C3_3) & At(D, C3_2) & At(E, C3_4) & '
        'Clear(C1_1) & Clear(C1_2) & Clear(C1_3) & '
        'Clear(C1_4) & Clear(C2_1) & Clear(C2_2) & '
        'Clear(C2_3) & Clear(C2_4) & Clear(C3_1) & '
        'Clear(C4_4)'
    )
    
    goals = "At(R, C4_4)"
    
    domain = (
        'Car(R) & Car(A) & Car(B) & Car(C) & Car(D) & Car(E) & '
        'Horizontal(R) & Vertical(A) & Vertical(B) & Horizontal(C) & Vertical(D) & Vertical(E)'
    )   
    
    return rush_hour_4x4(initial, goals, domain)


"""
The following is provided as a helper function for debugging 
PART 1.1. It prints out information relevant to debugging issues
with problem initialization.

Excessive and abnormal scoping is normal for the domain.
"""
def rush_hour_4x4_helper():
    planning_problem = simple_rush_hour_task()
    print((f"Domain: {planning_problem.domain}\n\n"
            f"Actions: {planning_problem.actions}\n\n"
            f"Initial State: {planning_problem.initial}\n\n"
            f"Goals: {planning_problem.goals}"))

"""
The following functions show how to set up a planning problem,
run GraphPlan on this problem, and linearize the problem from a
partial order plan into a totally ordered plan.
"""
def test_simple_rush_hour_graphplan():
    task = simple_rush_hour_task()
    partial_plan = GraphPlan(task).execute()
    linearized_plan = Linearize(task).execute()
    
    print(f"Partial plan: {partial_plan}")
    print(f"Linearized plan: {linearized_plan}")

    solution_baseline = ["MoveRight(R, C3_2, C3_3)", "MoveRight(R, C3_3, C3_4)"]
    for action in solution_baseline:
        assert expr(action) in linearized_plan
        

def test_complex_rush_hour_graphplan():
    task = complex_rush_hour_task()
    partial_plan = GraphPlan(task).execute()
    linearized_plan = Linearize(task).execute()
    
    solution_baseline = [
        "MoveUp(A, C4_2, C3_2)",
        # "MoveRight(C, C3_3, C3_4)" OR "MoveLeft(C, C3_3, C3_2)"
        "MoveUp(B, C4_3, C3_3)",
        "MoveUp(D, C3_2, C2_2)",
        "MoveRight(R, C4_1, C4_2)",
        "MoveRight(R, C4_2, C4_3)",
        "MoveRight(R, C4_3, C4_4)"
    ]
    print(f"Partial plan: {partial_plan}")
    print(f"Linearized plan: {linearized_plan}")
    
    for action in solution_baseline:
        assert expr(action) in linearized_plan
 
    return partial_plan, linearized_plan

################################################################
# PART 1.2: Planning Graph Statistics
################################################################
def extract_planning_graph_stats(problem: PlanningProblem):
    """Compute planning graph statistics for a given problem.
    These will be plotted by a caller function.

    Record the number of facts (state literals), the number of 
    actions (including persistence/noop actions) and the number 
    of mutex pairs [both state and action] at each level. The 
    exact output format is up to you, but we suggest the 
    statistics are returned as a list of tuples `[(n_facts0,
    n_actions0, n_state_mutex0, n_action_mutex0), (n_facts1, ...),
    ...]`.
    
    Parameters
    ----------
    problem : PlanningProblem
        The planning problem to analyse.
    """
    # Wrap problem in GraphPlan Object
    planner = GraphPlan(problem) 
    # Run GraphPlan, will return a partial order plan (POP). Note:
    # this call is stateful, and will populate the planner object.
    planner.execute()
    # List to be populated with e.g. tuples of information from
    # each level.
    stats = []
    
    # BEGIN_YOUR_CODE

    for level in planner.graph.levels:
        n_facts = len(level.current_state)
        n_actions = len(level.current_action_links)
        n_state_mutex = len(level.state_mutexes)
        n_action_mutex = len(level.action_mutexes)
        stats.append((n_facts, n_actions, n_state_mutex, n_action_mutex))
       
    # END_YOUR_CODE

    return stats

def generate_pg_plot(pp: PlanningProblem):
    """
    Plotter function for extracted graph statistics
    No return object is necessary.
    """
    stats = extract_planning_graph_stats(pp)
    print(stats)
    
    # BEGIN_YOUR_CODE

    levels = list(range(len(stats)))
    facts = [s[0] for s in stats]
    actions = [s[1] for s in stats]
    state_mutex = [s[2] for s in stats]
    action_mutex = [s[3] for s in stats]

    plt.figure()
    plt.plot(levels, facts, label='Facts')
    plt.plot(levels, actions, label='Actions')
    plt.plot(levels, state_mutex, label='State Mutex')
    plt.plot(levels, action_mutex, label='Action Mutex')

    plt.xlabel('Level')
    plt.ylabel('Count')
    plt.title('Planning Graph Statistics')
    plt.legend()
    plt.show()
   
    # END_YOUR_CODE

################################################################
# PART 1.3: Landmarks
################################################################
def list_landmarks() -> list:
    """Return a list of 'At' landmarks for the complex_rush_hour
    problem.

    The return value should be a list of strings, where each string 
    represents one landmark proposition.
    
    Implementing a full landmark detection algorithm is beyond the
    scope of this assignment.  Instead, use your understanding of the
    domain and manual solution to enumerate the required facts.  The
    autograder will verify that your list contains the expected
    landmarks, irrespective of order.
    
    The code can be as simple as: return ["...", ...]
    """
    
    # BEGIN_YOUR_CODE
    
    return ['At(R, C4_2)', 'At(R, C4_3)', 'At(A, C3_2)', 'At(B, C3_3)', 'At(D, C2_2)']
    
    # END_YOUR_CODE


################################################################
# PART 1.4: [EXTRA CREDIT] Generalized Rush Hour
################################################################
"""
PART 1.4 — EXTRA CREDIT
Generalized Rush Hour (4x4, with Trucks)
-------------------------------------------------

In this final part, you will implement a *generalized* Rush Hour domain 
for a 4x4 grid with trucks, which occupy two consecutive cells instead 
of just one.

The trucks may be:
 - Horizontal trucks: length 2 along a row
 - Vertical trucks:   length 2 along a column

You must define:
 -  A new function `rush_hour_with_trucks(config)` that creates 
    a `PlanningProblem` for this domain.

Once defined, the function should:
 - Create a `PlanningProblem` instance from a provided configuration.

Executing `GraphPlan` to compute a partial-order plan and
linearizing that plan on this planning problem should solve
in a reasonable amount of time on the autograder. 

Note:
    This part is HARD (a valid solution may not exist within compute 
    constraints imposed by the autograder). The GraphPlan algorithm (and its 
    linearization step) suffers greatly from the curse of dimensionality
    — as the grid and # vehicles/sizes grow, the search space 
    expands combinatorially. Even small configurations may 
    require nontrivial computation to solve. It will be graded as
    full points or none.

### Configuration Format

You will receive a configuration dictionary such as:

```
config = {
    'cars': {
        'R': {'pos': (3, 1), 'dir': 'Horizontal'},   # Red car, horizontal
        'A': {'pos': (1, 1), 'dir': 'Horizontal'},   # Car A, horizontal
        'B': {'pos': (4, 3), 'dir': 'Vertical'}      # Car B, vertical
    },
    'trucks': {
        'T1': {'pos': ((3, 2), (4, 2)), 'dir': 'Vertical'},  # Horizontal truck
        'T2': {'pos': ((2, 3), (3, 3)), 'dir': 'Vertical'}     # Vertical truck
    },
    'goal': {
        'R': (3, 4),                                 # Red car must reach exit
        'T1': ((1, 2), (2, 2))                       # Truck T2 must move up two cells
    }
}
```

Which represents an inital state of:

      C1  C2  C3  C4
   +------------------+
R1 |  A   *   .   .  |
R2 |  .   *   T2  .  |
R3 |  R   T1  T2  *  |      * represents goals for R, T1
R4 |  .   T1  B   .  |
   +------------------+


Note - positions are 1-indexed in the configuration
Note - the direction of trucks is included, but they will always
  only move in the direction of their longest axis.
Note - You can assume there will always be 3 cars - A,B,R, and two
  trucks - T1, T2.

Feel free to add, modify, or remove your own predicates, actions, etc.
"""

def rush_hour_with_trucks(config):
    """
    Returns a PlanningProblem object, emulating rush_hour_4x4()
    """
    
    # BEGIN_WORK_HERE
    
    raise NotImplementedError()

    # #END_WORK_HERE


"""
Feel free to modify or remove. This will not impact grading.
"""
if __name__ == "__main__":
    """
    TESTING 1.1
    """
    test_simple_rush_hour_graphplan()
    test_complex_rush_hour_graphplan()

    """
    TESTING 1.2
    """
    pp = simple_rush_hour_task()
    generate_pg_plot(pp)
    pp = complex_rush_hour_task()
    generate_pg_plot(pp)
    
    """
    TESTING 1.3
    """
    print(list_landmarks())

