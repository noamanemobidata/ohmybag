from ortools.algorithms.python import knapsack_solver

solver = knapsack_solver.KnapsackSolver(
    knapsack_solver.SolverType.KNAPSACK_MULTIDIMENSION_BRANCH_AND_BOUND_SOLVER,
    "KnapsackExample",
)


def solve(values,weights, capacities):
  weights= [weights]
  capacities=[capacities]
  solver.init(values, weights, capacities)
  computed_value = solver.solve()
  
  packed_items = []
  packed_weights = []
  total_weight = 0
  is_packed= []
  for i in range(len(values)):
      check= solver.best_solution_contains(i)
      is_packed.append(check) 
      if check:
          packed_items.append(i)
          packed_weights.append(weights[0][i])
          total_weight += weights[0][i]

  
  return {"computed_value":computed_value, "packed_items":packed_items,"packed_weights":packed_weights , "is_packed":is_packed}
