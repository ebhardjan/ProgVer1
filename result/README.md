# Result

We evaluated the three SAT solving algorithms using our own formula generator. 
We generated formulas in 3sat and 5sat, meaning each clause has at most 3 or 5 
literals respectively. In each of those categories, we used formulas with 
increasing numbers of variables and clauses. For each of these tuples 
(#variables, #clauses) we generated 10 formulas which are satisfiable and 10 
which are unsat. We will now look at the results in more detail.

## 3sat

For 3sat the set of the number of variables we used was
[3,5,8,10,15,20,25,30,40,50,60,70,80,90,100,120]. The number of clauses increase 
accordingly. So for each of those values we generated 10 satisfiable and 10 
unsatisfiable formulas, giving a total number of 320 formulas. We let each 
algorithm run twice on each formula, to then average the runtime. We set a 
timeout of 20 seconds, so if an algorithm took longer than that, we cancelled 
it and would record it as failed (those will just be missing entries in the 
graph). The following graph shows the result of the experiments:

![](3sat_allresults_2runs_i20s.png)

The x axis shows the number of variables of the formula, the y axis the runtime
in milliseconds on a logarithmic scale.

The first thing we notice, is that all algorithms show an exponential increase
of the runtime with increasing number of variables. Surprisingly DPLL performs
significantly better than CDCL. We assume this is because we are still dealing
with rather small numbers of variables, so the overhead of maintaining the graph
is quite high. The algorithmic advantage would probably show up on larger
formulas, but the general performance of our solver can't handle those in
reasonable timeframes.

DP can keep up to formulas with around 20 variables and 90 clauses before
going over the 20 second limit. CDCL goes up to about 50, DPLL to 80.

For the next two graphs, we seperated the satisfiable from the unsatisfiable
formulas, and only look at the average runtime over the ten formulas of same
size. We required all ten formulas to have completed in under 20 seconds to
appear on the graph.

![](3sat_aggregated_10_tests_sat.png) | ![](3sat_aggregated_10_tests_unsat.png)
---|---
only sat formulas | only unsat formulas



## 5sat