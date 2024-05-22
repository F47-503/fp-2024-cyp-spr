# Graph algorithms

This is supposed to be a library making it easy to use graphs in Haskell. 

* (10 points) Graph representation
* (15 points) BFS
* (15 points) DFS
* (10 points) Examples of library use
* (5 points) Error reporting
* (5 points) Unit test

Additional tasks: 

* (10 points) Make it possible to specify the type of vertices and edges
* (10 points) Dijkstra's algorithm
* (10 points) Render graphs nicely (you can use DOT)
* (10 points) Property-based testing

## Report
Please, see [project page](https://github.com/F47-503/Hagraphs) for source code.
1) I implemented graph as a type `Data.Map vertex (Data.Map vertex edge))`,
  
   as it should be possible to get all adjacent vertices knowing only one vertex, and it should be possible to get an complex template edge knowing 2 vertices.

   Also, this would allow us to create directed and undirected graphs with edge and vertex types as arguments.

   Then I created simple functions for graph creation, usually a graph is a list of edges, and therefore I created different functions to create Graphs from Lists.
3) On such graphs, I implemented DFS: from my point of view, DFS can implement quite complex logic, but it almost always can be described as 5 entities:
   * What we do when we first time visit the vertex.
   * What we do when we go to another vertex in recursion.
   * What we do when we return from another vertex in recursion.
   * What we do when we leave the vertex.
   * Accumulator which is actual result of DFS algorithm.
   The idea of this project appeared because of the logic above, as it is sometimes hard to formalize DFS and BFS logic strictly, but functional view

often gives good explanation here. Also it is perfectly aligned with using Monad State which will calculate accumulator while keeping set of visited vertices in Haskell, 

and it is the way how I implemented it. One of the most common examples is just simple list of vertices in connectivity component.

 4) On such graphs I also implemented BFS, but as it is harder to formalize it in terms of recursion / functions, I needed several "ugly components" here.

    But the key idea remains - we are keeping Monad State which tracks visited vertices and in case of BFS also tracks the first vertex to visit.
    
    The good thing about this implementation is that it can be easily upgraded to Dijkstra algorithm (as we are keeping some structure with minimum pop anyway).
 5) I provided some unit tests for this implementations which can be run using  `stack test`.

    Check (this)[https://codeforces.com/contest/1057/submission/262142770] for example of library usage. This shows that this library can even be used for CP programming, and optimizations (such as using Data.HashMap instead of Data.Map) are posiible.
    I don't see where the error can appear here if the types are not colliding, therefore I would like to receive at most 10 + 15 + 15 + 5 + 10 = 55 points.
