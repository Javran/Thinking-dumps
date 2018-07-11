const shortestPaths = graph => {
  const V = graph.length
  const dist = new Array(V)
  for (let i = 0; i < V; ++i) {
    dist[i] = new Array(V).fill(+Infinity)
    dist[i][i] = 0
  }
  graph.forEach((edges, nodeInd) =>
    edges.forEach(([thatInd, edgeDist]) => {
      dist[nodeInd][thatInd] = edgeDist
    })
  )
  for (let k = 0; k < V; ++k) {
    for (let i = 0; i < V; ++i) {
      for (let j = 0; j < V; ++j) {
        const newDist = dist[i][k] + dist[k][j]
        if (newDist < dist[i][j]) {
          dist[i][j] = newDist
        }
      }
    }
  }
  return dist
}

console.log(shortestPaths(
  // same graph as used in my Dijkstra's algorithm impl
  [
    // 0
    [[1, 4], [2, 6], [4, 9], [5, 10]],
    // 1
    [[0, 4], [3, 3], [4, 1]],
    // 2
    [[0, 6], [5, 3]],
    // 3
    [[1, 3], [6, 6], [7, 7]],
    // 4
    [[0, 9], [1, 1], [5, 6], [6, 3], [7, 20]],
    // 5
    [[2, 3], [4, 5], [6, 7]],
    // 6
    [[4, 3], [5, 7], [7, 7]],
    // 7
    [[3, 7], [4, 20], [6, 7]]
  ],
))
