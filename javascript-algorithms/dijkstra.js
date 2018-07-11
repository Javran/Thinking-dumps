const {BinHeap} = require('./binary-heap.js')

const shortestPath = (graph, sInd) => {
  /*
     graph[i]: Array of [<the other node>, <distance>].
     undirected graph so forall u-v with distance d, v-u exists with same distance.
   */
  const V = graph.length
  const dists = new Array(V).fill(+Infinity)
  const visited = new Int8Array(V)
  /*
     store nodeInd and dist *of current time*
     as dist gets updated during the process which
     would potentially violate heap invariant.
   */
  const pq = new BinHeap(elem => elem.dist)
  dists[sInd] = 0
  pq.insert({nodeInd: sInd, dist: 0})
  while (pq.size > 0) {
    const {nodeInd, dist} = pq.extractMin()
    if (visited[nodeInd])
      continue
    visited[nodeInd] = 1
    dists[nodeInd] = dist
    graph[nodeInd].forEach(([thatInd, edgeDist]) => {
      const newDist = dist + edgeDist
      if (newDist < dists[thatInd]) {
        dists[thatInd] = newDist
        // no decrease key, but we insert new element with smaller dist
        pq.insert({nodeInd: thatInd, dist: newDist})
      }
    })
  }
  return dists
}

console.log(shortestPath(
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
  0
))
