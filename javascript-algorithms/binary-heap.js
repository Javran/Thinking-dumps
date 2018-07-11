/*
   since JS does not come with priority queue,
   implementing a heap to serve this purpose seems reasonable.

   core operations are `siftUp` and `siftDown` for maintaining
   data structure invariants.

   for practical concerns `elemToKey` is implemented
   to allow every element to project a key for sorting.

   note about absence of decreaseKey: it's not necessary to implement it
   for applications like Dijkstra's algorithm: we can insert the same
   node as many times as we want and ignore the old value (as the newest
   value will float to the front faster than the old one)
 */

function BinHeap(elemToKey) {
  this.size = 0
  this.container = []
  this.elemToKey = elemToKey
}

const siftUp = pq => ind => {
  const {container, elemToKey} = pq
  while (ind !== 0) {
    const parentInd = (ind - 1) >> 1
    if (elemToKey(container[parentInd]) > elemToKey(container[ind])) {
      const tmp = container[parentInd]
      container[parentInd] = container[ind]
      container[ind] = tmp
      ind = parentInd
    } else {
      break
    }
  }
}

const siftDown = pq => ind => {
  const {size, container, elemToKey} = pq
  while (true) {
    const lcInd = ind*2+1
    const rcInd = ind*2+2
    if (lcInd >= size)
      break
    let preferInd = ind
    if (
      elemToKey(container[lcInd]) <= elemToKey(container[preferInd])
    )
      preferInd = lcInd
    if (
      rcInd < size &&
      elemToKey(container[rcInd]) <= elemToKey(container[preferInd])
    )
      preferInd = rcInd

    if (preferInd !== ind) {
      const tmp = container[preferInd]
      container[preferInd] = container[ind]
      container[ind] = tmp
      ind = preferInd
    } else {
      break
    }
  }
}

BinHeap.prototype.insert = function(e) {
  const eInd = this.size
  this.container[this.size] = e
  ++this.size
  siftUp(this)(eInd)
}

BinHeap.prototype.extractMin = function() {
  if (this.size === 0)
    return null
  const ret = this.container[0]
  this.container[0] = this.container[this.size-1]
  --this.size
  siftDown(this)(0)
  return ret
}

/*
   run a test by using it to sort a random generated array
   and compare the result with Array.prototype.sort.

   indeed we can do heapify much better than inserting elements one by one,
   but here we just view it as a priority queue and sees only `size`, `extractMin`, `insert`.
 */
const test = () => {
  const xs = []
  const hp = new BinHeap(x => x)
  const l = (1 << 16) + Math.floor(Math.random() * (1 << 16))
  for (let i = 0; i < l; ++i) {
    const val = Math.floor(Math.random() * (1 << 30))
    xs.push(val)
    hp.insert(val)
  }

  const ys = []
  console.time('min heap')
  while (hp.size > 0)
    ys.push(hp.extractMin())
  console.timeEnd('min heap')

  console.time('stock')
  xs.sort((x, y) => x - y)
  console.timeEnd('stock')

  for (let i = 0; i < xs.length; ++i)
    console.assert(xs[i] === ys[i])
  console.log(`${xs.length} elements sorted, test ok`)
}

// test()
exports.BinHeap = BinHeap
