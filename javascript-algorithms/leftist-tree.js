function LNode(val) {
  this.val = val
  this.left = this.right = null
  this.s = 1
}

const meld = (l, r) => {
  if (l === null)
    return r
  if (r === null)
    return l
  if (l.val >= r.val) {
    const tmp = l
    l = r
    r = tmp
  }
  // INVARIANT: l.val < r.val i.e. l's root is smaller
  l.right = meld(l.right, r)
  // INVARIANT: l.right !== null
  // this is because at least "r" cannot be empty
  // and `meld` cannot remove nodes in the process

  if (l.left === null) {
    l.left = l.right
    l.right = null
  } else {
    if (l.left.s < l.right.s) {
      const tmp = l.left
      l.left = l.right
      l.right = tmp
    }
    l.s = 1 + l.right.s
  }
  return l
}

function LeftistHeap() {
  this.root = null
}

LeftistHeap.prototype.insert = function(v) {
  const newNode = new LNode(v)
  this.root = meld(this.root, newNode)
}

LeftistHeap.prototype.extractMin = function() {
  if (this.root === null)
    return null
  const ret = this.root.val
  this.root = meld(this.root.left, this.root.right)
  return ret
}

const test = () => {
  const hp = new LeftistHeap()
  const size = 1 << 20
  const xs = []
  for (let i = 0; i < size; ++i)
    xs.push(Math.floor(Math.random() * 100))
  console.time('leftist')
  xs.forEach(x => hp.insert(x))
  const actual = []
  while (hp.root !== null) {
    actual.push(hp.extractMin())
  }
  console.timeEnd('leftist')
  console.time('stock')
  xs.sort((x,y) => x - y)
  console.timeEnd('stock')
  console.assert(actual.every((x, i) => x === xs[i]))
}

test()
