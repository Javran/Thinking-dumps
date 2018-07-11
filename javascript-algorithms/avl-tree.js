function Node(key) {
  this.key = key
  this.left = null
  this.right = null
  this.height = 1
}

const getHeight = node =>
  node === null ? 0 : node.height

const getBalanceFactor = node =>
  node === null ? 0 : getHeight(node.left) - getHeight(node.right)

const updateHeight = node => {
  if (node === null)
    return
  node.height = 1 + Math.max(getHeight(node.left), getHeight(node.right))
}

const rotateL = node => {
  let x = node.right
  node.right = x.left
  x.left = node
  x.height = node.height
  --node.height
  return x
}

const rotateR = node => {
  let x = node.left
  node.left = x.right
  x.right = node
  x.height = node.height
  --node.height
  return x
}

const insert = (node, key) => {
  if (node === null)
    return new Node(key)
  if (key < node.key) {
    node.left = insert(node.left, key)
    if (getBalanceFactor(node) === 2) {
      if (key < node.left.key)
        node = rotateR(node)
      else {
        node.left = rotateL(node.left)
        node = rotateR(node)
      }
    }
  } else if (key > node.key) {
    node.right = insert(node.right, key)
    if (getBalanceFactor(node) === -2) {
      if (key > node.right.key)
        node = rotateL(node)
      else {
        node.right = rotateR(node.right)
        node = rotateL(node)
      }
    }
  } else {
    return node
  }
  updateHeight(node)
  return node
}

const getInordSucc = node => {
  let tmp = node.right
  while (tmp.left !== null)
    tmp = tmp.left
  return tmp
}

const remove = (node, key) => {
  if (node === null)
    return null
  let cmpResult = key - node.key
  if (cmpResult === 0) {
    if (node.left === null && node.right === null) {
      return null
    } else if (node.left === null) {
      return node.right
    } else if (node.right === null) {
      return node.left
    }
    let inSucc = getInordSucc(node)
    node.right = remove(node.right, inSucc.key)
    inSucc.left = node.left
    inSucc.right = node.right
    inSucc.height = node.height
    return inSucc
  } else if (cmpResult < 0) {
    node.left = remove(node.left, key)
    if (getBalanceFactor(node) === 2) {
      if (getBalanceFactor(node.right) === -1) {
        node.right = rotateR(node.right)
        node = rotateL(node)
      } else {
        node = rotateL(node)
      }
    }
  } else {
    node.right = remove(node.right, key)
    if (getBalanceFactor(node) === -2) {
      if (getBalanceFactor(node.left) === -1) {
        node = rotateR(node)
      } else {
        node.left = rotateL(node.left)
        node = rotateR(node)
      }
    }
  }
  updateHeight(node)
  return node
}

let avl = null
for (let i = 0; i < 100; ++i)
  avl = insert(avl, i)

const inOrd = tree => {
  const ret = []
  const go = root => {
    if (root) {
      go(root.left)
      ret.push(root.key)
      go(root.right)
    }
  }
  go(tree)
  return ret
}

console.log(avl)

for (let i = 99; i >= 4; --i)
  avl = remove(avl, i)

console.log(avl)
