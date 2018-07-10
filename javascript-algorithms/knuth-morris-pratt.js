/*
   more famously known by it's abbr: KMP algorithm
 */

// as indicated in wikipedia
const KMP1 = (str, pat) => {
  const T = new Array(pat.length)
  let pos = 1
  let cnd = 0
  T[0] = -1
  for (
    /* NOOP */;
    pos < pat.length;
    ++pos, ++cnd
  ) {
    // determine T[pos]
    const isMatch = pat.codePointAt(pos) === pat.codePointAt(cnd)
    if (isMatch) {
      T[pos] = T[cnd]
    } else {
      T[pos] = cnd
      cnd = T[cnd]
      while (cnd >= 0 && !isMatch) {
        cnd = T[cnd]
      }
    }
  }
  T[pos] = cnd
  let j = 0
  let k = 0
  const ans = []
  while (j < str.length) {
    if (pat.codePointAt(k) === str.codePointAt(j)) {
      ++k, ++j
      if (k === pat.length) {
        // found
        ans.push(j - k)
        k = T[k]
      }
    } else {
      k = T[k]
      if (k < 0) {
        ++k, ++j
      }
    }
  }
  return ans
}

// from geeksforgeeks
const KMP2 = (str, pat) => {
  const T = new Array(pat.length)
  let len = 0
  T[0] = 0
  for (
    let i = 1;
    i < pat.length;
    /* NOOP */
  ) {
    if (pat.codePointAt(i) === pat.codePointAt(len)) {
      ++len
      T[i] = len
      ++i
    } else {
      if (len !== 0) {
        len = T[len-1]
      } else {
        T[i] = 0
        ++i
      }
    }
  }

  let i = 0, j = 0
  const ans = []
  while (i < str.length) {
    const isMatch = pat.codePointAt(j) === str.codePointAt(i)
    if (isMatch) {
      ++j, ++i
    }
    if (j === pat.length) {
      ans.push(i-j)
      j = T[j-1]
    } else if (i < str.length && !isMatch) {
      if (j != 0) {
        j = T[j-1]
      } else {
        ++i
      }
    }
  }
  return ans
}

// modified from CS 97SI Stanford University By Jaehyun Park
const KMP3 = (str, pat) => {
  const pi = new Array(pat.length + 1)
  pi[0] = -1
  let k = -1
  for (let i = 1; i <= pat.length; ++i) {
    while (k >= 0 && pat.codePointAt(k) !== pat.codePointAt(i-1))
      k = pi[k]
    pi[i] = ++k
  }
  k = 0
  const ans = []
  for (let i = 0; i < str.length; ++i) {
    while (k >= 0 && pat.codePointAt(k) !== str.codePointAt(i))
      k = pi[k]
    ++k
    if (k === pat.length) {
      ans.push(i-pat.length+1)
      k = pi[k]
    }
  }
  return ans
}

const mkStr = (() => {
  // construct a str..
  const min = 0, max = 3
  const range = max - min + 1
  const rndCodePoint = () => {
    const ind = Math.floor(Math.random() * range) + min
    return 65+ind
  }
  const mkStr = size => {
    const preStr = []
    for (let i = 0; i < size; ++i)
      preStr.push(rndCodePoint())
    return preStr.map(x => String.fromCodePoint(x)).join('')
  }
  return mkStr
})()

const test = () => {
  const str = mkStr(1 << 20), pat = mkStr(1 << 3)
  const ans = []
  let i = 0
  do {
    i = str.indexOf(pat, i)
    if (i !== -1) {
      ans.push(i)
      ++i
    }
  } while (i !== -1)

  ;[KMP1, KMP2, KMP3].forEach(KMPImpl => {
    const caseName = String(KMPImpl.name)
    console.time(caseName)
    let actual
    for (let i = 0; i < 10; ++i)
      actual = KMPImpl(str, pat)
    console.timeEnd(caseName)
    console.assert(actual.length === ans.length, 'len')
    console.assert(actual.every((x,i) => x === ans[i]))
  })
}

test()
