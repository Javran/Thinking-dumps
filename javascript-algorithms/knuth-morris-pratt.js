/*
   more famously known by it's abbr: KMP algorithm
 */

// as indicated in wikipedia
const KMP1 = (str, pat) => {
  /*
     pi[i] is the length of the longest possible proper prefix of pat
     and also a suffix ending at i-1.

     well actually the algorithm no longer matches this definition
     because it takes the librety of jumping backward more aggresively
     to avoid unnecessary checks.
   */
  const pi = new Int32Array(pat.length+1)
  let pos = 1
  let cnd = 0
  pi[0] = -1
  for (
    /* NOOP */;
    pos < pat.length;
    ++pos, ++cnd
  ) {
    // determine pi[pos]
    if (pat.codePointAt(pos) === pat.codePointAt(cnd)) {
      /*
         the different bit.
         we could have done:
         `pi[pos] = cnd`

         but note that since pat[pos] === pat[cnd],
         we can take a step further since
         when matching against pat[pos] fails, pat[cnd] will also fail anyway.

       */
      pi[pos] = pi[cnd]
    } else {
      pi[pos] = cnd
      /*
         note that since in this branch we have established that:
         `pat.codePointAt(pos) === pat.codePointAt(cnd)` is false,
         there is no need of verifying this fact again,
         therefore we skip a bit.
       */
      cnd = pi[cnd]
      while (cnd >= 0 && pat.codePointAt(pos) !== pat.codePointAt(cnd)) {
        cnd = pi[cnd]
      }
    }
  }
  pi[pos] = cnd
  // j scans through the string and k the pattern
  let j = 0
  let k = 0
  const ans = []
  while (j < str.length) {
    if (pat.codePointAt(k) === str.codePointAt(j)) {
      // upon successful match
      ++k, ++j
      if (k === pat.length) {
        // found
        ans.push(j - k)
        /*
           pretend it's mismatch, jump back.
           note that since k = pat.length, pi[k] cannot be -1
         */
        k = pi[k]
      }
    } else {
      k = pi[k]
      if (k < 0) {
        // cannot match, jumping ahead on str
        ++k, ++j
      }
    }
  }
  return ans
}

// from geeksforgeeks
const KMP2 = (str, pat) => {
  const T = new Int32Array(pat.length + 1)
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
    if (
      pat.codePointAt(j) === str.codePointAt(i)
    ) {
      ++j, ++i
    }
    if (j === pat.length) {
      ans.push(i-j)
      j = T[j-1]
    } else if (
      i < str.length && pat.codePointAt(j) !== str.codePointAt(i)
    ) {
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
  const pi = new Int32Array(pat.length + 1)
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

// test()

// KMP1('', 'ABCDABD')
KMP1('', 'PARTICIPATE IN PARACHUTE')
