/*
   more famously known by it's abbr: KMP algorithm
 */

// as indicated in wikipedia
const KMP = (str, pat) => {
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
    if (pat.codePointAt(pos) === pat.codePointAt(cnd)) {
      T[pos] = T[cnd]
    } else {
      T[pos] = cnd
      cnd = T[cnd]
      while (cnd >= 0 && pat.codePointAt(pos) !== pat.codePointAt(cnd)) {
        cnd = T[cnd]
      }
    }
  }
  T[pos] = cnd
  console.log(T)
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
  console.log(T)
}

[KMP, KMP2].forEach(KMPImpl => {
  console.log(KMPImpl('', 'ABCDABD'))
  console.log(KMPImpl('', 'PARTICIPATE IN PARACHUTE'))
  console.log(KMPImpl('', "AAACAAAAAC"))
  console.log(KMPImpl('', 'AABAACAABAA'))
})
