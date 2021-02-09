// Package diffiehellman implements steps in Diffie-Hellman-Merkle key exchange.
package diffiehellman

import (
	"crypto/rand"
	"math/big"
)

// big.Int representation for number two.
// should never mutate after initialization.
var two = new(big.Int).SetInt64(2) 

// PrivateKey generates a private key in range (1, p)
func PrivateKey(p *big.Int) *big.Int {
	pMinus2 := new(big.Int).Sub(p, two)
	k, err := rand.Int(rand.Reader, pMinus2)
	if err != nil {
		panic(err)
	}
	return k.Add(k, two)
}

// PublicKey computes the public key given given private key and p,g involved in this process.
func PublicKey(private, p *big.Int, g int64) *big.Int {
	tmp := new(big.Int).SetInt64(g)
	return tmp.Exp(tmp, private, p)
}

// NewPair generates private and public key pair given p,g involved in this process.
func NewPair(p *big.Int, g int64) (private, public *big.Int) {
	private = PrivateKey(p)
	return private, PublicKey(private, p, g)
}

// SecretKey computes the exchanged secret
// given one's private key and the other's public key.
func SecretKey(private1, public2, p *big.Int) *big.Int {
	return new(big.Int).Exp(public2, private1, p)
}
