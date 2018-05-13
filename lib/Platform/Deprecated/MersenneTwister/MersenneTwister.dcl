// ************************************************************
//  MersenneTwister - A pseudo-random generator
//  Version 1.0.1 - July 30, 1999 - Thorsten Zoerner
//  Catholic University of Nijmegen - zoerner@cs.kun.nl
// ************************************************************
//  This implementation is a rewrite of a C program by T. Nishimura
//  which can be found in the paper "Mersenne Twister: A 
//  623-dimensionally equidistributed uniform pseudorandom number 
//  generator" by M. Matsumoto and T. Nishimura in ACM Transactions 
//  on Modeling and Computer Simulation, vol. 8, no. 1, 
//  January 1998, pp. 3-30.
// ************************************************************
//  The original C code contained the following notice:
//      Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
//      When you use this, send an email to: matumoto@math.keio.ac.jp
//      with an appropriate reference to your work.
// ************************************************************

definition module MersenneTwister

genRandReal :: Int -> [Real]
// Generates an infinite list of in [0, 1] uniformly distributed 
// real pseudorandom numbers. There period is (2^19937)-1.
// Input any nonzero integer as seed value.

genRandInt :: Int -> [Int]		
// Generates an infinite list of in [-(2^31), (2^31)-1] uniformly distributed 
// signed integer pseudorandom numbers. There period is (2^19937)-1.
// Input any nonzero integer as seed value.

