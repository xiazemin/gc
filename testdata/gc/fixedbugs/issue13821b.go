// errorcheck

// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Issue 13821.  Additional regress tests.

package p

type B bool
type B2 bool

var b B
var b2 B2
var x1 = b && 1 < 2 // x1 has type B, not ideal bool
var x2 = 1 < 2 && b // x2 has type B, not ideal bool
var x3 = b && b2    // ERROR "mismatched types (p\.)?B and (p\.)?B2"
var x4 = x1 && b2   // ERROR "mismatched types (p\.)?B and (p\.)?B2"
var x5 = x2 && b2   // ERROR "mismatched types (p\.)B and (p\.)B2"
var x6 = b2 && x1   // ERROR "mismatched types (p\.)?B2 and (p\.)?B"
var x7 = b2 && x2   // ERROR "mismatched types (p\.)B2 and (p\.)B"

var x8 = b && !B2(true) // ERROR "mismatched types (p\.)B and (p\.)B2"
