// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build ignore

package cover

//state 14 // PACKAGE IDENT ';' IMPORT '.'
import . ""
//state 18 // PACKAGE IDENT ';' IMPORT '(' ')'
import ()
//state 41 // PACKAGE IDENT ';' TYPE IDENT '('
type _ (_)
//state 135 // PACKAGE IDENT ';' TYPE IDENT CHAN '('
type _ chan(_)
//state 261 // PACKAGE IDENT ';' TYPE IDENT '[' '(' ARROW CHAN IDENT [')']
type _ [(<-chan _)]_
//state 283 // PACKAGE IDENT ';' TYPE IDENT '[' '(' ARROW CHAN FUNC '(' ')' [')']
type _ [(<-chan func())]_
//state 284 // PACKAGE IDENT ';' TYPE IDENT '[' '(' ARROW CHAN CHAN IDENT ['(']
type _ [(<-chan chan _)]_
//state 285 // PACKAGE IDENT ';' TYPE IDENT '[' '(' ARROW CHAN '*' IDENT ['(']
type _ [(<-chan *_)]_
//state 287 // PACKAGE IDENT ';' TYPE IDENT '[' '(' ARROW CHAN '('
type _ [(<-chan (_))]_
//state 288 // PACKAGE IDENT ';' TYPE IDENT '[' '(' ARROW CHAN ARROW
type _ [(<-chan <-chan _)]_
//state 301 // PACKAGE IDENT ';' TYPE IDENT '[' CHAN IDENT '(' CHAR ','
type _ [chan _('a',)]_
//state 407 // PACKAGE IDENT ';' TYPE '(' ')'
type ()
//state 423 // PACKAGE IDENT ';' CONST '(' ')'
const()
//state 445 // PACKAGE IDENT ';' VAR '(' ')'
var ()
