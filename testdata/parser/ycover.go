// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cover

// Cover states not visited by yacc parser when parsing GOROOT/src.

import ()
import . ""
const()
type ()
type _ (_)
type _ [(<-chan (_))]_
type _ [(<-chan *_)]_
type _ [(<-chan <-chan _)]_
type _ [(<-chan _)]_
type _ [(<-chan chan _)]_
type _ [(<-chan func())]_
type _ [chan _('a',)]_
type _ chan(_)
var ()

//TODO- when 1.8 is out

const _ => _._
func _ => _._
type _ => _._
var _ => _._
