// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

// DeclarationKind describes a Declaration's kind.
type DeclarationKind int

// Values of DeclarationKind.
const (
	ConstDeclaration DeclarationKind = iota
	FuncDeclaration
	ImportDeclaration
	MethodDeclaration
	TypeDeclaration
	VarDeclaration
)
