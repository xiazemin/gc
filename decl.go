// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

var (
	_ Declaration = (*ConstDecl)(nil)
	_ Declaration = (*FuncDecl)(nil)
	_ Declaration = (*ImportSpec)(nil)
	_ Declaration = (*MethodDecl)(nil)
	_ Declaration = (*TypeDecl)(nil)
	_ Declaration = (*VarDecl)(nil)
)

// ------------------------------------------------------------------- Bindings

// Bindings map names to Declarations.
type Bindings map[string]Declaration

func (b *Bindings) declare(p *parser, d Declaration) {
	if *b == nil {
		*b = Bindings{}
	}
	m := *b
	nm := d.Name()
	if nm == "" {
		panic("internal error")
	}

	ex := m[nm]
	if ex == nil {
		m[nm] = d
		return
	}

	panic("TODO")
}

// ---------------------------------------------------------------- declaration

type declaration struct {
	nm  string
	off int
	src *SourceFile
}

func newDeclaration(src *SourceFile, off int, nm string) declaration {
	return declaration{
		nm,
		off,
		src,
	}
}

// Const implements Declaration.
func (d *declaration) Const() *ConstDecl { panic("Const of inappropriate Declaration") }

// Func implements Declaration.
func (d *declaration) Func() *FuncDecl { panic("Func of inappropriate Declaration") }

// ImportSpec implements Declaration.
func (d *declaration) ImportSpec() *ImportSpec { panic("ImportSpec of inappropriate Declaration") }

// Method implements Declaration.
func (d *declaration) Method() *MethodDecl { panic("Method of inappropriate Declaration") }

// Name implements Declaration.
func (d *declaration) Name() string { return d.nm }

// Offset implements Declaration.
func (d *declaration) Offset() int { return d.off }

// SourceFile implements Declaration.
func (d *declaration) SourceFile() *SourceFile { return d.src }

// Type implements Declaration.
func (d *declaration) Type() *TypeDecl { panic("Type of inappropriate Declaration") }

// Var implements Declaration.
func (d *declaration) Var() *VarDecl { panic("Var of inappropriate Declaration") }

// ---------------------------------------------------------------------- Scope

type Scope struct {
	Bindings Bindings
	Kind     ScopeKind
	Labels   Bindings
	Parent   *Scope
	Unbound  []Declaration // Declarations named _.
}

func newScope(kind ScopeKind, parent *Scope) *Scope {
	return &Scope{
		Kind:   kind,
		Parent: parent,
	}
}

func (s *Scope) declare(p *parser, d Declaration) {
	nm := d.Name()
	if nm == "_" {
		s.Unbound = append(s.Unbound, d)
		return
	}

	switch d.(type) {
	default:
		panic("internal error")
	}
}

// ConstDecl describes a constant declaration.
type ConstDecl struct {
	declaration
}

func newConstDecl(src *SourceFile, off int, nm string) *ConstDecl {
	return &ConstDecl{
		declaration: newDeclaration(src, off, nm),
	}
}

// Const implements Declaration.
func (d *ConstDecl) Const() *ConstDecl { return d }

// Kind implements Declaration.
func (d *ConstDecl) Kind() DeclarationKind { return ConstDeclaration }

// FuncDecl describes a function declaration.
type FuncDecl struct {
	declaration
}

func newFuncDecl(src *SourceFile, off int, nm string) *FuncDecl {
	return &FuncDecl{
		declaration: newDeclaration(src, off, nm),
	}
}

// Func implements Declaration.
func (d *FuncDecl) Func() *FuncDecl { return d }

// Kind implements Declaration.
func (d *FuncDecl) Kind() DeclarationKind { return FuncDeclaration }

// MethodDecl describes a method declaration.
type MethodDecl struct {
	declaration
}

func newMethodDecl(src *SourceFile, off int, nm string) *MethodDecl {
	return &MethodDecl{
		declaration: newDeclaration(src, off, nm),
	}
}

// Kind implements Declaration.
func (d *MethodDecl) Kind() DeclarationKind { return MethodDeclaration }

// Method implements Declaration.
func (d *MethodDecl) Method() *MethodDecl { return d }

// TypeDecl describes a type declaration.
type TypeDecl struct {
	declaration
}

func newTypeDecl(src *SourceFile, off int, nm string) *TypeDecl {
	return &TypeDecl{
		declaration: newDeclaration(src, off, nm),
	}
}

// Kind implements Declaration.
func (d *TypeDecl) Kind() DeclarationKind { return TypeDeclaration }

// Type implements Declaration.
func (d *TypeDecl) Type() *TypeDecl { return d }

// VarDecl describes a variable declaration.
type VarDecl struct {
	declaration
}

func newVarDecl(src *SourceFile, off int, nm string) *VarDecl {
	return &VarDecl{
		declaration: newDeclaration(src, off, nm),
	}
}

// Kind implements Declaration.
func (d *VarDecl) Kind() DeclarationKind { return VarDeclaration }

// Var implements Declaration.
func (d *VarDecl) Var() *VarDecl { return d }

// Declaration describes a constant, function, method, type or variable
// declaration.
type Declaration interface {
	// Const returns the Declaration's *ConstDecl. It panics if Kind is not
	// ConstDeclaration.
	Const() *ConstDecl

	// Func returns the Declaration's  *FuncDecl. It panics if Kind is not
	// FuncDeclaration.
	Func() *FuncDecl

	// ImportSpec returns the Declaration's  *ImportSpec. It panics if Kind
	// is not ImportDeclaration.
	ImportSpec() *ImportSpec

	// Kind returns the Declarations's kind.
	Kind() DeclarationKind

	// Method returns the Declaration's  *MethodDecl. It panics if Kind is
	// not MethodDeclaration.
	Method() *MethodDecl

	// Name returns the declared name.
	Name() string

	// Offset returns the Delcaration's offset within its SourceFile.
	Offset() int // Position in source file.

	// SourceFile returns the Declaration's SourceFile.
	SourceFile() *SourceFile

	// Type returns the Declaration's  *TypeDecl. It panics if Kind is not
	// TypeDeclaration.
	Type() *TypeDecl

	// Var returns the Declaration's  *VarDecl. It panics if Kind is not
	// VarDeclaration.
	Var() *VarDecl
}
